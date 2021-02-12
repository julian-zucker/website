module Essay exposing (Model, essays, getEssayBySlug, view, viewEssayLink, viewEssayPreview)

import Html exposing (..)
import Html.Attributes exposing (..)
import Route



-- TODO The source of every essay has to be in code in this file. I want to store the text in markdown-esque files
-- TODO include date that the essay was written
-- TODO previous/next links?


type alias Model =
    { name : String
    , slug : Slug
    , isShown : IsListed
    , content : List ContentItem
    }


type IsListed
    = Listed
    | NotListed


type alias Slug =
    String


type alias QuoteItem =
    { author : String
    , maybeSource : Maybe String
    , quote : String
    }


type ContentItem
    = Plain String
    | Paragraph (List ParagraphItem)
    | Quote QuoteItem


type ParagraphItem
    = Body String
    | Link { text : String, href : String }
    | Image { href : String }
    | Footnote String
    | NumberedList (List String)
    | NumberedListStartingAt Int (List String)


getEssayBySlug : String -> Maybe Model
getEssayBySlug essaySlug =
    List.head (List.filter (\p -> p.slug == essaySlug) essays)


view : Model -> ( String, List (Html msg) )
view { name, content } =
    ( name
    , [ div [ class "essay-content" ]
            [ div [ class "essay-title" ] [ text name ]
            , div [ class "essay-text" ]
                (let
                    accs =
                        List.foldl viewContentItem ( [], [] ) content
                 in
                 List.append (List.reverse (Tuple.first accs)) (List.reverse (Tuple.second accs))
                )
            ]
      ]
    )


viewContentItem : ContentItem -> ( List (Html msg), List (Html msg) ) -> ( List (Html msg), List (Html msg) )
viewContentItem item ( bodyAcc, footnoteAcc ) =
    case item of
        Plain contents ->
            ( p [] [ text contents ] :: bodyAcc, footnoteAcc )

        Paragraph paragraphItems ->
            let
                fn footnoteItems footnotesSoFar =
                    let
                        num =
                            String.fromInt (List.length footnotesSoFar + 1)
                    in
                    case footnoteItems of
                        Footnote content ->
                            div [ class "footnote" ]
                                [ div [ class "footnote-number" ] [ a [ name ("foot" ++ num), href ("#footnote" ++ num) ] [ text ("[" ++ num ++ "]") ] ]
                                , div [ class "footnote-content" ] [ text content ]
                                ]
                                :: footnotesSoFar

                        _ ->
                            footnotesSoFar

                footnotes =
                    List.foldl
                        fn
                        footnoteAcc
                        paragraphItems
            in
            ( p [] (htmlFromFootnoteItems paragraphItems (List.length footnoteAcc + 1)) :: bodyAcc, footnotes )

        Quote quoteItem ->
            let
                quote =
                    p [ style "width" "80%", style "padding-left" "8%" ]
                        [ i [] [ text quoteItem.quote ]
                        , br [] []
                        , span [ style "padding-left" "15%" ]
                            [ text ("— " ++ quoteItem.author)
                            , i [] [ text (Maybe.withDefault "" (Maybe.map (\s -> ", " ++ s) quoteItem.maybeSource)) ]
                            ]
                        ]
            in
            ( quote :: bodyAcc, footnoteAcc )


htmlFromFootnoteItems : List ParagraphItem -> Int -> List (Html msg)
htmlFromFootnoteItems footNoteItems footnoteNumber =
    let
        htmlFromFootnoteItem item ( soFar, currentFootnoteNumber ) =
            case item of
                Body content ->
                    ( List.append soFar [ text content ], currentFootnoteNumber )

                Footnote _ ->
                    ( List.append soFar [ a [ name ("footnote" ++ String.fromInt currentFootnoteNumber), href ("#foot" ++ String.fromInt currentFootnoteNumber) ] [ text ("[" ++ String.fromInt currentFootnoteNumber ++ "]") ] ], currentFootnoteNumber + 1 )

                NumberedList list ->
                    ( List.append soFar [ ol [] (List.map (\e -> li [] [ text e ]) list) ], currentFootnoteNumber )

                NumberedListStartingAt startingAt list ->
                    ( List.append soFar [ ol [ start startingAt ] (List.map (\e -> li [] [ text e ]) list) ], currentFootnoteNumber )

                Link link ->
                    ( List.append soFar [ a [ href link.href ] [ text link.text ] ], currentFootnoteNumber )

                Image image ->
                    ( List.append soFar [ img [ src image.href ] [] ], currentFootnoteNumber )
    in
    Tuple.first (List.foldl htmlFromFootnoteItem ( [], footnoteNumber ) footNoteItems)


textFromFootnoteItems : List ParagraphItem -> String
textFromFootnoteItems footnoteItems =
    List.foldr (++) "" (List.map textFromFootnoteItem footnoteItems)


textFromFootnoteItem : ParagraphItem -> String
textFromFootnoteItem item =
    case item of
        Body string ->
            string

        Footnote _ ->
            ""

        NumberedList _ ->
            ""

        NumberedListStartingAt _ _ ->
            ""

        Link { text } ->
            text

        Image _ ->
            ""


viewEssayPreview : Model -> Html msg
viewEssayPreview model =
    case model.isShown of
        Listed ->
            div []
                [ p []
                    [ viewEssayLink model
                    , div [ class "essay-preview" ]
                        [ text
                            (String.left 80
                                (String.replace
                                    -- There were spaces left over after stripping footnotes,
                                    -- this fixes the obvious cases.
                                    " ."
                                    "."
                                    (String.replace
                                        " ,"
                                        ","
                                        (List.foldr (\first -> \second -> first ++ " " ++ second) " " (List.map contentItemToString model.content))
                                    )
                                )
                                ++ "…"
                            )
                        ]
                    ]
                ]

        NotListed ->
            text ""


contentItemToString : ContentItem -> String
contentItemToString item =
    case item of
        Plain string ->
            string

        Quote quoteItem ->
            "\"" ++ quoteItem.quote ++ "\""

        Paragraph footnoteItems ->
            textFromFootnoteItems footnoteItems


viewEssayLink : Model -> Html msg
viewEssayLink model =
    span [ class "essay-link" ] [ a [ href (Route.toUrlString (Route.Essay model.slug)) ] [ text model.name ] ]


essays : List Model
essays =
    -- written 2019-06-02
    List.reverse
        [ Model "Deliberate Practice for Software Engineering"
            "deliberate"
            Listed
            [ Plain """Solving HackerRank-style programming challenges will not make you a better software developer. Day-to-day software engineering almost never requires implementation of algorithms, and when it does, it is important that your implementations are maintainable. Practicing how to write one-off algorithms then will not help you practice software engineering. It may give you skills that will be helpful in software engineering – learning the full API of the standard libraries of languages, practicing writing some types of code fast, and general debugging. But it won't help you as much as deliberate practice for software engineering."""
            , Plain """What would a programming challenge intended to be deliberate practice look like? I don't know exactly, and to figure it out we first have to define software engineering, so that we can see what it means to get better at it. Let's say software engineering is writing maintainable, correct code. If someone writes more maintainable code than they used to, we would say they're a better software engineer, and the same for writing code that is closer to correct. HackerRank-style challenges will help with correctness, as your code does have to pass a suite of unit tests. But it doesn't help you with _ensuring_ that your code is closer to correct, because you aren't writing your own unit tests. If you have someone else's test harness to lean on, you aren't practicing the skill of testing, which means a programming challenge where you have to write your own tests would be superior. HackerRank-style challenges also don't reward maintainability, in fact, by only judging competitors on speed to solve the problem, writing code that is maintainable will hurt your performance."""
            , Plain """How can we test programmers on their ability to test? One way of doing this is asking programmers to write unit tests for a problem without writing code, and then seeing if their tests pass with correct implementations and fail on incorrect implementations. This has some issues, most notably that typos in the unit tests would have a huge effect on outcomes, and that the iterative write-test/write-code/debug cycle is the skill we really care about, not writing the tests correctly on the first try. Perhaps it would be better to get developers to write code and tests, or just make the code complex enough that automated unit tests are useful."""
            , Plain """We can test maintainability of programs by having programmers maintain the programs they write for the coding challenge. We can't simulate the experience of opening up a file you haven't touched in a year and having no clue what's happening in it unless we are willing to wait a year. But waiting a year gets rid of the tight feedback loops that you need for deliberate practice. So, we will have people maintain their programs immediately. First, give your programmers a small task, have them implement it, and run their code past your unit tests. Score their solution on correctness on the first challenge, and then have them update the code, adding some features and changing some functionality. Repeat a few more times. Now, you can see how quickly a programmer was able to write and modify their code to meet changing requirements, as well as the functionality along each step of the way."""
            , Plain """If you're using this tool for deliberate practice, you would be able to see which of your decisions lead to maintainable code and which were difficult to update. This would allow you to practice writing maintainable code, something which current programming challenges don't allow. I appreciate that Object-Oriented Design at Northeastern does something similar. For the final two months of the class, you iteratively design a model, view, and controller for an animation that users can dynamically interact with. They manually grade test coverage for each assignment, but without tests, I doubt people would be able to make the third or fourth set of changes without regressions on the first set of requirements. """
            , Plain """Modifying code, not writing code, is most of software engineering, so our classes, challenges, and practice tools should focus on modifying code."""
            ]

        -- written 2019-07-14
        , Model "Improving Your Tooling"
            "tools"
            Listed
            [ Plain """Why did I swap to zsh? I believed some of the features in it would save me time in the future. Or at least, that's what I want to tell myself. In practice, I swapped to zsh and iterm2 because Danny told me to. I'm going to try and estimate how much time it will actually save me, and how much time investment it will require."""
            , Plain """Figuring out the time investment seems easy at first glance, all that I'll have to do is note that it took me two hours to install and configure the new set-up day of. But in the weeks following that, I've noticed little things bugging me about the current configuration, which prompts a thirty-minute or so dive into the docs to try and fix it. This post-install tuning will probably be the majority of the time that I spend on managing this terminal setup. My old terminal setup, which had major problems that are now resolved completely, did not provide room for tweaking, and so did not distract me with promises of a better future. For example, playing around with my prompt to include the git branch probably saves me time in the long run - now I won't accidentally commit to master and have to cherry-pick, etc. But playing around with the color, to make it a fun green instead of a plain black, will not noticeably affect my well-being in the future. Admittedly, I could have been playing around with prompts and so on with my old setup, but taking one step to improve the situation implicitly justified continuing to change it over time."""
            , Plain """There's a general lesson to be learned here. While I still should make the large changes that will give me most of the benefits (pareto's 20%), the smaller changes won't pay off as well. In general, in computer science/software engineering, large changes like using one shell over another, or one programming language over another, take the same amount of time as smaller changes, such as changing a minor misbehavior of your terminal or following a specific style guide in your language. Because the time investment is the same, the investment-to-payoff ratio is larger for larger changes. And so, in the future, I'll try to focus on picking the right tool for the job, and less on the color of that tool."""
            ]

        --    Written 2019-07-28
        , Model "Invariants for Everyday Life"
            "invariants"
            Listed
            [ Plain """I learned about class invariants in my "Object Oriented Design" class. A class invariant is a property of a class that is always true. For example, a class that instantiates a static field at compile-time has the invariant that that field is never null. Classes with final fields have the invariant that the value in the field won't change (barring reflection-based shenanigans). You get the gist – properties of a class that are enforced by the code. There are also loop invariants, a notion from algorithms, which are invariants about the state of a program during a loop. For example, if you are building up a list one element at a time, an invariant might be that the list you are building has length one less than the loop index."""
            , Plain """Invariants make code easier to reason about. In object-oriented, mutation-filled code, it can be hard to keep track of which methods change what fields, what values a given method may return, and so on. There are many moving parts, and invariants let programmers interacting with these systems rule out some of the worlds. Knowing of an invariant reduces the number of possible states of the world, which simplifies the process of ensuring your code is correct. In the same way that enumeration values make explicit the limited scope of a piece of data, while strings allow infinite possibilities, invariant properties shrink the state space."""
            , Plain """In everyday life, you can also ensure that some properties are true. For example, properties about the locations of your keys. Some people lose their keys often, because there are nearly infinite places keys can be - in your fridge, in a potted plant, in a desk drawer, and so on forever. However, by enforcing the simple invariant "my keys are either in my pocket, or in the key bowl", you can cut down the space to only two possible states. Admittedly, this is a harder constraint to enforce than an invariant in code, but the benefits are worth it. And following this is only a matter of habit: when you take your keys out of your pocket, put them in the key bowl, and vice versa. Another invariant that was helpful to me is ensuring that my wallet is always full while it is in your pocket: when you take your credit card out to pay at a restaurant, I take my wallet out of my pocket, and don't put it back in until I have put the cards back in. This has helped me overcome my habit of attempting to leave restaurants after I have started to pay but before I have signed. Similarly, I kept bringing my computer glasses case, while leaving the glasses themselves on my desk. So, I started enforcing this invariant: I only close the glasses case when it has glasses in it. Then, if it is open, I can clearly see that there are no glasses. Now, I never bring an empty glasses case with me."""
            ]

        --  Written 2019-08-04
        , Model "The GRE"
            "gre"
            Listed
            [ Plain """The GRE is a silly way to evaluate PhD candidates. Let's think about the skills required for day-to-day  research, and contrast with the skills tested by the GRE."""
            , Plain """Research requires a lot of reading, a fair bit of applied statistics, and a lot of writing. The reading is obvious, as papers are the primary way for researchers to formally communicate their ideas. If you lack reading comprehension, you won't be able to keep up with the literature, as you won't understand what the experts in your field are saying. So you must be able to extract the core arguments from a text, evaluate arguments, and understand the vocabulary used in these papers. You have to  used to evaluate statistical arguments, particularly in the health and social sciences, where a lot of the research is based on RCTs and statistical experiments to show the efficacy of a drug or the power of some psychological effect. If you don't know how to evaluate study design, what a p-value means, and how to do Bayesian inference, you will fail to understand many of the arguments being made. And writing is important for the same reason as reading - papers are the primary way to package your ideas for general consumption, so you must be clear and accurate in your writing."""
            , Plain """At first glance, the GRE appears to be testing on those things, after all, the three sections are "Reading", "Quantitative", and "Writing". So surely the skills tested by the GRE are those that will be valuable in a PhD? No. The section headers line up with the abstract notions of the skills, but the details are all wrong. """
            , Plain """Starting with the reading section, there is a large portion that is simply vocabulary and multiple-choice-test-taking skills. One section, about 6 of the 25 questions on the practice exams I've taken, are called the "Section Equivalence" Questions. The question asks you to find a pair of answers that mean the same thing when plugged into the sentence, and lead to a coherent sentence. Let's leave aside the discussion of whether their desired vocabulary is relevant. (Did you know that phlegmatic means "sluggish, unemotional or apathetic"? I didn't, because no one says or writes "phlegmatic".) The question has six possible answers. Looking at the answers alone, avoiding the question, you can get a decent score on these questions. Typically, there are only one or two pairs that mean the same thing, allowing you to narrow down the selection of answers from 30 (picking 2 answers from 6 possible choices) to 2. A 50/50 shot is pretty good: On the GRE, getting 50% of the questions right is around 50th percentile [citation needed]. But notice that we haven't even examined the sentence – this question format is almost entirely a vocabulary question. Once you read the sentence, some mental effort is required to understand which of the two pairs would be coherent, but normally the two pairs are diametric opposites, as in "commonplace/everyday" and "opulent/expensive". Yeah, a quick glance at the sentence is going to immediately tell you which of the two pairs fits. The "text completion" section is similarly based entirely on knowledge of vocabulary, if you assume that people who want to get a PhD have some basic understanding of how sentences work. Maybe my undergraduate classes are atypical, but I had reading comprehension drilled into me by my philosophy professors (after a series of low grades and hanging around in office hours until I got it). Finally, I'll give the GRE some credit: the passage comprehension problems I have no problem with, as reading a passage, performing basic exegesis, and understanding the conclusions that the argument makes and how each point ties in to the whole are fundamentally useful skills. They're useful for PhD students, yes, but also for reading and comprehending news articles and political speeches, so I have no problem practicing my ability to understand arguments."""
            , Paragraph
                [ Body """ Moving to the math section, I question the usefulness of a majority of questions. As I said above, I am entirely in favor of statistical reasoning being tested. Researchers need this skill. However, beyond the statistics, which are fairly rudimentary, the majority of the questions are about basic algebra and geometry. Don't get me wrong, these skills are incredibly useful in day-to-day life. Algebra is required to budget effectively, understand which of two items in the grocery store is cheaper per unit  ("""
                , Link { text = "https://xkcd.com/309/", href = "https://xkcd.com/309/" }
                , Body """), and other basic problems of optimization that pop up in your life. Geometry is useful for understanding how much surface area something has before you paint it, intuiting the amount of water in different-sized pots while cooking, and other operations on physical items. But these are not problems that are guaranteed to crop up in someone's research. In evolutionary game theory, algebra (and calculus) are essential to manipulating the mathematical symbols we represent evolutionary processes with. It's a crucial skill – in the field I want to go into. Neuroscientists, moral philosophers, and historians don't need geometry. So why do we evaluate them on this? You could say that at least the philosophers and historians aren't being evaluated on their quantitative scores as much as the verbal portion of the test - but neuroscientists most definitely are judged on the scores on the mathy parts. I don't object to assessing the quantitative skills of applicants, but the test would be more effective if it tested the quantitative skills that will actually be used most often. Studying geometry for the sake of one test, instead of learning some principles of statistics that will help you forever, seems like a waste. """
                ]
            , Plain """The writing section gets at some of the same skills that are used in writing a full paper, but misses the mark in some keys ways. For example, the text editor they give you is crap. No find-and-replace, no spellchecker, nothing. You're given 30 minutes to respond to make an argument for or against a prompt on a random topic, and then 30 minutes to find the holes in an argument. These are both legitimate writing tasks - the argumentative essay is a great way to express your opinions on a subject, and argument analyses demonstrate not only effective writing skills but the ability to understand the flaws in arguments. But giving someone only thirty minutes, while more or less demanding that both essays fit the "intro/three body paragraphs/conclusion" structure, divorces this writing task from academic writing. Papers are written over weeks or months, with multiple rounds of revisions. And they tend to be much longer than the ~750 words I can bang out in a half-hour. This is more about writing to the test, than it is about overall writing ability, although the most egregious lacks will be apparent. For example, a heavy reliance on a spellchecker, a complete misunderstanding of sentence structure, or an inability to make valid arguments will show through in this thirty minute mini-essay. So this task is fine for weeding out terrible writers, who I guess PhD programs assume can't be trained. But do we really need that? If someone is a terrible writer, and particularly if they are bad in the thirty-minute rushed essay responding to a novel prompt with no research, you'll already know. Most of high school involved writing those types of essays, and you know that this applicant got into college. Surely, if their grasp on the English language were so bad as to prevent them from being able to fake it for thirty minutes, they wouldn't be where they are. I understand the purpose of this section for non-native English speakers, but that's what the TOEFL is for."""
            , Plain """I still studied for the test a bit, and had a low-grade fear of doing poorly on it until I took the first practice test. Now, armed with some level of confidence that my score will be good enough, I won't be spending any more of my time on skills that will be useful only in this context. I hope PhD programs use the GRE to reject bad candidates, but don't look at high GRE scores as indicative of anything more than test-taking ability."""
            ]
        , Model "The Outside View"
            "outside"
            Listed
            [ Plain """I believe that watching Netflix, scrolling through Instagram, and swiping on Tinder is overall bad for other people, on average. So why do I do those things sometimes? In general, when I use these apps, I usually believe that my usage is atypical and so not as likely to be harmful as other people's usage of the apps. This is probably irrational. Without strong evidence pointing towards me begin special, it's much more reasonable to assume that the effect of technologies on my welfare is approximately average."""
            , Plain """"Taking the outside view" means looking at the statistics for people in your situation, and applying those statistics to your own position. Instead of asking whether you are being benefited by a particular app, ask whether people on average are benefited. This engages your critical thinking skills, instead of asking in essence whether you have a first-order desire to keep using some app. It may be addictive, and fun, and even enjoyable, but to ask yourself whether it is beneficial to others will reveal whether it is beneficial to you."""
            , Plain """So, if I think that deleting Instagram or Facebook would be good for everyone else, I ought to delete Instagram or Facebook myself. While I was always an atypical user of both, I think, the same reasons that apply to other people apply to me. My typical argument against Instagram is that the constant comparison of aesthetics with other people is bad for your mental health and promotes shallow cravings, as well as being a distraction from what really matters in life. Even when I moved my own Instagram account to a "write-only" mode where I would download the app when I had an image I wanted to post, post the image and delete the app again, I still was influenced by the pressures of knowing other people were seeing the images I posted. When taking photos, wandering around the streets of Boston with my DSLR, I was capturing the photos that I thought others would like. And it wasn't the kind of other-related preference like when you take a photo specifically because you think one person you know will like it, which is almost a form of gift giving. "I took this photo, because I thought you'd like it" is quite different from "I took this photo, because I think it implies positive things about my lifestyle to people who glance at it". Because Instagram makes you care about the judgements of everyone, in aggregate, the niche aesthetic moments that appeal only to one friend aren't the highest value."""
            , Plain """It's worth noting that I did, in fact, delete my Facebook, Instagram, and Snapchat. (I'm still working on Netflix.) This is mostly because the Black Mirror episode "Smithereens" really got to me, not because of the rational deliberations presented here. I think that I benefited from these deletions, because I think that other people would have benefited from them."""
            ]
        , Model "Good Self-Harm"
            "goodharm"
            Listed
            [ Plain """While "Good Self-Harm" seems like an oxymoron, it's not. Look at vigorous exercise, cold showers, or fasting. These three activities all cause pain, and we choose to voluntarily inflict them upon ourselves, so we have the "self" and the "harm". Normally, when people talk about self-harm they mean something like cutting, which is not good for you long term. But these other three, despite being painful, are good for you, and in fact one of the three is even actively encouraged by society."""
            , Plain """I think that intentionally doing things that you know you won't enjoy in the moment is good for you in the time beyond that moment. I am more productive and maybe even happier on days I take cold showers, and other people seem to report similar effects. Getting into the shower, I still feel my muscles clench in anti-anticipation, knowing that the cold will hurt. But I step in anyways, and after a minute or so, I don't feel the cold anymore. More importantly, it gets me in the habit of forcing myself to do things that I don't want to do in the moment. The same brain muscles, I think, get used dragging yourself to the gym, into a cold shower after the gym, and off Netflix to do something you actually want to do."""
            , Plain """So why is only exercise encouraged by society, of the three above? Fasting is seen as weird, especially the three-day-long fasts where I can work tirelessly, lift my old PRs effortlessly, and develop a deeper understanding of when I'm actually hungry and when I'm just bored. People don't like fasting, and I can totally understand why –\u{00A0}hunger pangs are painful, and missing out on some social opportunities because I can't eat sucks. But going to the gym also sucks, causes pain, and takes up time that could be used elsewhere. There's no important difference I can see here. There are health benefits to lifting, but there are health from fasting as well (autophagy). There are aesthetic/fitness benefits to going to the gym, but similarly to fasting. Fasting excessively can be dangerous, and a sign of a mental disorder, but so can excessive gym-going. And cold showers take very little time –\u{00A0}almost by design, as one of your first goals after you step into a cold shower is to get the hell out. Not only will you save maybe ten minutes by making your showers faster, but cold showers make you more productive and focused during the day. So, if you're going to the gym, I think you should consider the other two interventions, based on the same reasoning."""
            , Plain """Harming yourself with the aim of harming yourself is bad, as your end goal is to damage yourself, which is not good for your long-term prospects. "Harming yourself", like you do at the gym or in a cold shower, has the end goal is improving yourself, so it can be justified more. """
            , Quote (QuoteItem "William James" (Just "Principles of Psychology") """Keep the faculty of effort alive in you by a little gratuitous exercise every day. That is, be systematically ascetic or heroic in little unnecessary points, do every day or two something for no other reason than that you would rather not do it, so that when the hour of dire need draws nigh, it may find you not unnerved.""")
            ]
        , Model "Two Notebooks"
            "notebooks"
            Listed
            [ Plain """It's weird enough in 2019 to have one notebook, so actively using two earns me some sideways glances. First, the backstory of my attempts to be productive and keep a list of the things I need to do. """
            , Plain """I started bullet-journaling on January 1, 2019, but not because of a New Year's resolution. I had heard the news that Google Inbox was getting shut down in March, and wanted to have a smooth transition from my Google-Inbox-based life organization scheme to a different one. I had heard about bullet journaling from Cal Newport, and I thought I'd give it a try."""
            , Plain """First, let me explain my old bookkeeping system. People would send me emails sometimes, and when they did, I would either archive it immediately after reading (if it didn't require action), or leave it read in my inbox (if and only if the email represented a task). If someone gave me a task in a non-email format, such as professors assigning homework in class, I would put that task as a "reminder" in Google Inbox, and it would show up inline, interwoven with the emails. So, the end result was a list of all my tasks, in reverse chronological order by assignation date. The snooze feature allowed me to keep my inbox free of tasks due far in the future, while not forgetting about them entirely. All was well and good."""
            , Plain """Without Google Inbox, though, you can't see your emails and reminders in one place. (Currently, there is only one place in google that you can snooze your reminders - even though there are about six places where you can add and remove and complete them.) So, I needed a new system. I experimented with other within-Google solutions, like Keep and Reminders and even the calendar. But nothing satisfied my aim of having all my tasks in one place."""
            , Plain """This lead me to bullet journaling. I could write down the immediate tasks for a day each day, and I used Google reminders still for longer-term aims. It seemed reasonable. Eventually, I realized that centralizing the task list was good, and that writing down your goals in a book helps you form the habit of checking that book and doing tasks out of it. So, I stopped, made a calendar assigning one page per week until the end of the year, and resumed the daily sections afterwards. Now, I can plan more than one day in advance, and I'm able to schedule tasks for far in the future. For example, whenever I swap out toothbrushes, I write a note to swap toothbrushes, assigned for three months in the future."""
            , Plain """Bullet Journaling™ wants you to use one notebook for everything – you write down your daily task list, then you doodle a bit, take some notes on math you're thinking about, jot down an idea for an essay, and then write down tomorrow's daily task list five pages of nonsense later. I couldn't quite deal with this, as I started getting into research, as having to flip through dozens of pages of arithmetic to find what I needed to do today and what I failed to do yesterday was difficult. Hence, the second notebook. This notebook allows me to have my math from yesterday next to my math from today, and frees up the other notebook to have one day's tasks next to the other. Even though there are more physical items involved, increasing the physical clutter on my counter, the decrease in intellectual and organizational clutter makes it worth it. """
            ]
        , Model "Achieving Second-Order Desires"
            "desire"
            Listed
            [ Plain """Actualizing upon your second-order desires is easier than most people believe, and has a large positive impact on your life. To clarify, second-order desires are the desires you have that are about the desires you have, for example, a desire to desire candy less and vegetables more, or a desire to desire learning more. These desires are about something entirely internal, your desires, and so should be easier to achieve than desires that require changing the external world. Unfortunately, people don't have that level of control over their brains."""
            , Plain """Or, perhaps, we do have a lot of control over the future state of our minds, but we don't know how to exert that control. This seems relatively implausible, because the default assumption in most matters of the mind is that each person knows their mind entirely. Looking at examples like walking, however, paints a different picture. Walking is widely known [citation needed] to increase subjective wellbeing and causes happier moods, but most people still walk very little. In fact, when I am in a bad mood, I know consciously that going for a walk will make me feel better, but it still requires mental fortitude to get my shoes on and get out the door. Other things which also would improve my mood, like seeing a friend or drinking good coffee, I don't resist in the slightest. I enjoy going for walks, and they are long-term good for me, and yet I still fail to take as many walks as would be subjective-wellbeing-maximizing. I have a second-order desire to desire to go for walks, but a first-order desire to avoid walks. Actualizing the second-order desire, changing my opinion of walking to be immediately and viscerally positive, would be good for me. But conventional wisdom says that you can't just up and decide to change your mind."""
            , Plain """I think that the conventional wisdom is mistaken. Positive and negative reinforcement work on humans, just as well as they do on other animals. Go for a walk, then eat a square of chocolate. Surely, your disposition towards walking will be better than if you bit a lemon immediately afterwards. If you keep up the habit of having chocolate after you go for a walk, your brain will start associating the walk with the pleasant feeling, and keep valuing the walk even once you discontinue the chocolate routine. Another possible way to change your desires is through cognitive behavioral therapy. If cognitive behavioral therapy can help some people with severe eating disorders, depression, or OCD change their thought patterns, it is clear that it has the power to change negative thought and action patterns to neutral ones. If it can change negative to neutral, why not neutral to positive? For the first three weeks I went to the gym, I told myself that I enjoyed the experience, even as I woke up at 6AM, sore, exhausted, and not seeing any improvement. And after those three weeks, I started enjoying it. Now, correlation is not causation, and it's possible that I would have enjoyed getting back into working out regardless, but it can't hurt."""
            , Plain """The value of being able to change your habits at will (or with a little bit of work) is very high. So I believe it's definitely worth the time taken to at least explore whether you have power over your habits. Low cost, high reward, and trying really hard to change your habits for a few weeks isn't going to hurt anything. """
            ]
        , Model "Endnotes and Footnotes"
            "notes"
            Listed
            [ Plain """In non-fiction, don't use endnotes, because flipping to the end of a book to get slightly more context is annoying, and footnotes are just convenient. In fiction, though, endnotes and footnotes can be used stylistically, because they feel very different to the reader. A footnote is playful, like a parenthetical, requiring just a glance down at the bottom of a page to see what the author is talking about. An endnote is daunting, a quest laid out in front of you, requiring flipping to the very end of the book to investigate something."""
            , Plain """The archetypal use of footnotes is found in Terry Pratchett's writing. Pratchett uses footnotes for witty side commentary, and there is a real reason that the commentary is in a footnote instead of in the text itself – comedic timing. For instance, in "Small Gods", Pratchett writes "Kitchens and storeroom and craftsmen's workshops belonging to the Church's civilian population honey-combed the citadel. (1)", footnoted to "It takes forty men with their feet on the ground to keep one man with his head in the air". Incredible! The footnote sets you up with context that has been set by the "footnote culture" of non-fiction, where footnotes provide additional detail that some readers might wish to skip. Going in to that footnote, I thought I was going to get a description of the kitchens and such, but instead I was hit with an aphorism. """
            , Plain """The quintessential deployment of endnotes is in Infinite Jest. Eleven hundred pages, and one of those hundred is entirely endnote. These endnotes aren't just clarifications and citations and the drudgery that I have come to expect from non-fiction's endnotes, though, they give the story character. It's a book you must read with two bookmarks, one for the main text and one for the endnotes, because there is an endnote on average once per three pages, and flipping over a thousand pages is no fun. I will try to characterize the content of these endnotes, although they try their hardest to elude description. Some of them are definitions, as early in the book when drugs are being introduced, a chemical specification and a brief overview of the effects of the drug. But as the book wears on, they gradually fade into stories, completely separate side plots from the main book, ten-page excursions into a piece of backstory that might help you understand a moment that just happened in the main text. But these bits of story tucked away in the back of the book are precisely those stories that fit there, hidden away in the endnotes – stories of embarassments of characters, of intrigue and spies and so on. The endnotes, too, begin to feel like a mockery, David Foster Wallace pointing out without pointing out that you are willing to flip pages just for him, just to consume some media."""
            , Plain """Footnotes and endnotes are both ways to extend a book for some readers but allow readers to opt-out without the guilt that comes from skipping main text. But, particularly in fiction, they add different textures to the story you are telling. """
            ]
        , Model "Quantum Outfits"
            "quantum-outfits"
            Listed
            [ Plain """There is an interesting connection between wave function collapse and picking an outfit to wear. The wave function collapse algorithm, which is a classical algorithm based on quantum mechanics, works roughly as follows: starting with a probability distribution over some set of choices, pick one thing at random and update the probability of choosing the rest based on their conditional probability, conditional on the choice you made. For example, this algorithm is often used to generate tilemaps for video games: pick one tile, then choose the rest based on the probability that they are adjacent. This allows patterns like roads to be generated, if the probability of a "straight road" tile coming after another "straight road" tile is sufficiently high, you end up with long uninterrupted roads. """
            , Plain """ You should be able to see how this maps to choosing an outfit. Typically, the way I choose outfits follows a very similar algorithm. First, I pick one piece of clothing that I will wear. Let's be honest: I always pick black jeans. Then, I pick other clothing items, conditional on this first one: perhaps black boots, which will go well with the black jeans. Then, the wave function collapses further: I decide to wear a white band shirt. Finally, I need a coat, and I pick that coat conditional on the clothing that I know I will be wearing underneath it."""
            , Plain """There is no instrumental value to knowing about the connection between wave function collapse and outfit choice, as far as I can see. But I still think it's neat. Perhaps it can help you understand why you never wear some parts of your wardrobe. If you don't pick a piece of clothing first when dressing, you run the risk of having the wave function collapse somewhere else. In my case, shirts that go with blue jeans, but not black, never end up being worn, because my wave function starts its collapse around black jeans. """
            ]
        , Model "A Nebraska Double: Shot 1"
            "nebraska"
            Listed
            [ Paragraph
                [ Body """ On Halloween, one of my ex-roommates (call him Roommate), one of my exes (call her Ex), and a guy I met at my college orientation (call him Orientation) and I went to Lincoln, Nebraska. You need to have a very legitimate reason to leave Seattle, Boston, Boston, and Denver (respectively) to go to Nebraska, but we had one: we were going to present posters at a philosophy conference. Our poster was titled "Technology, Social Choice, and Democracy: The Cute Dog Project","""
                , Footnote "I put commas outside of quotes, and you're just gonna have to deal with it."
                , Body """ and it was pretty much what it sounded like: we ran a vote to determine who in Northeastern's philosophy department had the cutest dog. I thought this whole situation was pretty absurd. """
                ]
            , Plain """ My train leaves Denver at 7:10, so I leave my office's Halloween party at 6:50. I stumble up to the Amtrak at 7:08, and when I say "stumble", I really mean stumble. I make it onboard and collapse into a seat. I try unsuccessfully to fall asleep. The man next to me was having a hushed but still quite loud phone call, where he describes in detail how his girlfriend has been stealing his drugs. Looking at him, I'm not quite sure how he has a girlfriend, and I imagine that he could benefit from having fewer drugs around. So I'm sympathetic to his girlfriend's choices, if she really exists and really is stealing from him. A few rows ahead, a baby wails. I put on soundproof headphones, and get ready to read. I take my shoes off, trying to get comfortable, and the second I set my feet on the ground, I feel cold and wet. Someone has left some kind of liquid on the ground beneath the seat I'm in. I take a deep breath, convince myself that it's just water, and move one seat up. At this point, I'm pretty sure that the weekend is going to be mediocre at best. """
            , Paragraph
                [ Body "My Lyft driver "
                , Footnote "I feel an ethical imperative to choose Lyft over Uber, because of various bad things that Uber does. Strangely, this is the only company I feel negatively about – my assembled-by-children-in-sweatshops iPhone doesn't make me feel bad in the slightest, and I don't particularly care about the fair trade label on food and coffee. I think that having the two apps on my phone, right next to each other, makes it very obvious that I have a choice between something bad and something worse, whereas in most decisions I make, there's no one pointing to the ethical alternative next door."
                , Body """ didn't change my mind. He picked me up, and started up a conversation. I did not enjoy this. Leaving aside the fact that it was 4 AM and I had possibly stepped in someone's pee, the only thing he wanted to talk about was how boring a town Lincoln was. It's not that he was saying things he thought were interesting, but I found uninteresting (like "Lincoln has more than four bars!"): he actually told me that he liked Lincoln because nothing much happened at all. Shit. I like when things happen, and I was going to be stuck here for forty-eight hours. I wasn't too worried, after all, with my collegiate colleagues around, something interesting was guaranteed to happen. """
                ]
            , Plain """ I get to the AirBnB. A small, stout, brick house, sturdy and imposing but not particularly pretty. I open the front door, and I immediately begin to worry for my mental health. The entire house is "The Office" themed. The pillows on the couch all have Michael Scott's face. The posters on the wall are all quotes from The Office. (Later, I would discover to my horror that the shower curtain had a large watercolor painting of Prison Mike printed on it. This made pooping difficult, as whenever you sat on the toilet, you had to stare deep into the eyes of Michael Scott.) The AirBnB was not listed as a themed apartment, but they changed it between my reservation and us showing up. Also, the only bedroom left was the bedroom that has French doors: unfrosted glass windows let people in the living room see into my bed. Perfect."""
            , Plain """ The next morning, running on four hours of sleep, everything seems unreal. The only food in the house is a can of Pringles, a box of Cheerios, and milk. I eat cheerios, shower (hidden behind Michael Scott's face on the shower curtain), get dressed, throw my backpack on. We head to UNL's campus. On the walk, I find far too many things funny, as you are wont to do when you haven't slept. I laugh, and laugh, and laugh, at each one, until Ex tells me that I seem manic and slightly disturbed. I rein it in. We make it to the conference without further incident."""
            , Plain """ I say this about many universities, but UNL is just a knockoff Northeastern. As we walk in, I see college students wearing red shirts with a large N as a logo. The only real difference from a Northeastern sweatshirt is the text above, but "Nebraska" looks a lot like "Northeastern" at first glance. But this is just one event, so we laugh it off and keep walking. We remember that UNL's mascot is the "Husker", a frankly terrifying cartoon man who husks corn. Northeastern's mascot is a husky. There are two parallels, this is starting to seem like a pattern. """
            , Plain """ We do philosophy for most of the day, and then Ex and I break off to work on a paper that was due a few days after the conference. We sit down on a couch that a nearby whiteboard informs us is named "Philosophy Couch", and get to work. Or try to. The second we sit down, the lobby burst into motion. We work for five minutes, then look up, and realize we are surrounded by whiteboards. We laugh, but the sort of worried laugh you do when you are unsure whether something that seems harmless is secretly going to cause your death in the next few minutes. We write another paragraph, and look back up: the whiteboards have been scattered back across the room, and people are hanging posters on them. A woman is waving a hair drying against a large, white sack. We discuss a third paragraph, decide it wouldn't add much to the paper, and check back in on the whiteboard situation. There are no whiteboards in the lobby. As we laugh another worried laugh, a man comes up to us, and asks if his amp and guitar are in an aesthetic place in the room. He has them set up in front of a massive screen, perhaps twenty feet wide, currently displaying UNL's homepage, which features UNL's cheerleaders wearing what must technically be called clothing. The monitor is displaying a lot of exposed skin. Given the square footage of exposed skin on the screen, I am almost certain it counts as pornography. I point out this fact, suggesting that it is not very aesthetic. The absurdity of the situation sinks in. I am sitting, slouched, on "Philosophy Couch" on UNL's campus, and strange men want me opinion about the aesthetic of their music equipment. I laugh, not at him, but certainly in his direction, until Ex tells me that I'm laughing manically again. """
            , Plain """ In the center of a lobby, a group of college students wearing all black are setting up a wide variety of percussion instruments. Gradually, as they get set up, an ethereal tune starts to haunt us. This is not music that makes you comfortable, this is music that tells you a ghost is about to kill someone in the horror movie you're watching. We look up again. The woman is still massaging the air balloon with the blowdryer. We decide we're not going to get any more work done, and try to head upstairs, to the room where we dropped our stuff. Along the way, we see a piece of art that can only be described as two robots breaking up. We get ready to head out to a restaurant, looking for a vegan restaurant. There are more vegan restaurants in Lincoln than in Denver, which seems backwards. When your primary export is beef, you'd imagine that restaurants would end up serving a lot of it. When your primary export is tech workers who smoke a lot, it's easier to understand the market forces that allow vegan restaurants to exist. """
            , Paragraph
                [ Body """ The food is good. Vegan restaurants are always really good """
                , Footnote "Restaurants that serve meat can afford to be bad, because meat done poorly is still okay and many meat-eaters just want a meal, they're not looking for something exceptional. But vegan restaurants, because they're serving such cheap ingredients, need to stand out in other ways to convince you to spend money. Sautéed brocolli isn't going to make me fork over $10. Something interesting, with many ingredients, and good spices, and "
                , Body """. After dinner, we go to another restaurant. This restaurant also serves drinks, but we can't go to a real bar as Ex is still 20 years old.  """
                , Footnote "Lame."
                , Body """. I am shocked at how cheap alcohol is here. Some of the beers on this menu (and they're nice, craft beers, with fun backstories) are five bucks. At nine, Roommate informs us that he's leaving because he has some work to do. We're undergrads, and so we follow him: we're used to the idea of leaving when the people your age start to leave. We are real adults, and we could have hung out with the philosophers, but our instincts lead us outside."""
                ]
            , Paragraph
                [ Body """ As we walk out, Orientation asks "are we done drinking for the night?", and I immediately tell him that we have at least four more hours of drinking in the night"""
                , Footnote "We had seven more hours, in fact."
                , Body """. Our plan of leaving to do work is postponed, and we find a liquor store. The nearest liquor store is a half mile away"""
                , Footnote "Fucking Nebraska."
                , Body """but we walk it. Once we've made it there, only half-frozen, we split up into underage person and someone to hang out (Ex and Roommate), and Orientation and I go inside. Alcohol in liquor stores is strangely similar in price to Boston and Denver liquor stores, for a place where restaurant alcohol prices mean that poor people can afford to be alcoholics too. When waiting in line, we see a wee lad, clearly not more than 19, holding a bottle of Smirnoff Raspberry. He asks the cashier, "Do you have this in... big?". They do. We laugh, buy our box of wine """
                , Footnote "Rosé, obviously."
                , Body ", and we back to our AirBnB."
                ]
            , Paragraph
                [ Body "To make a long story short, we drink, play The Office Trivia Game™, and drink some more. We read passages at random from Peter Welch's book \"Observations of a Straight White Male with No Interesting Fetishes\", and drink every time he says \"sex\". We drink pretty often, and pretty thoroughly. Ex goes to bed at 12:30 "
                , Footnote "Which normally is early enough that I'll make fun of the person going to bed, but in this case, we had been drinking since 6, so I thought it was reasonable."
                , Body ", and Roommate and Orientation and I head out to a bar. We are trying to figure out which bar to go to, and discover another strange parallel between Northeastern and UNL: Boston has a bar called Tavern in the Square, Lincoln has a bar called Tavern on the Square. Obviously, this is a sign, and so we head out."
                ]
            , Plain "We make it to Tavern on the Square just before 1. I order a Ketel One double with ginger beer, not realizing that this is equivalent to asking for a mule until the drinks come in copper mugs. I know what a double mule tastes like, and this was not that. A Nebraska double includes far more alcohol than a Boston double. We drink, and talk, and drink, and I play some darts with strangers, and so on, and then we head outside. They have a Connect Four board that's about four feet tall, and we square up to play. It's important to know that when Roommate and I lived together, we kept a Connect Four board on our dinner table, and when we had fifteen minutes of downtime we would talk while we played Connect Four. In other words, Connect Four ability was very important to both of us, and we always had a friendly competition going."
            , Plain "That night, Roommate kicked my ass at Connect Four."
            , Plain """A sidebar, to tell a story that happened on the side of this bar. I challenged a girl to Connect Four, beat her (thus regaining some of my Connect-Four-based self esteem), and then we started talking. At one point, she asked if I lived in Lincoln, so I told her I was just here for the weekend. She ponders this for a moment, and then says "Okay, that means we can make out but not have sex". Then, she made out with me for about twenty seconds, and walked away, never to be seen again."""
            , Plain """ Last call rolls around. I was getting bored of yelling about philosophy with the high-school dropouts at the bar, and last call is as good an excuse as any to leave a bar. We head home. But the night is not even close to over yet."""
            , Paragraph
                [ Body """ Perhaps five minutes into our walk, in the bleak mid-fall, a pedicab driver """
                , Footnote """Actually, a "Pedicab Chauffeur", according to his business card."""
                , Body """ pulls up. It's the weekend of Halloween, so it's not completely absurd that he's wearing a cow onesie with articulated udders. It's still slightly absurd, but not completely. We chat for hours. Or I should say, Orientation and Roommate chat with him for hours. I meet some strangers, including an Asian man who pretends he doesn't speak English, until he realizes his friends and I are going to be yelling about philosophy for a while, and then caves and joins the conversation. The first English he spoke to me was "Bullshit! You know that's impossible". I was taken aback. I thought, just for a second, that he had learned English in that moment, just to tell me I was wrong. I was impressed at his dedication, and then I realized he had been messing with me. After an hour of conversation, on a street corner, with a pedicab driver and three random folks (they claimed that one of them had just gotten married, but they hadn't given me much reason to trust them, and they were carrying take-out food, so I choose not to believe it), we head home. """
                ]
            , Paragraph
                [ Body "We make it home, argue about the trolley problem"
                , Footnote "To Roommate, if you're reading this: you are deeply, hopelessly wrong about the trolley problem."
                , Body ". And that concludes the first day. But I still had twenty hours left in Lincoln."
                ]
            ]
        , Model "A Nebraska Double: Shot 2"
            "nebraska2"
            Listed
            [ Paragraph
                [ Body "I wake up deeply hungover. Through the French doors of my bedroom, I see the living room in disarray. Sitting on the coffee table, taunting me, is the previous night's box of wine. I get up, put it in the fridge, and start to consider the day. I make coffee"
                , Footnote "This is Nebraska, so the AirBnB only has a Mr. Coffee, although surprisingly the coffee itself is from Peet's, which I consider to be quite good. I curse myself for forgetting to bring my AeroPress. I spend a few minutes contemplating my decision to bring only one backpack on this trip. Was it more motivated by my desire to prove to other people that I can travel light or the actual benefits of traveling light? I'm just about ready to indict my desire to travel light as strictly social signaling, and then the coffee is ready, and I stop thinking about it."
                , Body " and take a caffeine pill"
                , Footnote "At this point, I had slept a total of eight hours over the previous two nights. "
                , Body ". I text everyone else, asking if they're awake. Ex stumbles downstairs, and it looks like someone has punched her in the face –\u{00A0}her lip is swollen and split. I ask what happened and learn that she has no idea either, but she woke up upside down in her bed, with her feet on her pillow, and blood everywhere. We try to piece together what could have happened last night after she went to bed, and idly speculate until Roommate shows up. He says that after we got home, he went upstairs and walked past her room, found her lying on her bed, light still on, and importantly with no blood to be seen anywhere. Seeing as in the morning, it looked like at least two-thirds of a murder happened in her room, it seems unlikely that he missed it. So we knew it happened after we got back from the bar (and the associated pedicab-driver-chatting and stranger-harassing), but we didn't know much else. We speculate a bit more, and then realize we're going to be late to Day 2 of the conference, so we head out."
                ]
            , Plain "On that day's walk in, we end up walking behind three UNL students most of the way in. I notice that they are completely ordinary people, very mainstream-looking. I am completely certain that they were not involved in the interpretive dance shenanigans from yesterday, and in fact, that if we suggested that interpretive dance happened on this campus, that they would not believe us."
            , Paragraph
                [ Body "We show up halfway through the only talk I was actually interested in seeing at the conference, which is unfortunate. I stay engaged through the second half of the talk, and quite enjoy it. The next talk, not so much. I take very aggressive notes, calling out the motte-and-bailey "
                , Footnote "Google it."
                , Body """ argument, with the motte being "If AI is better than us, it will be different than us", and the bailey not even being a coherent enough thesis to recount here. I complain and complain and complain about this talk, in my notebook, doodle a fair bit, and then the talk concludes and I clap anyways """
                , Footnote "When there are only twenty people in the audience, not clapping is conspicuous."
                , Body ". Then, the speaker starts heading back towards her seat. Prior to the walk, she was sitting next to me, so obviously she will be sitting next to me after the talk. I don't realize this, however, and keep sitting and pondering, notebook open to two full pages of scathing attacks on her talk. As the next talk starts, I look down and realize how clear my handwriting is, as I decry that one of her points \"Does not follow!\" and another seems \"Very motte and bailey :(\". I glance over at her, and see that she is looking directly at my notebook. Oh well. I flip to the next page, and start taking notes on the next talk."
                ]
            , Paragraph
                [ Body "Somewhere in there, Roommate left "
                , Footnote "Didn't even say bye."
                , Body "."
                ]
            , Paragraph
                [ Body "Lunch is served, sandwiches "
                , Footnote "On pretzel bread. Pretzel bread is deeply underrated in general, so I wanted to take this moment to appreciate it publicly."
                , Body " and chips and all the sorts of things you expect to be part of a free lunch "
                , Footnote "Not quite a free lunch, really. It cost a whole trip to Nebraska."
                , Body ". One of the professional philosophers there was vegan, and someone else brought it up to point out the relative lack of vegan options. Normally, when people say they're vegan, I don't particularly care. I'm sure that, if we talked for a few minutes, their argument for veganism would at least be inconsistent with some of their other views "
                , Footnote "Note that this does not mean their argument for veganism is wrong, just that I can feel morally superior to them."
                , Body ". But with philosophers, I'm almost certain that any discussion about veganism would go the opposite way, with them pointing out the inconsistencies in my beliefs "
                , Footnote "For instance, I believe that I am not an alcoholic, despite strong evidence to the contrary."
                , Body ". So I stay out of that debate, and eat my meat-laden sandwich in silent shame. Ex (who is vegan now, I think) asks if I want to eat the meat off of a sandwich, so she can eat the bread "
                , Footnote "As previously mentioned, pretzel bread is the shit, so it is not surprising that she would want to eat the slightly soggy sandwich bread over chips or trail mix or whatever else they had that was vegan."
                , Body ". I say yes, and have just the meat and cheese of the sandwich. While holding just meat and cheese, I make eye contact with the vegan philosopher, and immediately turn away to hide my meat-and-cheese habits "
                , Footnote "Despite my shame in this instance, I generally support eating just the inside of a sandwich. Bread is the worst part of any given sandwich. It's just there because it's cheaper than the other ingredients, and makes it socially acceptable to eat the sandwich with your hands."
                , Body "."
                ]
            , Plain "Lunch is over. We sit back down, sit through a talk, and then it's what the conference schedule describes as \"Social Hours (continue conversations, check out posters, etc)\". We head home, to drop off our backpacks and posters before dinner. As we walk in, Ex jokes, \"Well, time to start drinking again?\" Orientation and I do not realize this is humor, and pour ourselves some more wine."
            , Paragraph
                [ Body "We show up to the dinner buzzed and ready to talk about philosophy. We sit down, order our food "
                , Footnote "I get the scallops, for two reasons. First, Northeastern was paying for this meal, so I felt someone obliged to get the most expensive thing on the menu. Second, there were vegans afoot, and scallops are some of the least meat-like meats."
                , Body ", and wait interminably for the food to arrive. Ex has to leave about an hour after we show up to the restaurant, so she gets her food to go. We order drinks "
                , Footnote "If you're ever in Lincoln (unlikely) and at the Blue Orchid (more likely, once you're in Lincoln) and have someone else paying for your meal (unlikely, despite you being in Lincoln), I would highly recommend ordering three Ginger Flowers. And one for your underage (hopefully, under only the drinking age, and not any other relevant ages) ex. "
                , Body " and talk about philosophy. Someone else is severely drunk, which is always a relief, because it means you're not the drunkest person at the party. They have paper tablecloths, which some people find tacky but I love. We talk about Newcomb's paradox "
                , Footnote "I'm a one-boxer."
                , Body ", and as we talk, we can scrawl decision trees on the paper on the table "
                , Footnote "This both lets us communicate more clearly, and also signal to people outside of our conversation that we're having more fun than they are."
                , Body "."
                ]
            , Paragraph
                [ Body "We finish up dinner, and head out to a bar, or brewery, or taproom, or something. I continue to be shocked by the price of alcohol here. I order a marshmallow stout "
                , Footnote "It tastes exactly like how it sounds."
                , Body ", which is 13% by volume, and it costs $7. Orientation and I sit with one of the younger philosophers, talking about the philosophy job market, how to teach, and various other academic-oriented ideas. Orientation and I leave for our AirBnB at 10:50 or so, an hour and a half before my train is scheduled to leave "
                , Footnote "And you should be able to infer, from the fact that I said \"scheduled\", that it was delayed. "
                , Body ". We walk by the corner where we met the pedicab driver last night, laugh, and make it home with no event."
                ]
            , Paragraph
                [ Body "As we walk in, my phone buzzes "
                , Footnote "Not really. My phone is always on do not disturb. I check my phone, and see the message. No buzzing occurred."
                , Body ", and I see that my train has been delayed "
                , Footnote "Well, sort of. The message in the text explains that the train is expected to get in later, but it might be on time, and it may be delayed again. So really it's just warning me that life is uncertain, and not making any definite statements about the train."
                , Body ". Of course, at this point, we're back in the same apartment as a box of wine, so obviously we resume drinking "
                , Footnote "There are seven bottles of wine in a box, after all, and we had only drank about five of them."
                , Footnote "And, the more I drank, the better I would sleep on the train."
                , Footnote "Better here means not sleep quality but speed at which I would fall asleep, I wanted my train ride back to be completely uninteresting, and being asleep for all of it seemed like a feasible way to accomplish that."
                , Body ". Orientation and I talk about philosophy a bit more, and commit to writing a paper together"
                , Footnote "Perhaps the most surprising part of this trip was we re-committed to writing this paper while sober."
                , Body ". Then, I head out for the train, because the text that Amtrak sent warning about the delay did not inspire hope that they knew when the train would arrive."
                ]
            , Paragraph
                [ Body "I arrive at the train station. Our front, there are three massive parking garages, with different colored lights illuminating each one. I appreciate the architecture for a moment. Then, I realize that spending just two days in Nebraska was enough to compromise my aesthetic sensibilities, and I was enjoying what were objectively quite boring-looking parking garages. I head inside the train station, a small rectangle with twenty or so seats on the benches inside. I check my phone as I walk in. The train is delayed again, and I have an hour and a half "
                , Footnote "Of course, the text mentions that it may arrive earlier, or later, or at the exact time that they forecast."
                , Body " to kill. I want to go to a bar, but I don't want to miss my train, and these are conflicting desires. I check a map "
                , Footnote "Okay, fine, Google Maps. But checking a map sounds much more impressive."
                , Body " and see that there is a bar just three minute's walk away. The train, when it shows up, will certainly take more than three minutes to board "
                , Footnote "Or at least, that's what I convinced myself."
                , Body " and so, I could definitely get someone in the station to call me when the train arrives, and then make it back in time."
                ]
            , Paragraph
                [ Body "I ask around. I approach the people closest to me, a couple with a baby, and regale them with my story: I tell them I want to go to a bar to wait for the train, and that if they could call me when it arrives, I would be able to make it back in time. The husband "
                , Footnote "I don't know Nebraska's rate of extramarital pregnancy, but I'm hoping it's low, so I'm going to assume the couple was married. "
                , Body " says \"I'm sorry, but I have a baby\", as if fatherhood was a legitimate excuse to not enable a stranger's alcoholism. The next person down the row, a woman with a large camo-patterned duffel bag that looks vaguely military, says she doesn't have a phone. She is actively using her phone. Whatever. The next person in the row is an old man "
                , Footnote "Although keep in mind, I am 21, so when I say \"old man\", I mean \"over 40\"."
                , Body ", who tells me he doesn't approve of drinking."
                ]
            , Paragraph
                [ Body "Well, I don't need their help anyway. I head out, and three minutes later, I've made it to a place that the people of Nebraska call a bar. It is ramshackle. I ordered a greyhound, and the bartender pulled out a bottle of Svedka, and at that moment, I immediately started doubting my decision. This bears repeating: their well vodka was Svedka. I sit at the bar, writing in my notebook, realizing that this is the first time I've ever been in a bar alone. I don't mind it at all "
                , Footnote "Which is probably a problem."
                , Body ", in fact, I quite enjoy exchanging glances with the girl sitting on the other side of the bar, in between moments spent writing. I'll skip over my whole conversation with her, which revealed that she was Nebraskan and existentially unhappy about it, and skip to the good part: I made it back to the train station right as the train was pulling up. Everyone else in the station looked at me with disgust. Clearly, they had been hoping that I would miss the train because I was at the bar, thus getting my comeuppance, but I ruined that for them. I board, and fall asleep rather immediately."
                ]
            , Plain "I wake up six hours later. I wake up looking out the window, and the rolling hills of northeastern Colorado wave up and down, white with snow on one side, and the light brown of late-fall grasses on the other. The sun is rising, the clouds are a brilliant, piercing orange, and the whole sky is choosing to take part in the rainbow that normally is relegated to the areas near the horizon. I stand up, dazed, and stare out of the back window of the train, watching it tear over the tracks as the sun slowly meanders its way up, the clouds lose their brilliance, and the rainbow fades. I sit back down, deeply content, and thoroughly sleep-deprived. An hour later, as we pull into Union Station, and I laugh and laugh and laugh, silently, to myself, at what a weekend this has been."
            ]
        , Model "High School Sucks"
            "high-school-sucks"
            Listed
            [ Paragraph
                [ Body "As a software engineer, I am pampered "
                , Footnote "Breakfast is served in my office everyday. Lunch is usually free, whether catered or delivered. I can leave early if I want, and no one cares if I'm late. And, most importantly, the office has a ping pong table."
                , Body " because the people in charge want to maximize my output and keep me around. High schoolers, on the other hand, are not exactly pampered."
                ]
            , Paragraph
                [ Body "I think there's something contradictory about those two facts. Surely, we behave as though we want high schoolers to be successful, to learn a lot, and to be reasonably happy "
                , Footnote "People's definitions of reasonably differ a lot. For example, Gunn High School, near where I grew up, only started thinking about it's students emotional health after five students committed suicide in a year. Before that, they assumed that pushing high schoolers to be perfect in their school work while also volunteering and being on a sports team and assigning many hours of home work a night would make them reasonably happy."
                , Body ". If any of those three things are the case, we ought to take better care of them. While there are some social programs that give (poor) high schoolers free lunches, it is framed as an ethical requirement, not an economic benefit. We force them to sit in classes, require that they ask permission to use the bathroom, and they certainly don't have any ping pong tables. People don't think there are self-interested reasons to provide high-schoolers with supportive environments."
                ]
            , Paragraph
                [ Body "But, high schoolers eventually graduate, and they do go on to their jobs. Generally, we believe that education will make people more productive. And when we talk about education making them more productive, we don't mean that sitting in a chair in a classroom helps, we must be talking about some combination of socialization and learning. Neither socialization nor learning is maximized by the current high school system. If you believe that software engineers are being made more productive by all the benefits they are given, we should give high schoolers similar benefits. But it may not be that these benefits are given to improve productivity. It's possible that the reason companies offer these benefits is to be seen as a more attractive place to work. In which case, assuming you want high schoolers to show up for school, you ought to give them the same benefits. There is no fundamental difference between a software engineer's set of motivations and a high schooler's, so if having these benefits at some location makes one more likely to show up to that location, you can assume it'll be true of the other."
                ]
            , Paragraph
                [ Body "Perhaps people don't believe that the purpose of high school is to educate. Suppose the purpose of high school is to keep young criminals out of trouble for some part of the day "
                , Footnote "An argument that Bryan Caplan makes pretty compellingly in \"The Case Against Education\"."
                , Body ", surely the more appealing you make high school, the less appealing you make skipping school?"
                ]
            , Paragraph
                [ Body "There are many reasons to believe that primary school isn't about education"
                , Footnote "And many reasons to believe it about college, but I won't go there today."
                , Body ". For example, Direct Instruction, a teaching paradigm, has been shown to have better outcomes for students than standard high school classes. But no public high schools have adopted it "
                , Footnote "There are some principled objections. For example, the argument that DI is \"teacher-proofed\" and so cannot allow excellent teachers to help students as much as they could otherwise. I'm glad the public school system thinks that having a few excellent teachers makes up for the rest."
                , Body "."
                ]
            , Plain "No one is quite sure what the purpose of high school is. Colleges value doing well in high school, because it predicts future success pretty well. But the people in charge of running high schools have forgotten that they could serve a purpose other than improving your chances of getting into a college. "
            ]
        , Model "Bullet Journaling"
            "bullet"
            Listed
            [ Paragraph
                [ Body "Bullet journaling is a mindfulness practice, where you sit down once a day to write down the tasks you want to accomplish in that day, and the events that will happen that day, and you add notes throughout the day "
                , Footnote "There's more to it, at least as it's written in their official dogma, but this is the general idea."
                , Body ". During the day, you can plan around the tasks you should be doing and the events that will happen. I started using this system "
                , Footnote "Well, my variant on it."
                , Body " in January 2019 and have quite enjoyed it. Bullet journaling helps me follow through on my commitments, achieve long-term goals, define whether a day was successful or not "
                , Footnote "That's right, motherfucker. This is a high-school-style five-paragraph essay."
                , Body "."
                ]
            , Paragraph
                [ Body "Bullet journaling helps with follow-through for somewhat obvious reasons. My journal is structured like this: 53 pages, one per week of the year "
                , Footnote "You probably think there are 52 weeks in a year. While this is not technically false, 52 * 7 is 364, so there will always be 53 weeks in a year."
                , Body ", followed by empty pages, which are filled up with my daily to-do and event lists "
                , Footnote "This is a lie. In reality, the first half is structured as one weekly page, followed by seven daily pages, followed by the next weekly page, and so on. But in the second half, I pulled my shit together and wrote all the weekly pages in a row. This is nice because I can reference them all easily, and plan for the future, but annoying because I have to flip back and forth over many pages. But my next bullet journal will be 53 weekly pages followed by daily pages."
                , Body ". Whenever I pick up some new responsibility, I write down the date that I have to take each action it required. So, for example, if a friend tells me he's going to fight lions on October 13, and needs me to film it, I'll write down on whatever week October 13th falls on that I have to film a lion fight. When the week rolls around, I'll see this commitment, and know that I'll be a bit busy that Friday. Sometimes, though, you have obligations that are less specific. For example, suppose someone tells me that I have to help them burglarize a house at some point in June. In this case, I'll make a note in the page for some week in May to follow up and ask for details "
                , Footnote """For example, "Which house?", "What's in it for me?", and "How are you not in jail?" """
                , Body ". And when I get a new deadline, I'll write down not just the deadline but also multiple steps and the times I should do them. For example, for my PhD applications, I saw they had deadlines in late December through early January. So in a week in August, I wrote that I should decide which schools I'm applying to, in a week in October, that I should have finished my writing sample "
                , Footnote "Full disclosure: It is November, and the writing sample is still not finished."
                , Body ", and so on, so that each task gets done at the right time, and when December rolls around I'll be ready and unstressed "
                , Footnote "Leaving aside the previously mentioned unfinished writing sample."
                ]
            , Paragraph
                [ Body "Achieving long-term goals follows pretty naturally from writing down obligations. When I have some long-term goal that I want to achieve, I can split it into small steps, and achieve each of these as they come up in the journal. The fundamental algorithm for achieving hard things"
                , Footnote "Which I stole shamelessly from someone on the internet."
                , Body "is as follows:"
                , NumberedList
                    [ "Find something that is like the hard thing but is easy."
                    , "Modify the easy thing so that it is like the hard thing in exactly one way that you find hard."
                    , "Do the modified thing until it is no longer hard."
                    , "If the original hard thing is now easy, you’re done. If not, go back to step 2."
                    ]
                , Body "Each time you have a new easy thing that is like the hard thing in one way, you can write tasks in your bullet journal to do that thing. Like the example I gave with my PhD applications. Applying to PhD programs is difficult, but deciding which schools would be interesting is not. So, write that down as a  task. Then, the next step is to create a writing sample "
                , Footnote "Writing the writing sample may itself be difficult. But one of the benefits of the general algorithm for achieving hard things is that you can apply it recursively. If modifying the easy thing so that it is like the hard thing in exactly one way is itself a hard problem, you have a whole algorithm just for solving hard problems ready at hand."
                , Body ". Repeat a few more times, and you've done your whole PhD application "
                , Footnote "Keeping in mind that I am writing this essay while procrastinating on finishing the writing sample."
                , Body "."
                ]
            , Paragraph
                [ Body "The most underrated benefit from bullet journaling, though, is being able to define a day, or week, or year as successful. If you write down your goals for each day in the morning, then in the evening, you can see whether those goals were met or not. If they were, congrats! You've had a successful day. If not, do better tomorrow. It's critical, though, that if you are writing down tasks for your immediate goals and tasks that will eventually achieve your long-term goals, simply following the to-do list in your bullet journal lets you achieve everything you need to! Additionally, you're going to have to complete the tasks that weren't completed eventually. Ending a day with tasks unfinished means more work tomorrow, on top of being unsatisfied with today. So writing something in my bullet journal helps me get it done, because once it's written, I am committed to doing it, and may as well just get it done today. Bullet journaling doesn't just define success, it helps you achieve it."
                ]
            , Paragraph
                [ Body "I know I said this was going to be a five-paragraph essay, but conclusions are useless when someone can just reread your introduction "
                , Footnote "The reason they're required in high school because high schools are based on colleges, and the reason they're required in colleges is because colleges used to exist to train people to give better legal arguments, and legal arguments at the time were oral, not written."
                , Body ", so I'll end it here."
                ]
            ]
        , Model "The Coolest Kid at the Hackathon"
            "coolest-kid-at-the-hackathon"
            NotListed
            [ Paragraph
                [ Body "I was the coolest kid at HackBeanpot 2019 "
                , Footnote "Admittedly, being the coolest person at any hackathon isn't that hard. I'm 100% sure that writing about it on your personal website helps."
                , Body ". The hackathon itself was held February 8-10, in the Seaport area of Boston "
                , Footnote "If you're imagining tall brick buildings on the waterfront, you're right on."
                , Body ", and I was there because my friend invited me. I had known this guy for about a year. He was the friend of my old randomly-assigned college roommate, and he was a big data science/machine learning nerd, so let's call him DataScience. Working with us was a guy DataScience knew. I remember only two facts about him: he was a freshman, and his last name had far too many X's in it "
                , Footnote "I think it was only one X, but it was at the beginning of his last name. That's excessive."
                , Body ". Call him X "
                , Footnote "Or Freshman. I don't care what you call him, and he is not an important character in this story."
                , Body "."
                ]
            , Paragraph
                [ Body "We spend about an hour hanging around DataScience's apartment, figuring out what project we want to work on. We get bullied pretty quickly into making a website that lets you visualize and explore the data you can export from Facebook. Then, we gather supplies. I have a laptop, and so I'm set. DataScience brings an external monitor, and a keyboard, the whole nine yards "
                , Footnote "Or at least, the whole two peripherals."
                , Body ". We decide we're ready, call an Uber, and make bad jokes about programming the whole way there."
                ]
            , Paragraph
                [ Body "We show up to the Hackathon, and listen to people telling us about the rules we should follow and how to have good sportsmanship and so on. We idle impatiently near the door to the work area. When the introduction is over,  they let us in to the area where we are going to be working for the next 48 hours. We are, obviously, one of the first groups in, and push towards a nice room in the back "
                , Footnote "It shocks me, as always, how many people listen to opening speeches so far from where they need to be immediately after the speech ends. Surely, you must get tired of being stuck in the crowd of people pushing through doors?"
                , Body ". We set up our room, and start drawing plans on a whiteboard."
                ]
            , Paragraph
                [ Body "People drift in and out of our room, asking questions and telling us about events "
                , Footnote "Hackathons always have a thousand different events. Yoga? Really? How is that going to help me make fun of Facebook? The lectures on a programming tool I understood completely, but the sparkling water blind taste test challenge I did not. "
                , Body " that are happening. We hack and hack and hack into the night. Pizza comes. Normally, pizza arriving at a hackathon is not worthy of a sentence in a description of that hackathon "
                , Footnote "But clearly, I think that descriptions of the normality of pizza at hackathons is worth a sentence."
                , Body ", it's just to be expected. What is incredible about this particular arrival-of-pizza scenario is the sheer quantity of pizza. This hackathon has maybe 200 people present. The pizzas are stacked on a counter in the kitchen, a rectangular prism "
                , Footnote "A word that means \"3D rectangle\" and also \"I want to brag about my math education\". "
                , Body " two boxes wide, seven boxes long, and ten boxes high "
                , Footnote "I counted."
                , Body ". Doing the math, that comes out to 140 boxes of pizza, for 200 people. This is my dream. I take two boxes "
                , Footnote "One meat-lover's, one vegetable-heavy pizza that also included meat. "
                , Body " back to my team's room, and we keep working. At 3AM, I call it for myself, head home, and crash hard. X leaves with me, but DataScience stays there. He gestures frantically at the Google Sheet we made earlier that day, which demonstrates our work/sleep schedules for the weekend. Apparently, it indicates that he is staying there and working until 7AM, a mild insanity which I am far too tired to argue about. "
                ]
            , Paragraph
                [ Body "I wake up the next morning, shower "
                , Footnote "Unlike many other people attending the hackathon."
                , Body ", and take the train in. On the train with me is my old TA, someone who I never got close to but argued with constantly during labs "
                , Footnote "Which were only labs in the weakest of senses. Computer Science labs always seem a bit forced. The purpose of a biology or chemistry or geology lab session is the lab building – you cannot perform these experiments anywhere else. The purpose of a computer science lab is to force you to write some code, and maybe to have TAs make fun of you while you do it. Imagine an \"English lab\". No one would take it seriously. But somehow, computer science, by virtue of having science in the name, gets away with it."
                , Body ". We chat a bit about the hackathon. I ask him if he goes to hackathons often. He says yes. He asks if I do. I say I don't. I ask him if he graduated. He says yes. I say \"cool.\". It's the sort of conversation that doesn't communicate any information, but makes you feel that you followed a social norm. Normally, this feels good, but I had slept around five hours the previous night, so it just made me feel more tired."
                ]
            , Paragraph
                [ Body "We eventually make it in to the hackathon. I go in to the room, and find DataScience passed out on the couch. I sit outside, working on my laptop, trying to decipher the changes he made after I went home last night. The last commit is at seven AM "
                , Footnote "And, amazingly, it has a coherent message and no obvious bugs. My 7AM commits tend to have messages like \"code\" and \"it works now\", and include obvious bugs with comments about how I don't care."
                , Body ". I pick up where he left off. I write code for most of the morning, with X once he shows up. I wander around fairly often, to stretch my legs and rest my eyes, and ask other teams what they're working on. I am unsure whether I am flirting with the girls on the teams, partially because I'm at a hackathon, which is a non-flirting-friendly place, and partially because I'm never sure if I'm flirting with anyone."
                ]
            , Paragraph
                [ Body "At four, I put a piece of paper on my tongue "
                , Footnote "Hopefully, this is enough to count as plausible deniability."
                , Footnote "Reason 1 why I was the coolest kid at the hackathon."
                , Body ", and coding becomes much more interesting. I can see the abstract syntax tree of the code I am writing, and see links between different pieces of the code that connect together, but my memory gradually deteriorates and typing becomes difficult, because I am too interested in watching the letters on each key swim around."
                ]
            , Paragraph
                [ Body "Now is probably the best time to explain why I was eating paper. That night, I was going to a sorority social "
                , Footnote "Not typically my type of event, but I was invited, and it would be churlish to refuse."
                , Body ", and instead of drinking "
                , Footnote "Because I was going to a sorority event, I wanted to make sure that I wanted to be edgier than the other people there, so I could assure myself that I wasn't one of them."
                , Body ", I picked a different drug. I take a Lyft home, and stare at the driver until she stops trying to talk to me. I stare out the window the rest of the drive. We go over a bridge, as the sun sets"
                , Footnote "It was 5PM. Fuck daylight savings time."
                , Body "and I notice that everything looks like a painting, everything looks beautiful, and I smile to myself and enjoy the rest of the drive home."
                , Footnote "The Uber driver rated me five stars, somehow, despite how strangely I was acting."
                ]
            , Paragraph
                [ Body "I get home, and change into my sorority social outfit, which is, unsurprisingly, different from my hackathon outfit "
                , Footnote "My outfit for the social included a red velvet turtleneck. The other men at the pre-game were very impressed that I knew how to dress. They were all wearing navy suits, and clearly didn't realize that men's fashion could be anything else."
                , Body ". I step into my room, changing, and then step back out into the living room "
                , Footnote "Or the twenty square feet of space that are considered a living room in Northeastern dorms"
                , Body " and notice the bathroom door is closed. I am slightly concerned about this fact, because it was open when I walked into my room. Of course, I am quite high at this point, and I could be mistaken, but I start to get concerned. After a brief moment that feels like an eternity, one of my roommates steps out of the bathroom. I scream a little, then calm down, then tell her "
                , Footnote "Yes, I have had female roommates. For some people reading this story, this is the most surprising part."
                , Body "I walk ten minutes or so to my girlfriends place, where she is hosting a pregame "
                , Footnote "Or \"presocial\", but that sounds more like a phase of infant development than a type of party"
                , Body "."
                ]
            , Paragraph
                [ Body "In the distance between my apartment and hers, nothing interesting happens "
                , Footnote "Which is quite strange, for a trip (of the walking kind) during a trip (of the drug kind)."
                , Body "."
                ]
            , Paragraph
                [ Body "I arrive and stride in, boldly "
                , Footnote "The front door of her apartment building has conveniently had a broken lock for several months, at this point. It makes striding in boldly much easier."
                , Body ". Showing up to a party and immediately walking into a girl's bedroom with the girl in tow gives you some respect from everyone else there "
                , Footnote "Except for Keith. But fuck that guy."
                , Body ". I give her a piece of paper of her own, and take another half of one myself "
                , Footnote "No one ever said undergrads made good decisions."
                , Body ", and we rejoin the party. No one knows. This is the pregame for a sorority event, so everyone is quite drunk. I know too many people here. One guy tries to talk to me about entrepreneurship and a startup idea I mentioned in passing to him "
                , Footnote "Remember Keith, from Footnote ? This is why I don't like him."
                , Body ". Someone tries to Ice me. I take the ice, and walk away, somewhere between confusion (what a strange world we live in, where one type of drink demands immediate drinking) and judgement (fuck these frat-type people). I notice how parched my throat was, and how nice the cold Ice tastes. I drink four more over the next hour. This was a bad decision."
                ]
            , Paragraph
                [ Body "Slightly tipsy "
                , Footnote "An understatement."
                , Body "and severely high, I notice that everyone is being shooed out the door. The check-in for the social is starting. In my confusion, I had forgotten for a moment that each person going to a sorority event has to check-in at a place on campus before the event. This is to ensure that everyone is sober enough to make it through the night without embarassing the sorority "
                , Footnote "If you are a member of a group that encourages (underage) drinking, at least pick a group that won't shame you for (underage) drinking."
                , Body ". So all forty of the people who were at the pregame head out. We form a line of well-dressed and warm men and scantily-clad "
                , Footnote "But, some would say, still well-dressed."
                , Body "and cold women as we stream towards the auditorium where we have to check in."
                ]
            , Paragraph
                [ Body "The auditorium has a bad aura "
                , Footnote "I only see or believe in auras while I'm tripping. But in that moment, I knew it was not where I wanted to be."
                , Body ". The check-in table is set up in the business building "
                , Footnote "Already, a bad sign."
                , Body ", and, the second I step in, I feel overwhelmed. Hundreds of drunk freshman throng the lobby, chattering and prattling and jabbering and blabbering. I overhear a girl talk about breaking up with a guy because he cheated on her with her sister. Before I realized she meant sorority sister, I felt very sad on her behalf. After realizing, I just felt regular amounts of sadness. My date and I push forward, trying to get to the desk. As we enter the crowd, I look her in the eyes, and say \"If I lose you in this crowd, I will lose my mind\". I meant it. I grab her hand, and hold it tightly, and she pulls me forward. We make it to the desk, sign in, and rush back outside. But it's too cold outside, so we wait in the airlock between the lobby and the outdoors. Here, we overhear a few more comical stories, meet up with our buddies, and head out."
                ]
            , Paragraph
                [ Body "You may be wondering what I mean when I say buddy. Each person in the sorority brings a date "
                , Footnote "Or tries to."
                , Body ", and pairs of dates are buddied up. Neither pair can get in to the venue without the other, and the pairs will get in trouble if the other pair doesn't sign out at the end of the night. Our \"buddy\" is a small, energetic Asian freshman, who reminds me in many ways of a girl who was an intern at the same company as me, the summer after my freshman year. Over the course of the night, she earns disapproving looks from three different bouncers, show us her (unconvincing) fake ID, and makes emphatically sure that we know she loves Northeastern."
                ]
            , Paragraph
                [ Body "We arrive at the venue. The bright gold sign proclaims that the venue is named \"Gilt\". I had been hearing it as \"Guilt\" until this point, and I am given a brief moment of clarity in an otherwise very confusing night. The line is long, and cold, and hilarious. People in front of us occassionally stumble as they walk, and the most egregious stumbles, the stumbles that result in falling over the divider that creates the line, results in people getting kicked out. I'm having a grand old time. I get patted down at the entrance. The bouncer asks me if I have any nips on me. I resist the obvious joke that I have two. After investigating most of my pockets, he lets me in. I don't have any alcohol on me, but I do have three more pieces of paper in my wallet." ]
            , Paragraph
                [ Body "We show up, and I try to dance, but my heart just isn't in it. There is a feeling of judgement, that dancing at this party isn't doing anything useful, that the music is too loud, that the people are valid, that the entire Greek life system is fundamentally flawed, and so on "
                , Footnote "This sentence uses passive voice, but I'll admit in the footnotes: I'm the person doing the judging."
                , Body ". I loiter on the side of the dance floor. My date and I discuss leaving, and then agree we should leave. We walk out, as people in the line are still coming in. A girl who is in the class I TA sees me, and says hi. I am unhappy with this notion. I wave, and we head out."
                ]
            , Paragraph
                [ Body "Choosing to leave the event after a mere five minutes inside makes me feel superior to the people who are going to spend more than five minutes there "
                , Footnote "I'm going to not consider what it says about me that I show up to events that I feel good about leaving."
                , Body ". We call an Uber, and as we're waiting for it, see two girls who are barred from entering. The bouncers say that they are too drunk, and kick them out. They walk up the street towards where we are, and hide behind us, afraid of the people in line seeing them. We offer to share our Uber with them, as we're heading to similar places, they agree"
                , Footnote "Well, my date offers. I am nowhere near this kind, and also think it would be rather creepy if I asked two young drunk sorority girls to share an Uber with me."
                , Body ". The Uber arrives, and we drive back. It was horrific. The girls share the story of not being let in, from their perspective. Their perspective was wrong. They begin the ride by saying that neither of them had drank anything "
                , Footnote "In which case, it was rather confusing that they were slurring the words in their defense of their sobriety."
                , Body ", and that they didn't want to go to the social anyway "
                , Footnote "In which case, it was rather confusing that they showed up at all. On the other hand, I was there, despite not really wanting to be, so I can't really make fun of them for this one."
                , Body ". My date and I exchange glances through the rear-view mirror. She consoles them a little. The next iteration of their story, they just had one shot each"
                , Footnote "Shots, famously, being the best way to consume small amounts of alcohol over a long period of time"
                , Body "and they are slightly bummed about not getting in, but mostly worried about their buddy "
                , Footnote "I don't think I said this last time I mentioned the buddy system, but it's paternalist bullshit."
                , Body ". I believe this story slightly more. My date talks about how they'll be laughing about this in a year "
                , Footnote "I certainly am laughing at them a year later, but I don't think this is what she meant."
                , Body ", and not to worry about it. They tell us the next iteration of their story, in which they consumed slightly more alcohol, and feel slightly sadder. But they make sure to blame the bouncer for being a dick, because underaged drinking is expected at sorority events, so they didn't really do anything wrong. This pattern repeats for a few more iterations, and then we arrive "
                , Footnote "Thank god."
                , Body "."
                ]
            , Paragraph
                [ Body "My ex and I walk in to her apartment, lie down on her bed, and each eat some more paper. Just as I place it on my tongue, there's an urgent pounding at the door. Because I'm high, I assume that the police have found out about us. I start worrying. My date is in the bathroom, so I get up and go to the door. I open it, and it's just a normal guy"
                , Footnote "Actually he was 6'9\": the Northeastern baseball team lived in the same building, so everyone weas freakishly tall."
                , Body ". He tells me there's a fire in the building, and we have to evacuate. I don't hear any fire alarms going off, and we're on the first floor near the door, so I'm not too worried, but I tell my date to hurry up in the bathroom anyway. I pull my shoes on, grab my jacket, and loiter near the front door watching the scene unfold. Various people in various states stumble out the front door. A few minutes in, a fire truck pulls in, and two firemen in full fire gear burst through the front door and run upstairs. At this point, I yell at my date that we really have to be getting a move on, because there's a real fire; she and I run out. Waiting on the street, we are captivated by the beautiful lights on the fire truck and ambulance, and talk about how crazy it is that this night, of all nights, would be the night her apartment building burned down. Her roommates walks out a few minutes later, with a backpack and talking about how she brought her laptop and passport and other valuables out of the building. We feel like idiots for not bringing anything "
                , Footnote "Well, she does. All my stuff is in my apartment, a 10 minute walk away."
                , Body " but it's far too late to go back into the building."
                ]
            , Paragraph
                [ Body "One of my exes, let's call her Russia, walks out of the building. Well, ex is not strictly the correct characterization. She and I are no longer seeing each other, but at the time I was alternating nights between my date and Russia "
                , Footnote "Russian happened to be my former student, and was at the time TAing the same class as me. It was a strange semester for my sex life."
                , Body ". She and I see each other, and say nothing "
                , Footnote "We had a relationship based on mutual disrespect."
                , Body ". We awkwardly glance at each other for a while, but mostly we're concerned with the building on fire next to us. It's cold out, so my date and I decide to head to my apartment. We make it as far as the street corner, before we encounter Russia and her friends. Fortunately "
                , Footnote "Unfortunately."
                , Body ", my date knew one of my ex's friends, so my ex and I have to studiously ignore each other while they talk. Then, mercifully, we head opposite directions."
                ]
            , Paragraph
                [ Body "We make it back to my apartment. One of my date's friends wants her to come hang out. There is some drama, someone offended someone, and my date feels the urge to apologize in this moment. Because she's very high "
                , Footnote "And, because I want to have sex with her."
                , Body ", I tell her that this would be a bad idea. She starts panicking in a serious sense, hyperventilating and being unable to form complete sentences. I talk her down from the panic, and we drink some water, and then we have incredible sex "
                , Footnote "The paper we ate helped."
                , Body ". We come down a bit, hanging around and drinking water. We go back to her place. It wasn't on fire, someone on the third floor just burnt something while cooking. We toss and turn and toss and turn and eventually fall asleep."
                ]
            , Plain "And the next morning, I wake up at 7 AM, and go back to the hackathon."
            ]
        , Model
            "How to Get an A+ in Algorithms"
            "how-to-get-an-a-in-algorithms"
            Listed
            [ Paragraph
                [ Body "Many systems at Northeastern have poorly-designed incentive structures. The Algorithms course is one particularly egregious example. I got a nearly-perfect grade in that class, primarily because I understood the incentive structures better than most other students."
                ]
            , Paragraph
                [ Body "Of course, I did in fact study and learn in that class. I use Anki, a spaced-repetition flashcarding tool that makes classes much easier. Each lecture, I would record a few (usually around a dozen) notes, the facts that I figured I ought to remember for the rest of the semester, and make a flashcard for each of those facts. This certainly helped, but I still made several mistakes on the first homework assignment. I understood the material well, but it is easy to make small mistakes while doing math, and even easier to make mistakes while doing math you don't care about. " ]
            , Paragraph
                [ Body "So, I thought about the way that the homeworks are graded. At office hours, a TA told me that they give you the points if anythign in your answer is correct. This is a terrible idea. In my future homeworks, I would sometimes answer a question two different ways"
                , Footnote "Many of the questions were ambigious enough that there were multiple correct answers, making this less insane than it sounds."
                , Body "and if one of the two was correct, the TAs would mark it correct and give me the points, even as they crossed out the other half of the answer. When a proof was assigned, I would write pages and pages of inscrutable prose, with the correct answer almost definitely in one of the statements I proposed. I would lead with the answer I believed to be most likely to be true, and then write a whole lot more. Why not? If anything in there is correct, I would be given the points, so the more I wrote, the higher the expected grade. For the rest of the semester, I continued this strategy of proof by intimidation, and my grades rose to reflect my deeper understanding of the grading system "
                , Footnote "Not the material. But then again, what college class is actually about the material?"
                , Footnote "Fundamentals of Computer Science is actually about the material."
                , Body "."
                ]
            , Paragraph
                [ Body "This makes me an asshole. One of the TAs of the course, who I was quite friendly with by the end of the semester "
                , Footnote "This sounds like I slept with him. I did not sleep with him."
                , Body ", told me that he hated grading my homeworks. I explained why I was incentivized to write what amounted to a treatise on each problem, and he grudgingly agreed that it makes sense. Still, I forced him to read far more pages of mediocre solutions to mediocre math problems than anyone ought to be forced to read "
                , Footnote "And it wasn't even typeset or in nice handwriting. I gave them the full cursive treatment. This is because I also understood that if everything I was saying seemed right, and reading it was difficult, the path of least resistance for a grader is to give up and give me a good grade. This is how I got through most of high school."
                , Body ". I would go to his office hours once a week, and we would collectively struggle through my handwriting and hazy memory of the previous week "
                , Footnote "This was a semester where I was drinking most Thursdays, Fridays, and Saturdays, so my memory was weaker than usual. And even usually, I have a terrible memory."
                , Body " to try to find the correct answer in the slew of answer-like things I had written."
                ]
            , Paragraph
                [ Body "The office hours were absurd for another reason. The TAs were not given the answer key to the homework, but they tended to know the answers to the homeworks "
                , Footnote "Although when they lead the review sessions for the tests, they tended to tell us that most of the information on the test was bullshit we would never use, to justify why they didn't know the answers to the questions on the test review. While I appreciated the honesty, this was not the time for that type of cynicism; I needed to get an A+ in the class so that I could be taken seriously when I wrote a snarky piece about it later."
                , Body ". And, they were fully authorized to tell you whether or not an answer was correct. So, I would show up to office hours with several reasonable answers to each question, and run them by the TAs until one was confirmed to be the right idea. Then, I would demonstrate it was true, complete the steps of the proofs, or draw the pretty picture  "
                , Footnote "Fuck doing graph theory problems on paper."
                , Body " that was required to get the correct answer. But importantly, I only had to do that work for answers I knew were correct, because I could confirm I was headed in the correct path. This saved a significant amount of time on the homeworks, time that was better-spent writing down four alternative solutions that were all along the path the TA described."
                ]
            , Paragraph
                [ Body "It may be a surprise, given everything else I've written here, but I enjoy learning, and think it has immense intrinsic value. I just also see the incentive structures behind (and instrumental value of) getting good grades. I learned a lot during this class "
                , Footnote "And ended up hanging out with two different girls to study (and not study), an unusually high number for a computer science class."
                , Body " and ended up enjoying it quite thoroughly "
                , Footnote "Although I won't speculate on whether I enjoyed the class because of the girls I met or because of the things I learned."
                , Body "."
                ]
            ]
        , Model
            "Stop Inviting Me to Bible Study"
            "bible-study"
            Listed
            [ Paragraph
                [ Body "I've been living in Denver for four months. Mostly, it has been like any other city, but more people smoke weed "
                , Footnote "This is an oversimplification. More people have tripped acid, also."
                , Body ". Denver is a revelrous city, with drunk people whirring past on electric scooters "
                , Footnote "Boston really needs to get its shit together with respect to e-scooters."
                , Body " pretty much constantly after 9PM "
                , Footnote "Or, to be honest to Denver's drinking culture, 6PM. My coworkers and I go out for drinks after work, and it turns out that two drinks at 5280 feet above sea level is equivalent to several bottles of wine at sea level. I got the opportunity to test this fact when I visited Boston for my 21st birthday. Drinking at sea level felt like a marathon. I drank a bottle of wine to warm up, had a few Mike's Hard Lemonade's, and then went out and had several drinks at several bars. I stayed vertical. After four drinks at altitude, I get horizontal."
                , Body ". But strangely, despite the higher rate of revelry per capita, people keep asking me to if I want to go to  bible study."
                ]
            , Paragraph
                [ Body "I am severely atheist. To convince me that God exists, you're going to need a whole new miracle, caught on several cameras. If your argument is predicated on the Bible being God's "
                , Footnote "As an atheist, do I have to capitalize God?"
                , Body "literal truth "
                , Footnote "Which the Bible itself doesn't claim. None of the Bible is Jesus's words, it's all random goons who happened to be nearby and wanted to talk to journalists."
                , Body ", I'm not going to be convinced. I signal this, strongly. A man wearing all black, scowling on a street corner, trying to cross the road, is probably not the best target for your Bible study. I was crossing 15th "
                , Footnote "The road itself doesn't matter, but saying a specific road probably convinces you that this really happened."
                , Body ", and a man waiting at the corner with me starts making sketchy eye contact with me. I say sketchy, because he was acting as if he was very attracted to me "
                , Footnote "A common Bible-study-inviter tactic. We'll revisit this."
                , Body ": he would glance over at me, and when I glared back "
                , Footnote "I was having a bad day. I don't normally glare."
                , Body ", he would avert his eyes. We played this game of stare-glare-avert several times, until he eventually started talking to me. He said hello, and so I said hello. He asked me if I wanted to go to Bible Study "
                , Footnote "Okay, I know that the S in Study isn't capitalized, but Jesus-types want me to capitalize so much else. I get carried away with capitalization around God."
                , Body """. I laugh, and say "Sorry! I'm very atheist, doesn't my outfit signal that?", as I gesture towards myself. I am wearing Chelsea boots, a famously atheistic shoe """
                , Footnote "At least, I wear Chelsea boots to signal my atheism."
                , Body """. He laughs back, and tries to convince me that anyone can wear any clothing. While he isn't strictly wrong, he is missing the point of clothing. Clothing is the most potent social signaling mechanism that we have, and wearing a long(ish) black coat over black jeans with black chelsea boots with a band shirt is a pretty strong social signal. It signals that I don't want to go to Bible study. """
                ]
            , Paragraph
                [ Body "He won't accept no for an answer "
                , Footnote "Which says bad things about him as a person."
                , Body ", and he presses me. Every Bible-study-inviter has a set of talking points. And, incredibly, they're all different "
                , Footnote "I wonder if they have a handbook. They probably don't A/B test opening lines, because I'm pretty sure they think statistics was invented 6000 years ago."
                , Body ". This guy asks me how long I've been studying math. I think for a second, forgetting for a moment how old I am "
                , Footnote "Everything since I turned 21 has been somewhat of a blur."
                , Body ", then I tell him fifteen years, which I figure is close enough to true "
                , Footnote "I'm sure it doesn't matter for his pickup line. And, in fact, it doesn't."
                , Body ". He asks me if I think I understand all of math, and I reply with a long laugh and a \"Nope!\". He doesn't find it as funny as I do. He asks me how long I've spent studying the Bible. I see where this is going. I say \"long enough\", which he doesn't hear at all. He asks whether I can understand, truly, what the Bible is saying, if I've studied it for less time that I've studied math. I tell him that I think I understand what Animorphs is saying, and I only studied each book in the series for a few hours tops. At least they had good cover art! He is not happy with this. We part ways. I turn left without telling him, and he goes straight. The same social pressures that lead me into conversation with him prevents him from turning around and following me. I make it home. Being asked to Bible study once is not particularly interesting, so I take no note."
                ]
            , Paragraph
                [ Body "The next day, I'm on CU Denver's campus "
                , Footnote "Sometimes I go to campus, and sit on a bench, and write poetry. Not that it's any of your business what I do on CU Denver's campus."
                , Body ". I hang out for a bit, revel in what I know to be one of the last days of summer "
                , Footnote "This is false. Denver's weather is slightly schizophrenic, so that it can be overcast and snowy on the same day that it's 70 and sunny."
                , Body ", bask in the sun, write a bit. I pack my stuff, stand up and leave, and as I'm walking out, I pass a gaggle of girls. One of them whispers something to another, which I hear as \"go for it!\", so I am marginally excited, as I think someone is about to ask for my number "
                , Footnote "Despite the fact that something like this has never happened before. I suffer from the male predisposition to fantasize about things like this."
                , Body ". But that is not the case. This cute black girl "
                , Footnote "My understanding of churches is that they are typically strongly segregated by race. The man in the previous paragraph was Asian. Are these two different churches? Are they both members of a single group of churches, united by their shared love of Bible study?"
                , Body " walks up to me, and my heart flutters a bit "
                , Footnote "Although, at 5280 feet above sea level, even just walking short distances causes my heart to flutter."
                , Body ". She asks me whether I want to go to Bible study this Thursday at 3. I laugh, and say I'm atheist. She has no banter. She just looks severely disappointed in me. I walk home, confused about the emerging pattern of people asking me to Bible study."
                ]
            , Paragraph
                [ Body "I am sitting on a bench on CU Denver's campus. I have had a weird morning "
                , Footnote "I met a man named Imhotep. I am firmly convinced he is a drug dealer. He gave me his number. We have plans to get drinks. He said the phrase \"movin' and groovin'\" at least three times."
                , Body ", and so I am ready for anything. Or so I think. A man "
                , Footnote "White, to emphasize the multicultural nature of the people who ask me to Bible study."
                , Body " asks if he can sit next to me. I say yes, assuming he is going to be an interesting bench compatriot. He is not. He asks what I'm doing on campus. I make something up. He asks me if I want to go to Bible study. By now, I've started formulating responses to this question. I quip \"I'm sorry, but I have a girlfriend. Thanks for the offer, though\". Despite hours "
                , Footnote "Maybe minutes. But I certainly had one full shower worth of thoughts about responses to Bible study invites."
                , Body " of time spent preparing this response, aimed at unsettling the asker enough to scare them away, designed to cut deeply at the (presumed) fear of homosexuality in the person asking me to Bible study, it has no effect. He laughs, and says it's not like that. He wants me to find Jesus inside me "
                , Footnote "A message with deeply homosexual undertones, I might add."
                , Body ". I tell him to please leave my bench. He asks whether I've ever experienced something I would consider unexplainable by science "
                , Footnote "No."
                , Body ". I tell him to please leave my bench. He asks whether I've ever been in a situation of mortal danger, when I thought I was going to die, but I lived "
                , Footnote "I've been to New York and tried to jaywalk, so the answer is definitively yes."
                , Body ". I tell him to please leave my bench, and this time he listens."
                ]
            , Paragraph
                [ Body "Too many people have asked me to Bible study for it to be a coincidence "
                , Footnote "They would tell me that this means I should believe in the Christian God. A meta-argument for Bible study."
                , Body ". I don't know what it is about me that attracts them. But I'd like to say, to all of them: please, stop asking me to Bible study. And, to God: you're going to have to send some more attractive girls."
                ]
            ]
        , Model "Professional StarCraft 2"
            "professional-starcraft"
            Listed
            [ Paragraph
                [ Body "I am, technically, a former professional StarCraft 2 player. When I was in middle school, I played StarCraft 2 extensively. I was very good for my age "
                , Footnote "I think."
                , Body ", and played in some age-limited tournaments, which I occasionally won. In fact, I won often enough that I recouped my tournament buy-ins, enough to cover the cost of the game itself, and I thought this was a pretty good deal "
                , Footnote "My slightly-older self realizes that spending hundreds of hours playing a game in order to make ~$100 is not a good deal. But as a child, I had far fewer opportunities to make money than I do today. In fact, it was probably illegal for people to hire me. And playing video games is more enjoyable than mowing lawns."
                , Body ". The money was nice, I guess "
                , Footnote "Can you tell I grew up in a firmly upper-ish-part-of-the-middle-class household? "
                , Body ", but playing StarCraft competitively changed my life. Other than being one of the reasons I met one of my best friends through all of middle and high school, it taught me the generic algorithm for getting better at things "
                , Footnote "Although it took me several years to realize that this algorithm could be applied to things other than StarCraft."
                ]
            , Paragraph
                [ Body "StarCraft 2 has a ladder system that provides immediate feedback on your performance. You can queue up, get matched against a stranger, and play a game. At the end of the game, the winner gains some points, and the loser loses some. Internally, they use Elo rankings, but they publish a different number. Regardless of the exact scoring mechanism, you can see this score trend up and down, and if you play enough games "
                , Footnote "And I was certainly playing enough games."
                , Body ", it becomes quite accurate. I floundered about in the lowest rung on the ladder, playing games and being so incompetent that I didn't even understand why I was losing games when I lost. I thought perhaps I could learn from the real professional games, and so I tuned in to a tournament. The camera flew around, showing different parts of the game map every second, mysterious things were happening, and the close-ups of the players' hands dancing over the keyboard were downright intimidating. Eventually, I found the livestream of a StarCraft personality, Day9. He would watch a replay of a game, commentating on why one player did better or worse than another, and importantly, his content was targeted at bad players like me."
                ]
            , Paragraph
                [ Body "He introduced me to the general algorithm for getting better at things."
                , NumberedList
                    [ "Choose the thing you want to get better at."
                    , "Choose one piece of that thing that you are doing poorly."
                    , "Practice, focusing only on that one piece, even at a cost to your overall performance."
                    , "Once you are doing better at the one piece, go to 2."
                    ]
                , Body "Armed with this way to create practice regimes, I did. I knew what I was failing at "
                , Footnote "And this part is hard. Often, the hardest part of this algorithm is step 2. If you are unconscious of your incompetence, you don't know what you ought to be improving."
                , Body ", and I could get better at those things! My ranking suffered in the short term, because I was focusing on some small parts of the game while neglecting almost everything else. But after picking enough of those small parts of the game to improve at, my ranking began to climb again. This felt really good. "
                ]
            , Paragraph
                [ Body "I kept playing StarCraft, occasionally competitively, mostly just with friends, for most of middle school. By the time high school rolled around, I was starting to make the transition into MineCraft and various indie single-player games, which aren't skill-focused. They're about enjoying yourself and making cool-looking contraptions or following a story. There's nothing inherently wrong with this type of game, but they will never provide someone with an environment where they really want to grow their skill at something. StarCraft 2, for me, was like learning how to play an instrument. The reason we want children to learn how to play instruments "
                , Footnote "Except guitar. If you pressure your kid to learn guitar, you want them to get laid. But oboe? Not so much."
                , Body " is to give them this same experience. This is the growth mindset –\u{00A0}knowing that you can improve, and having to figure out how to devise training regimens for yourself. This is an invaluable skill. Much to my parent's disappointment, clarinet did not teach me any of this, but fortunately, I managed to get some of the same outcomes through video games."
                ]
            , Plain "See, Mom? I told you playing video games was good for me."
            ]
        , Model "Of Course We're Dependent On Alcohol"
            "of-course-were-dependent-on-alcohol"
            Listed
            [ Paragraph
                [ Body "We co-evolved with it. Alcohol was discovered thousands of years ago "
                , Footnote "I made this fact up. But it sounds believable, right? Alcohol's been around for a bit, is all I really mean to say, but thousands of years is just so much more impressive. "
                , Body " and humans have been drinking it ever since. If humans evolved with alcohol, then it makes since that we would evolve to be dependent on it."
                ]
            , Paragraph
                [ Body "Obviously, the environment that a population exists in influences the path of their evolution. But a bunch of creatures in a warmer climate, those with less fur will thrive, in a colder climate, just the opposite. All evolution works in this way: based on the environment that a species is in, certain traits are advantageous, and those traits allow their owners to have more children (or more successful children), so they take over the population. But this may not make as much sense in the human context. Reproductive fitness, in the modern age, is more determined by will to have children than ability to produce enough for your children to survive. This is why religions that encourage their members to have many kids, or avoid using birth control, tend to survive"
                , Footnote "Unfortunately."
                , Body "."
                ]
            , Paragraph
                [ Body "But thousands of years ago, in semi-pre-historic times, food was still scarce, rates of murder were quite high, and reproductive fitness was based on more than just your inclination to baby-making "
                , Footnote "In the literal sense, not the euphemistic one. Have as much protected sex as you want, it won't make you more fit (in the Darwinian sense, although it may make you more fit in the athletic sense)."
                , Body ". So the evolutionary pressure exerted by the environment were a bit stronger. And alcohol had been invented."
                ]
            , Paragraph
                [ Body "If we evolved in an environment with alcohol "
                , Footnote "We did."
                , Body " and the environment shapes evolution "
                , Footnote "It does."
                , Body "it follows that alcohol shaped our evolution. Human social instincts are the result of evolution. And the presence of alcohol shapes what social instincts are beneficial to have. In an environment where alcohol is present, people can choose to lower their inhibitions and become more gregarious, whenever they choose "
                , Footnote "Alcohol won't always be available, but let's imagine for a second."
                , Body ". If this is the case, then people whose social instincts are overly sociable have no way to fix this problem, while people whose social instincts are overly reserved can make up the difference with alcohol. So, I imagine, the best social disposition is one that is slightly too shy - shy enough to keep you safe from conmen "
                , Footnote "And conwomen. Conpeople? That sounds wrong. I'll keep it as conmen."
                , Body ", but open enough that a little bit of alcohol puts you in a sociable enough mood to meet new friends and dance in religious ceremonies and whatever else added to reproductive fitness thousands of years ago."
                ]
            ]
        , Model "Philosophy Is Hard"
            "philosophy-is-hard"
            Listed
            [ Paragraph
                [ Body "But people often act as if it's not. The default perception is that any person could do professional philosophy, but that only a few people can do professional math "
                , Footnote "I'm talking almost exclusively about research here. Engineers, I suppose, are mostly professional mathematicians."
                , Body ". I think that both are difficult, but in different ways, and that social perception here is wrong."
                ]
            , Paragraph
                [ Body "First, a caveat: it is much easier to bullshit in philosophy than in math. Because philosophy texts are so full of jargon and words with different meanings in philosophy and common parlance "
                , Footnote "I blame this on the GRE. Academics spend many hours studying vocabulary for this test, and then spend the rest of their careers trying to justify it by overusing esoteric words in their papers. This is my theory for why academics tend to write like assholes."
                , Body ", bullshit philosophy and real philosophy look quite similar. Because the medium for communicating is English, a relatively imprecise language, real philosophy described poorly can seem quite like bullshit, and well-presented bullshit philosophy can seem legitimate. In math, the mode of communicating is a symbolic logic, more precise, and much easier to fact check. Philosophers can make arguments in favor of a point, and then realize they were mistaken, in math, it is somewhat shameful to publish a proof that is later found to be flawed. It may be easier to bullshit philosophy than math, but that doesn't mean that doing good philosophy is easier than doing good math."
                ]
            , Paragraph
                [ Body "Philosophy, as a discipline, involves lots of reading and writing. Most people can read, and write, and so assume that philosophy can't be that hard. But the reading and writing is quite different; extracting a coherent argument from someone else's writing involves much more thought and care than just skimming a news article. Math, on the other hand, involves lots of messing around with little squiggly lines on paper. This is something that most people can't do, or at least are afraid of. So, math gets a reputation for being more difficult, because it involves squigglier lines." ]
            , Paragraph
                [ Body "A thought experiment someone told me once, aimed at identifying which academic disciplines are the easier, goes like this: \"Imagine that we live in a dictatorship, and all of the professors are forced into another discipline, chosen randomly. They have two years to get up to speed in that discipline, and then are expected to start producing research in that field. Who is the most scared?\" The example I heard it with was swapping the fields of literature and physics professors, where my intuition says that the physicists would have a better shot at adapting to their new field. But I'm not so sure about philosophy and math "
                , Footnote "Of course, there is a broad overlap between philosophy and math, for example, most of logic can be ascribed to either field. Pretend that there is no overlap here, for now. Imagine we swap the metaethicists with the linear algebraists."
                , Body ". Could math professors really understand metaethics? Would they be totally fine ramping up on Kant in two years? I think they would struggle as much as the philosophers who had to learn how to do mathematical research."
                ]
            , Plain "Math is hard. Philosophy is hard. Stop insulting the intelligence of philosophers, please."
            ]
        , Model "People Aren't Researching Software Engineering"
            "people-arent-researching-software-engineering"
            Listed
            [ Paragraph
                [ Body "Software engineering education is woefully understudied, software engineer productivity is largely unknown, and I think it's because of the incentives in academia."
                ]
            , Paragraph
                [ Body "There are few studies on how to teach programming, and so most people must be doing a bad job at it. Personally, I love Northeastern's curriculum, which begins with software design, and then gets into the details of implementing software. But we have no idea what the most effective way to teach developers is. There are some studies, where professors will teach one class to write their tests after their code, and another to write tests before their code "
                , Footnote "Although my experience as a TA for a class that demands you write tests before code is that students will always write the code first."
                , Body ". And then they will compare the average scores of the two classes, or something similar. These experiments are nice, but they are being conducted primarily by computer science professors, and so often make silly mistakes in their attempts at pedagogical research. The professors with degrees in Education are teaching classes about Education, and so can't influence CS curricula or run experiments on computer science students of their own. Even Northeastern's curriculum, which increased the number of students getting co-ops where they write code from 33% to 90%, is not considered to be superior "
                , Footnote "By the population of CS educators in general. At Northeastern, it has a pretty good rap."
                , Body " to the curriculum it replaced. So much effort is poured into getting middle schoolers higher standardized test scores, and so little into training computer scientists."
                ]
            , Paragraph
                [ Body "No one really knows what makes software engineers more productive. People who believe in different programming paradigms or methodologies yell at each other online, but there aren't any good studies where people are randomly assigned either language A or language B, and asked to solve the same problem, and then time to completion and number of bugs and similar metrics are compared. When studies like this are done, it is students solving trivial problems, and so I worry that the results do not generalize to more experienced developers developing software over longer periods of time. There are very few journals of Software Engineering Productivity. In academia, if there is no venue for publishing a paper, there is no incentive to write the paper. Instead, you see papers about new tools, programming languages or development environments or anything that software engineers use. Those papers make some bold claim about how their tool is better for developers, results in fewer bugs, or lowers development time, and they have to substantiate those claims. So, these fringe tools being created by academics are well-tested, and the tools that most actual engineers use are not. This is backwards, but it's clearly where the incentive structures lead."
                ]
            ]
        , Model
            "Pedestrian"
            "pedestrian"
            Listed
            [ Plain "We walk in the winter. The bitter, freezing wind burning at your exposed face, your hands gloved and shoved in pockets but nonetheless tingling a bit. Snow crunches underfoot, until it gives way to ice, where you must expend all of your available focus on just making horizontal motion happen without a fall. But this all is good for the soul. Brushing up against the primal fear of death by exposure, I feel all the more grateful when I make it back inside. For some moments, out on a walk in the cold, your life is in your own hands, you must make it back to shelter soon, and everyone else is inside, hiding from the elements, unable to help. You know you are safe, and that you will survive this brief winter walk, but parts of your brain are unaware of our modern civilization. Those parts of your brain reward you, when you find a reprieve from the cold in your apartment. But if you never walk, if you leave your heated apartment for your heated car for your heated destination, you don't really experience the winter."
            , Plain "We walk in the fall. Leaves crunch underfoot, but quietly; quietly enough, at least, that you can hear yourself think. Cars may rush by, pulling you out of your thoughts with the sputter of their exhaust and the hum of their engines, but walking, alone, the air brisk but not cold, you can ponder and ponder and ponder. You can sit on a bench, if you get tired, or if the ambulation of footsteps is disrupting you. You notice things, in the fall, mushrooms sprouting out of otherwise well-manicured yards, the decorations for the various holidays, the people you walk by. Motorists are always mad at each other, because they can't see each other's faces; pedestrians pass each other without contest, sometimes with a smile and a wave and a \"Good Morning!\". You can grab the leaves, throw them in the air, stomp them and kick them and even jump in them."
            , Plain "We walk in the summer. Wind blows cool over your face, pulls your shirt away from your sweaty back, provides an iota of relief. The heat is not fun, by any means, but it makes the water you drink all the sweeter. You can enjoy a nice drink in the comfort of your apartment, insulated from the world by four walls and a roof and the knowledge that you have somewhere to stay. But that drink, no matter what it is, will feel worse than ice-cold water on a summer walk. The sun may beat down, your sunglasses seemingly useless, your eyes blind. A bench, in the shade, may provide respite. The summer walk demands a shower, when you return home. Walking near the water, you fantasize about jumping in, cooling off. But you don't. Remembering the winter, so recent in memory, the cold that chills you to your bones, you catch as much of the sun as you can, expel the last remnants of winter in your body and your spirit. The summer walk is a sauna. Relax, and let the sun beat down, and feel your skin turn golden-brown, and bask in the moment."
            , Plain "We walk in the spring. Mist and rain together drizzle down, or perhaps it's just a fog, but anyway it's rather damp and you feel droplets accumulate in your hair. A light jacket, pants, boots, serve to protect you, mostly, from this set of weather. Are you wearing rainboots? If so, go ahead, jump in the puddle. If not, jump anyways, we'll all be thoroughly waterlogged at the end of the walk anyway, may as well hasten the process. See the skyscrapers, enshrouded in fog, cut off halfway up, looking as if they go on forever, past the fog line. The light is dim, though the sun is overhead, the clouds diffusing and blocking the sun to various degrees, so that you can see everything, but everything still seems concealed. The animals are out, again, and you can hear birds twitter away in the trees that have yet to regrow their leaves."
            , Plain "They drive. And they feel nothing."
            ]
        , Model "What Are Libraries For?"
            "what-are-libraries-for"
            Listed
            [ Paragraph
                [ Body "I am sitting in a library while writing this. It's no good, writing in a library. The goal of perfect silence, an admirable goal, means that the library is almost dead still. Except, of course, once or twice a minute, when someone zips up a backpack, drops a book, or says a few words to their friend. Then, because the moments before and after the noise are so silent, it stands out, makes you look up, stop concentrating. The syncopated bursts of sound in otherwise deep silence doesn't work for me, the coffeeshop's consistent white noise being far less distracting "
                , Footnote "Until a rambunctious group of people comes in, and they start loudly discussing their friend group's most recent breakup. During those moments, I prefer the library."
                , Body "."
                ]
            , Paragraph
                [ Body "I am conscious of the sound of my typing. One of the keys on my keyboard squeaks a little, every time you press it. It never annoyed me before. Now, I slam the key down, hoping the mushy thud of a laptop key being pressed will drown out the squeak, hoping that I'm not causing anyone undue annoyance. Near me, a pencil scratches on a piece of paper. An eraser, pushed past the edge of the page, squeaks onto the table. No comments from the people in the library, these are the noises of people at work. My laptop keyboard, squeak or no, is a working sound, and so accepted. Nearby, a man chuckles, and the people near him shush him."
                ]
            , Plain "Libraries are the only place where people will always shush you if you laugh."
            , Paragraph
                [ Body "I type away on my computer. When I need to look something up, I open a new tab, and launch a search "
                , Footnote "On DuckDuckGo, not on Google. You should be afraid of Google."
                , Body ". The books next to me, although they may hold the answer, aren't as easily searched as the internet. Fiction, and books I want to read for fun, those I can understand having in print form, because there's something distinctive about reading a physical book. But my textbooks, and books I'll want to cite later, those I prefer having on a computer. But libraries still hold on to shelves after shelves of reference material. There are computers here, available for the public to use "
                , Footnote "Although I'm not sure I would trust the cleanliness of the keyboards."
                , Body ", so everyone else who needs to learn some particular fact will turn to Google "
                , Footnote "And probably not DuckDuckGo, unfortunately."
                , Body "."
                ]
            , Plain "I hear clothing rustling. Someone whispers \"stop\", and the rustling stops. A hushed conversation, back-and-forth, between a guy and a girl. I can't hear what words they say, but I can hear the tones of their voices. A couple is having sex in this library."
            , Paragraph
                [ Body "Many of the people in the library are clearly students. One is watching a movie, presumed Netflix. One is scolling through Facebook. One is on Reddit. Two, sitting next to each other, are on Twitter. I bet they all consider this time \"studying\". One girl, sitting behind a pile of what looks to be pre-med coursework, has four takeout boxes open on the table: a hamburger, a cheeseburger, fries, curly fries. All are untouched."
                ]
            , Plain "Sitting in the library was okay. At the very least, I wrote this."
            ]
        , Model
            "What Are You Supposed To Do When A Fire Alarm Goes Off?"
            "fire-alarm"
            Listed
            [ Paragraph
                [ Body "I feel like I really should know the answer to this question, but I don't. Years of living in a dorm desensitized me to fire alarms. When a fire alarm goes off once a year, I can make a big deal about it, but when you're at five per week "
                , Footnote "Of course, all of them between 2 and 6 AM. Thanks, stoners."
                , Body ", you stop caring."
                ]
            , Paragraph
                [ Body "In a dorm, at least, someone told me when I moved in what to do if there was a fire alarm "
                , Footnote "I didn't listen. I was too busy trying to flirt with every girl in my dorm at the same time, and failing miserably."
                , Body ". So, when the fire alarm did happen, I could pop my head out the door, see everyone else making for the stairs, and decide to follow them. My freshman year, fire alarms were quite regular. I owned a pair of large, flowy, cotton pants, tie-dyed in a rainbow, from red near the waist to purple at the ankle "
                , Footnote "I made it myself. It was the choice of a younger, more colorful me."
                , Body ". I would wear those, and whatever t-shirt I was wearing the previous day, and would stagger down five flights of stairs with the rest of my bleary-eyed dormmates. We would loiter across the street, and I would always meet someone new, an acquaintance of an acquaintance. I almost appreciated the fire-alarms, as a way of putting everyone in one place with nothing to do but socialize "
                , Footnote "Or check their phones incessantly. But at 4AM, everyone knows no one is up and texting you, so you're not fooling anyone."
                , Body ". After several of these fire alarms, we start to get a bit slower in our responses. We have to clear out of our rooms, or firefighters might yell at us, but the timeline is reasonably long, a few minutes at least. We realize that we can afford to pack our backpack with whatever we need, laptops to do work on, water bottles filled with alcohol if it's a Friday or Saturday night fire alarm "
                , Footnote "The worst timing for a fire alarm. The best timing is when you're already leaving, and so the fire alarm doesn't effect you. A fire alarm can't make your day better, but with good timing, it won't make your day worse."
                , Body ", that sort of thing."
                ]
            , Paragraph
                [ Body "So in a dorm, I knew what was up: grab some stuff, head down, wait for the fire fighters to show up, as resigned as you are about this one dorm building's alarm system "
                , Footnote "Halfway through the year, they disabled the fire alarms entirely. At first, I was scared about losing that measure of safety against fires, and then I realized that I was happy to trade a small risk of dying in a fire for being able to sleep through the night."
                , Body ". A fire alarm is a reason for a grumble, but not panic."
                ]
            , Paragraph
                [ Body "Today, in my apartment building, the fire alarm went off. This was the first time since I moved in that the entire, building-wide alarm went off "
                , Footnote "I have set off my apartment's fire alarm multiple times while cooking, but managed to ventilate well enough that it didn't trigger the whole building. You know, how competent people handle fire alarms."
                , Body ". I didn't know what to do! Should I panic? Probably not, right? Not having anyone else around to confirm this with "
                , Footnote "And, also, being high enough that I didn't care."
                , Body ", I decided that it was no big deal, I would throw on pants "
                , Footnote "I was wearing pants in my apartment, but they were sweatpants. I figured odds were good of this not being a huge deal, so I had time to change into something more reasonable."
                , Body " and a jacket "
                , Footnote "I thought a light jacket would be appropriate. I stepped outside, and despite the day being a balmy 70 degrees, the night was 40-something. I was cold."
                , Body ". I storm down the stairs, other residents of the apartment building and I commiserating about how loud the fire alarm was "
                , Footnote "Many fire alarms are obnoxious, this fire alarm was absurd. I understand it has to be loud to get everyone's attention. And in my apartment, it was loud but not unbearable. But the second I step into the hallway, I get a splitting headache, the grinding sound (why was it grinding? who designed this alarm??) overwhelming my feeble ears."
                , Body ". I go for a lap around the block, hoping the situation will have resolved itself by the time I get back. Waiting on the sidewalk for the light to turn, so I can escape, other residents trickle in. We chat about our hopes, expectations, and goals for this fire alarm. Many of them are strangers, and almost certainly, I had quietly passed them in the stairs or shared a wordless elevator ride with them before. But today, in light of the fire alarm, we were all incredibly chatty "
                , Footnote "This is thinly veiled social commentary."
                , Body "."
                ]
            , Paragraph
                [ Body "Coming back from my walk, I notice some people loitering in front of the liquor store in my apartment building "
                , Footnote "The location of this liquor store is quite bad for my liver."
                , Body ". I chat with them, and learn that you can get fined stupid amounts of money for being in a building while the fire alarm is going off, up to $2000. This is the sort of information I wish adults had told me in my formative years. I was ready to stroll back in, and just assume that the fire alarm ending meant we were all safe. One woman is called away by a man who seems to be quite impatient, and does not convince me the building is safe. Eventually, it's just me and a girl wearing a barstool hoodie "
                , Footnote "Yikes."
                , Body ". We make our way back into the building. We call the elevator, and briefly discuss whether an elevator is safe to take in what may be a fire. I feign for the stairs, but we're interrupted: the elevator we called arrives. Two firemen, clad in full neon-and-black firefighting regalia, step outside. I ask them if it's safe for us to be in the building, and they say we're fine. I step into the elevator with this girl. We pull up to my floor. I think about asking for her number, as we've spent the last half-hour bantering about this fire. Then I remember she is wearing a barstool hoodie, and step out of the elevator. She tells me to have a good night. I tell her that I hope her apartment didn't burn down."
                ]
            ]
        , Model "Genetics + Statistics = I'm Getting Divorced"
            "genetics-statistics-divorce"
            Listed
            [ Paragraph
                [ Body "My parents are divorced. All of my grandparents are divorced "
                , Footnote "A fact I learned only in the first grade. They all divorced and remarried before I was born, so I just had eight grandparents, and no one found it necessary to tell me that I was only biologically related to four of them. One day, in first grade, we had to draw our family trees. I wrote my name down, put it in a circle. I wrote my parents names in circles, and drew two lines, one to my mom, one to my dad. I drew my grandparents, and realized that there were four lines from each of my parents. I asked my teacher for help; my teacher was equally confused. I asked my parents after school, and they filled me in on the whole divorce thing."
                , Body ". Divorce is highly heritable. Statistically, I am almost certain to get a divorce."
                ]
            , Paragraph
                [ Body "Perhaps the statistics don't apply here, but I am generally in favor of "
                , Link { text = "taking the outside view", href = "./outside/" }
                , Body ", so I have to imagine I'm not special. Anyway, looking at who I am as a person "
                , Footnote "Promiscuous, atheistic, and alcoholic, just for starters."
                , Body " it seems unlikely that I would be less likely to get divorced than the statistics predict. Even knowing that I have this genetic predisposition to getting divorced doesn't seem like it can help me escape it."
                ]
            , Paragraph
                [ Body "I hate the idea of predetermination. If you believe that the future is out of your control, you absolve yourself of all responsibility for improving your lot in life. People who believe in spirits or ghosts or gods implicitly give up some of their agency to these supernatural beings. Once you've given up that agency, you lose some of your power over the world, and some of the culpability for your mistakes. I don't approve of all that. My family history of divorce increased my chance of being divorced, but if I get divorced, it's still my fault. Genetic predetermination is weak, and escapable. I am at risk of divorce, but not guaranteed it."
                ]
            , Paragraph
                [ Body "Having a high chance of divorce seems like the sort of thing people should care about, when they're thinking about whether they might want to marry someone. People will give all kinds of advice about evaluating potential spouses: make sure that you have compatible star signs "
                , Footnote "Ha!"
                , Body ", that you have similar hobbies, that you agree on the number of children you want to have. For just about every attribute you can judge someone on, I have been told that that attribute determines whether we could be happy together. But I have never been told to look at a potential spouse's family history of divorce. It is the one of the best predictors of divorce, certainly a better predictor than star sign, but no one seems to advocate it. Maybe it's because most dating advice is old "
                , Footnote "For example, \"don't have premarital sex\" makes perfect sense in a world where there is no birth control, STDs are more likely to be fatal, and you can get more money for marrying if you are a virgin."
                , Body ", and understanding of genetics and the heritability of divorce is relatively new. Maybe we want to believe in freedom from genetic determinism, so we choose to ignore this piece of information. Whatever the reason, people seem to be ignoring something that could help them decide not to marry me. I think I'll choose to not be too upset about this irrationality."
                ]
            , Paragraph
                [ Body "I also have genetic predispositions to alcoholism, depression, and heart disease "
                , Footnote "But also, to becoming a professor or engineer. My genes aren't all bad, I just like to complain. Maybe I have a genetic predisposition to complaining."
                , Body ". Lots of my friends would not genetically modify their children's embryos to improve them. I suspect that my position on genetic enhancement "
                , Footnote "I think genetically enhancing your children is as obligatory as reading to them."
                , Body ", while backed up by what I find to be sound philosophical arguments, probably derives from my fear of the ticking time-bombs in my genes. I'm aware that I can overcome just about anything in my genes, that nothing is guaranteed. But statistically, things look somewhat bleak."
                ]
            ]
        , Model
            "Daniel G, Attempted Yelp Casanova"
            "daniel-g"
            Listed
            [ Paragraph
                [ Body "Sometimes, I read Yelp reviews. Never to figure out whether a restaurant is good or not "
                , Footnote "I'm not very picky. Most restaurants are good, and I don't trust other people know which restaurants I'll like. When I'm with a group of people, and someone says they have two ideas for where to go, I'll tell them to pick the first one. If everyone agrees, we get to avoid ten minutes of bickering about what sounds better based on someone's hazy recollection, and go to a restaurant unsullied by the trash-talking of the propoenents of the second choice."
                , Body ", but to try and remember what I found amusing about a place I've been before, or to convince picky eaters that the Yelp reviewers of the world are on my side. I was scrolling through Yelp reviews of my apartment building, and saw a review from a man named Daniel G. He said that he has \"recently moved to Denver to work in tech\", which amused me. Such pointless detail! I don't care that you work in tech, I care about whether you wrote down the name of the security company that our apartment building uses "
                , Footnote "Don't worry about why."
                , Body ". And he didn't, in fact, include the one piece of information I wanted in his review. But his bold opener intrigued me, so I investigated further."
                ]
            , Paragraph
                [ Body "Here's what we know about Daniel G, from his Yelp profile. He just moved to Denver to work in tech. At the top of his profile, right below his name, it says \"Honolulu, HI\", from his reviews, he clearly lives in Denver, so I am unsure what Hawai'i has to do with him. Nonetheless, it's safe to assume that he's thought about Hawai'i at least several times. His photo is the most generically white man you can imagine, brunette but with a forgettable hairstyle, his whole face so astoundingly normal that it's difficult to describe. He is wearing a suit, a black blazer and black tie with a light blue shirt "
                , Footnote "It doesn't look good, but it fits with the rest of his look: bland, uninspired."
                , Body ". He looks somewhat like a cardboard cutout. The proportions are wrong, or the lighting is bad, or something. "
                ]
            , Paragraph
                [ Body "Let's discuss some of his reviews. He started in LA, before moving to reviewing Denver spots, lending some credence to his earlier claim that he was new to Denver. In LA, he rated the same bar four times, drowning them in praise. Here's something strange, though: his first review says that they have \"amazing cocktails, and even better food\", his next review says that the \"cocktail menu looks awesome\". Did he forget that he has tried the cocktails? Did they redesign their menu, and he decided just to look at it this time, not ordering anything, not commenting on the drinks themselves, just the presentation "
                , Footnote "Of the menu, not the drinks."
                , Body "? He includes a photo, in that same review: a burger, partially disassembled but not eaten, a half-empty glass of water to the side, two sets of utensils "
                , Footnote "But only one glass, so I doubt he was there with anyone. Also, he is a prolific Yelp reviewer, so I doubt he was there with anyone."
                , Body ". He seems to be sitting at a couch; the table is low to the ground, the same height as the seat of the couch opposite him. The empty couch opposite him."
                ]
            , Paragraph
                [ Body "\"The lounge area is the best\", he says, in his most recent review. \"I'll be coming here all the time\", he follows, but surely, given that this is his fourth review, he has already been going there all the time? Dnaiel G is uncertain of his relationship with this bar."
                ]
            , Paragraph
                [ Body "Let's move, now, to Daniel's reviews of places in Denver. I knew that he rated my apartment building, but that was not the only apartment building he rated. In fact, Daniel doesn't live in my apartment building. He lives in a different building owned by the same management. His first review, in May, sings the praise of his building. His next review, in August, is of the management company that runs both of our buildings."
                ]
            , Plain "So far, so good."
            , Paragraph
                [ Body "His next review, on the same day as his review of the management, is a bad copy-and-paste job of his review of the management in general. He missed the first letter. One starts \"I recently moved to Denver to work in tech…\", while the other just starts \"recently moved to Denver to work in tech…\". What motivates a man to copy-paste his Yelp reviews, to tell people looking at an apartment build he doesn't live in that the management has done a pretty good job "
                , Footnote "He seems a bit more excited than that. \"Highly recommend!\", he says."
                , Body " elsewhere?"
                ]
            , Paragraph
                [ Body "This is clearly a mating call. He's telling people he's new to Denver and works in tech. New to Denver is informing the world he's single, that he's well-traveled "
                , Footnote "At least, that he's lived in at least two places in his life."
                , Body ", and where to find him "
                , Footnote "Although presumably the only people looking at Yelp reviews of apartment buildings in Denver already live there."
                , Body ". He's saying he works in tech to advertise that he's doing alright for himself, that despite being normal in literally every other way, that he might just make an above-average salary. He's advertising his apartment building, which is reasonably nice, to corroborate this fact. Daniel G is on Yelp to find himself a woman "
                , Footnote "Or man, I suppose, but his incredibly bog-standard white-man appearance gives me some confidence in my heteronormative assumption."
                , Body "."
                ]
            ]
        , Model "Wear Black To Frat Parties"
            "wear-black-to-frat-parties"
            Listed
            [ Paragraph
                [ Body "You will have more sex if you use game theory to inform your choice of outfits "
                , Footnote "This sentence perfectly captures who I am as a person."
                , Body ". In this essay, I will argue that you should be wearing black to frat parties. If you're going to a frat party, I hope your goal is to have casual sex. I hope this for two reasons, first, because I go frat parties for casual sex, and having other people there with the same goal makes that easier, and second, because there is no other possible justification "
                , Footnote "Well, the booze is cheap. If you're there to drink economically, I support that as well."
                , Body " for spending several hours crammed in a smallish basement with a hundred people. Assuming that your goal is to find a partner, let's talk about the game theory behind this choice."
                ]
            , Paragraph
                [ Body "Finding a sexual partner at a frat party is roughly equivalent to the "
                , Link { text = "stable marriage problem", href = "https://en.wikipedia.org/wiki/Stable_marriage_problem" }
                , Body " "
                , Footnote "Except that you're not looking for a marriage, or anything close to that."
                , Footnote "And that a little instability can be fun."
                , Body ". Assume for a moment we just care about the heterosexual subset of the people at this party, and further, we only care about the people who are there to get laid "
                , Footnote "When I say \"care\", I mean for the stable matching, not in general."
                , Body ". There is imperfect information, and people don't even know their own preference set over the people at the party "
                , Footnote "Partially because people never know their exact preference set, partially because a dark basement with flashing lights is not the best place to evaluate someone else's attractiveness. "
                , Body ", but the general principle is the same. People are trying to end the night matched with a partner, and they have some set of preferences over partners."
                ]
            , Paragraph
                [ Body "So let's examine these preference sets! It is pretty easy to observe that, on average, tall, muscular men are more attractive. You don't have to spend a lot of time at frat parties in order to discover these facts. But it is important to notice the girls that these men are making out with "
                , Footnote "Making out with so very very publicly."
                , Body ". The girls they make out with fall within one particular archetype, slender and sorority-esque. Around the edges of the party, however, there are people that don't fit either of these archetypes. People who look more like musicians, or like art students, chat with each other. And they're pretty hot too! While many people are attracted primarily to people who fit the main frat party archetype, those are not the only people there. However, the standard frat archetype is the largest group present, so most people are tailoring the way they look to be attractive to those people."
                ]
            , Paragraph
                [ Body "But the more people that are competing for a girl's attention, the harder it will be for you to stand out. The hordes of tall, fit white men wearing button-downs and jeans are rough competition. As long as your preference set is similar to mine, where you find lots of different types of people attractive, you don't want to be competing for this oversaturated part of the population. Think about the preference sets of the girls that you find attractive. Some of them absolutely love the button-down look. They have plenty of options, so trying to be at the top of their preference set is difficult. But some of them prefer men wearing something else "
                , Footnote "Of course, it's not just about physical appearance. Wearing a button-down and blue jeans signals very different things about who you are as a person compared to wearing black jeans and a black band t-shirt, especially in a frat context."
                , Body ". It is far easier to be at the top of their preference set. And, in my experience at least, it is better to be at the top of a few people's lists than in the middle of many people's lists. If you're in the middle of someone's preference set, then even if they find you attractive, they're keeping their eyes on the top of their preference set, seeing if those people are interested. It doesn't matter if everyone in the room finds you passably attractive if no one is actively interested. However, if you are near the top of, say, four or five people's preference sets, presumably one of them will be willing to go home with you. "
                ]
            , Paragraph
                [ Body "I stopped going to frat parties "
                , Footnote "Partially because I turned 21 and started having more options for basements to spend Friday and Saturday nights in, partially because I discovered ways to meet women outside of basements."
                , Body " so this doesn't matter as much for me as it used to "
                , Footnote "Also, I gained 40 lbs of muscle, which I'm sure changes the payoffs for different aesthetics. I was kicked out of a frat for having sex with someone on a couch in their basement while a party was happening upstairs. Prior to gaining weight, I don't think that people would have been as willing to have frat-basement-couch sex with me. "
                , Body ". But it's an interesting application of game theory, and it made a large difference in how I experienced my junior year of college."
                ]
            ]
        , Model "The Punisher Guards My Pool"
            "the-punisher-guards-my-pool"
            Listed
            [ Paragraph
                [ Body "My apartment building has a pool "
                , Footnote "In my first week, I went to read by the pool. After reading for about an hour, I noticed my skin was turning pink: I had a sunburn. That day, I spent a while learning about things that work differently at 5280 feet above sea level."
                , Footnote "And I realized that the reason so many apartment buildings in Denver have pools is because there are no beaches, which made me slightly sad. But most of the sadness went away when I remembered that I had a pool."
                , Body ". This pool has three conflicting user groups: the people who want to read quietly, the riotous twenty-somethings who want to blast music, and the parents who want to teach their babies to swim."
                ]
            , Paragraph
                [ Body "In order to resolve this conflict, my apartment building's management decided to institute some policies. For example, each resident can only have two guests with them at a time in a pool, open alcoholic beverages can't go in the pool, and you can't play loud music. This excludes the riotous twenty-somethings, leaving the readers "
                , Footnote "Just me, really."
                , Body " to be annoyed at the parents for having such loud offspring. But the riotous twenty-somethings aren't exactly the types of people who follow rules "
                , Footnote "Before you judge them, consider how many rules you broke in college, and remember that living in Denver as a twenty-something is the closest you can get to college without paying tuition."
                , Body ", and so they continued to riot, blast music, drink, jump into the pool, and so on. I didn't really mind, I empathized with them and understood their motivations entirely. But slowly, the parents complained on the internal social media site that management spun up for residents of the apartment building "
                , Footnote "This social media site was pretty strange. The only times people used it was when something changed and they were unhappy. The staff would run a competition once a month where the person who posted the pet photo that got the most likes would get a $20 gift card to a pet store. The only posts I ever saw were people whoring their pets out for gift cards, and people complaining about the pool."
                , Body ". Eventually, management took action."
                ]
            , Paragraph
                [ Body "I saw a post mentioning that a \"Courtesy Patrol\" was going to be instituted. This Courtesy Patrol was going to make sure that the rules were followed. I thought nothing of it, except for a cursory curse at having RAs again. I go to the pool that same week, and notice nothing different, and lacking obvious evidence in favor of the existence of the Courtesy Patrol, I assume that it was just a scare tactic."
                ]
            , Plain "I was wrong."
            , Paragraph
                [ Body "The next time I go to the pool, it's some holiday. As I beep in "
                , Footnote "Beeping is the technical term for unlocking something with a keyfob."
                , Body " to the pool. I see what appears to be the precursor to a mass shooting. The pool and the people around it are eerie, silent, motionless. I see a man, standing off to the side of the path, dressed in all black. He is wearing all black, from his tactical sunglasses "
                , Footnote "Actually, everything he wore was black, and could easily be described as tactical, so I'll leave that off of the rest of the descriptions."
                , Body " down to his combat boots. Black cargo pants, a long-sleeve shirt under a bulletproof vest, various bits of equipment hanging from his vest and belt. Most notably: a combat truncheon "
                , Footnote "Not that there are other kinds of truncheon."
                , Body ", and a gun. He stands there, glaring out at the pool, at the people trying to have fun. Once I realize he is the Courtesy Patrol, my heartbeat slows from \"Am I about to get shot?\" to \"There is an man with a gun near me but he's probably safe, I hope\"."
                ]
            , Paragraph
                [ Body "The security company he works for is called Front Range Patrol. Their logo is this: "
                , Image { href = "../assets/front_range_patrol_logo.png" }
                ]
            , Paragraph
                [ Body "There is no high-resolution version of the logo in the center, only this fake police badge. Other than the fact that they're clearly pretending to be police officers, there is an obvious flaw with their logo: it's a fucking skull. How can you convince yourself that your company is doing the right thing when they give you a gun and tell you to wear body armor that has several large skulls on it? There's a whole "
                , Link { text = "sketch", href = "https://www.youtube.com/watch?v=hn1VxaMEjRU" }
                , Body " about this! And, even worse, the particular skull they use is the same as the logo of The Punisher: "
                , Image { href = "../assets/punisher_real.jpeg" }
                , Body "If you look really closely, you can even see that they copied the stars and stripes. We get it, you have a superhero fantasy inside your fantasy of being a police officer, but you don't have to make it so obvious!"
                ]
            , Paragraph
                [ Body "He starts walking towards me, and my heartbeat elevates again "
                , Footnote "In case it isn't obvious, I don't like guns, or authority, and I especially don't like when authorities have guns."
                , Body ". He asks if I have any guests coming. I stammer out a no, and keep walking to the nearest available pool chair. It is uncomfortable to talk to someone wearing body armor covered in skulls at any time, but it is undoubtedly more uncomfortable while you are shirtless."
                ]
            , Paragraph
                [ Body "I find a chair, sit, and try to read. Anytime someone new walks into the pool area, The Punisher asks them which resident they are the guest of. There is a minor scene when someone tries to bring three guests, which is strictly forbidden. I say that one of them is my guest, earning looks of gratitude from these people, and a minor glare from The Punisher, who says something about knowing that I'm up to no good before he returns to his post. This continues for a little while, until I decide that I am not having a good time anymore, and I go back to my apartment to read there."
                ]
            , Paragraph
                [ Body "The pool was quiet, and I could read without distractions, but to the parents who pushed for this change: would you rather your kids growing up seeing people having fun (sometimes obnoxiously), or that they grow up learning that when people are doing things you don't like, you should hire men with guns to make them stop? "
                ]
            ]
        , Model
            "Frat Basement Couch Sex"
            "yikes"
            NotListed
            [ Paragraph
                [ Body "MIT has more frats than attractive people "
                , Footnote "Sorry, MIT. Please still let me into your PhD program."
                , Body ", so the frats welcome Northeastern students with open arms. I ended up going to quite a few parties across the river. This is the story of one such party."
                ]
            , Paragraph
                [ Body "It was Saturday "
                , Footnote "I'm guessing, but I have a 50%ish chance of being right."
                , Body ". Around dinnertime, some of my friends invites me to a party at MIT. I get ready "
                , Footnote "That is to say, I start drinking."
                , Body ", and head down the hill to their apartment. We hang out and drink for a bit, shoot the shit in the collegiate way, sprawled across a couch, drinking and laughing and enjoying the heat of a summer night. This couch was slightly broken, because a month or so earlier, during St Patrick's day, we got drunk "
                , Footnote "We started drinking at noon. I blacked out at 3 PM. That's a personal record!"
                , Body " and fought a little "
                , Footnote "We fought a lot. To be fair, getting drunk and fighting is Irish tradition!"
                , Body ". In the heat of the fighting, or wrestling, or whatever you want to call it, I throw someone onto the couch "
                , Footnote "It's not so much that I didn't know my own strength, as that I wasn't sure if everyone else in the room knew my strength, so I had to show off. Or at least, this is what I reconstruct my motivation to be, I have but a hazy memory of this. The throw looked good on people's social medias, though, so I guess I have that going for me."
                , Body ", and from that point onward, the couch was slightly wobbly and creaky."
                ]
            , Paragraph
                [ Body "We hop in two separate Ubers "
                , Footnote "Ten or twelve of us went. That's a lot of people to bring to a party. That's enough people to have your own party!"
                , Body ", and drive over. I get lightly mocked for dancing to Ariana Grande in the car "
                , Footnote "I stand by it. \"break up with your girlfriend, i'm bored\" is art."
                , Body ". We arrive, and I realize with a start exactly which frat we're at."
                ]
            , Paragraph
                [ Body "My freshman year, I was a prolific frat thief. I would go to frat parties, and drink and dance and snog, which are all behaviors sanctioned by the people who are throwing the party. But I would also tend to leave with something –\u{00A0}a shot glass, a pocket beer, once some salt and pepper "
                , Footnote "I was out, and this was more convenient than going to a grocery store."
                , Body ". Most notably, near the end of my freshman year, I walked out of a frat house with a full create of pears. Buried in the pears was a handle of rum. The frat brothers manning the front door saw me walk out fully laden; I winked at them, and they didn't stop us "
                , Footnote "My friends tell a slightly different story. They claim that I ran out full speed, forgetting my jacket, and that one of them had to go back in to get it, because obviously the people running the place wouldn't be super happy to see me in that moment. I'll let you choose who to believe."
                , Body "."
                ]
            , Paragraph
                [ Body "So, I return to my old haunt "
                , Footnote "Or, because I stole from them and only ever visited once before, my old mark."
                , Body ", two full years later. I try to reflect for a moment on all that has happened between then and now, but I am too busy being pushed out of the Uber, into the frat, upstairs "
                , Footnote "Actually, before we went upstairs, we were diverted to a side room, where people were sitting at a desk. We wrote our names and colleges on a sign-in sheet (scanning the college column, I saw that everyone there was from Northeastern), then they write that you are over 21 and give you a wristband saying as much. This must either be some half-brained scheme to avoid legal liability (\"officer, the wristband said they were 21! how were we to know?\"), or just to give the older frat brothers plausible deniability when they sleep with freshmen (\"bro, she had a wristband that said she was 21. how was I to know?\")."
                , Body ". We get drinks. There's a solid crowd there, maybe sixty or seventy people, in a nice open room, large enough to fit everyone comfortably, small enough that people can rub against each other and pretend it's an accident "
                , Footnote "This is the perfect people to room size ratio for a frat party. Northeastern students tend to have normal houses with normal size basements, and cram over a hundred people in there. Being in these basements feels like getting groped on a Japanese subway."
                , Body ". We dance, and dance. My friend and I dance our way up to a group of girls, and introduce ourselves. Well, more accurately, I introduce myself as Julian, and my friend as \"Coolguy Wearsgoldchain\" "
                , Footnote "I am a bad wingman."
                , Body ". I had been making fun of him for wearing a gold chain earlier "
                , Footnote "He normally wears the chain tucked under his shirt, in a quite reasonable look. As we got ready to leave for this party, he pulls it out over his shirt. He looked like he was trying to impersonate a small-time drug dealer, and I wanted him to stop."
                , Body ", and botched this introduction. For him. It worked out pretty well for me. This girl and I dance closer, and closer, and closer, and so on."
                ]
            , Paragraph
                [ Body "After some discussion back-and-forth, with her friends saying out loud that she shouldn't sleep with me, and her saying that she wanted to "
                , Footnote "I appreciated the sentiment, but it is weird to see a group of girls yelling over music about whether one should sleep with you, particularly when your friends have moved on to somewhere else, so you're left dancing along while people decide how many people will sleep in your bed that night."
                , Body ". Eventually, her friends let her go with me. I remember seeing, as I walked in, a sign on a door, saying \"Basement. Do not enter!\", and I figure that a place where no one is allowed to go is the perfect place to bring a girl, moreso when that girl's friends just sanctioned a sexual encounter. We go down there, and we have the basement to ourselves. This basement is awesome. There's a bar on one side, a ring of couches on the other, a pool table, the works."
                , Body "This girl is the second-worst kisser I've ever had the displeasure of making out with. Her strategy for kissing someone goes roughly like this:"
                , NumberedList
                    [ "Close your eyes."
                    , "Fall towards the person you want to make out with."
                    , "Hope."
                    ]
                , Body "The first fall went alright, because I caught her. After the first one I was busy formulating a way to say \"What the fuck was that?\" in way that let me tell her what she was doing wrong "
                , Footnote "If you make out with someone, and they're a bad kisser, you have an obligation to tell them what they're doing wrong, and help them try to improve."
                , Body " in a way that would make her continue making out with me, just less dangerously. The second kamikaze dive took me by surprise. Our faces collided somewhat faster, and in slightly bonier places, than most kisses go. It hurt. I sit down on one of the couches, hoping that it would prevent this insanity from continuing. She closes her eyes, puckers her lips, and falls three feet onto me as I sit on the couch. Eventually, I decide she is too drunk to be hooking up with anyone, and I go back upstairs."
                ]
            , Paragraph
                [ Body "While this is a girl I fooled around with on a frat basement couch, this is not the titular "
                , Footnote "Heh."
                , Body " girl."
                ]
            , Paragraph
                [ Body "Once I'm back upstairs, my friends congratulate me for disappearing with a girl, but then I tell them the story, and some of them retract their congratulations. I get another drink, dance a bit more, and then I see her. A little shorter than me, blonde, wearing a tight blue halter top with white stripes "
                , Footnote "It is probably bad that this is most of what I remember about her."
                , Body ", dancing with her friends. I go over to say hello, and her friends rapidly part their circle, leaving me and her on the outside. I tell her I'm Julian, she tell's me she's Anna. We confirm that we both go to Northeastern. She tells me that she's a first-year, I tell her I am a third-year. She asks me what I do, and I ramble for a while about various projects I'm involved in "
                , Footnote "She was asking me what my major is."
                , Body ". She scroffs, and asks \"Do you think that makes you impressive?\", and I say \"Yes\". I smile, and she smiles, and I lean in, and she leans in, and then we're making out. This was a very public make-out, we were around fifteen feet away from the couches my friends were all sitting on. They cheer and hoot and holler and provide suggestions (which I ignored for the most part, but when they said \"grab her boobs!\" I pulled my face off hers and glare at them). Then, I realize that this was actually a pretty good suggestion, or at least something I would like to do, so I invite her to come with me to find somewhere quieter "
                , Footnote "Quieter meaning both more private and also literally quieter, frat parties are loud and my ears were starting to ring a bit."
                , Body ". So, obviously, we go to the basement. She seems slightly offput by the sign saying not to enter, so I tell her that it means that no one else is likely to be down there. She gets it. There is a frat boy hovering near the door, who tries to prevent us from going downstairs, but I make up a story (I think I tell him that I left my wallet downstairs), and he lets us through."
                ]
            , Paragraph
                [ Body "Twenty minutes after we go downstairs, we hear the door open. Shit. Let me describe the current state of that couch. I was lying down on my back, with my pants not quite off, but just on one leg and rotated 180 degrees so they were out of the way. I am still wearing one shoe, the shoe that is on the leg that holds my pants. Anna is sitting on top of me "
                , Footnote "So, in some strange sense, I am not fully naked, or at least not fully exposed."
                , Body ". She has her top mostly off, hanging loose around her neck, and that's all she's wearing. She can see the door from where she sits, I have to contort a little as I turn around to see what's happening."
                ]
            , Paragraph
                [ Body "Two frat guys walk in, somewhat obliviously, chatting with themselves. Then, they notice us on the couch. One stutters out a \"Uh... you can't be doing that here!\", the other one just turns around and walks out. I apologize profusely "
                , Footnote "And insincerely. I was pretty happy with the situation, other than being interrupted, and would do it again in a heartbeat."
                , Body ", but this guy keeps giving us variations on a theme: \"This is not the place for that\", \"I can't believe you're doing that here\", and on and on. He tells us to get dressed, and tells me to get out"
                , Footnote "He didn't tell her to get out. Frats want to keep around the women who have sex in frat basements, but not the men who do the same. There's a lot to unpack here."
                , Body ". Then, he waits in the doorframe. I am mildly uncomfortable. I say \"If you don't leave right now, you're going to see my penis while I change\". He is extremely uncomfortable. He leaves, Anna and I get dressed "
                , Footnote "And fool around a bit more, because we just got the room to ourselves again."
                , Body ", and then walk out. She makes a beeline to the second floor, to find her friends. I am accosted by two burly "
                , Footnote "Burly by MIT standards."
                , Body " frat bros, who tell me I have to leave."
                ]
            , Paragraph
                [ Body "As I'm getting ready to leave, finding my jacket and so on, I talk to the guys guarding me. We discover that we're all computer science majors, and talk about how certain grading practices are bullshit, and how our eyes and fingers hurt after we write code for hours, and generally bond over shared hardship. I talk about MIT's curriculum, and how Northeastern imitated and changed it, we have a brief discussion about our opinions on the best way to teach computer science. We make it to the lobby, and I sigh dramatically, \"I guess I'll leave, then\". They exchange glances, and say that I can stay, as long as I don't do it again."
                ]
            ]
        , Model
            "Flipping Cones"
            "flip"
            Listed
            [ Paragraph
                [ Body "Would you flip a traffic cone? As in, walk up to a traffic cone that's standing up and push it over, and then leave it lying on its side? Most people wouldn't. And it's not just that they wouldn't, but that the mere thought of flipping a traffic cone makes their heart race and makes them anxious."
                ]
            , Paragraph
                [ Body "People are afraid of violating social norms in public. This is a completely normal feeling to have. Historically, making strangers feel uncomfortable might get you killed, so it's understandable that the people who survived long enough to have children are those who feel uncomfortable making strangers uncomfortable. We inherited this desire to fit in socially, in a world where it doesn't really matter. The rule of law is strong, in most places, in most first-world countries, and strangers are really unlikely to murder you. But we still feel a deep-seated anxiety about breaking social norms."
                ]
            , Paragraph
                [ Body "I first flipped a cone in public to impress a girl "
                , Footnote "I was young and naive. I understand now that this was not very attractive."
                , Body ". Walking down a Boston street, I pushed a cone over. I felt the nervousness, the pounding heartbeat, and all the trappings of social anxiety. But nothing bad happened "
                , Footnote "Except that the date went poorly. But I think that was for other reasons."
                , Body ". It felt kinda good, actually, to have flipped this cone, to have defied this social norm, and to feel no repercussions."
                ]
            , Paragraph
                [ Body "I kept up this habit. I would flip cones often "
                , Footnote "More often when drunk."
                , Body ". It changed my general demeanor. I became more outgoing, more willing to be slightly transgressive, and more fearless in social interactions. While it's hard to say which way the causation runs, whether flipping cones made me more confident or being more confident made me more willing to flip cones, it sounds plausible that consistently violating social norms in public made me more unafraid of what I thought were social norms elsewhere. One result, for example, is that I started asking stores if they had student discounts, and eventually, just asking if they were running any promos that they could get me in on. Often they would say no, but occasionally they would say yes "
                , Footnote "I am realizing, years too late to act on it, that some of those cashiers that gave me a discount after some banter were into me."
                , Body ". So in a very real sense, flipping cones made me some money."
                ]
            , Paragraph
                [ Body "Violate the silly social norms, when it will cost you nothing, so that you can avoid blindly following the serious social norms, when it will cost you something."
                ]
            ]
        , Model
            "Alien Girl"
            "alien"
            NotListed
            [ Paragraph
                [ Body "The summer after my sophomore year of college, I slept around. I was living in a dorm. Well, not quite a dorm –\u{00A0}Northeastern leased an apartment building, and then leased it to me. And, for the first half of the summer at least, it was just me and one other guy in this massive apartment, that Northeastern normally crammed four students in. The living room alone was larger than my last apartment: the whole apartment was maybe 2100 square feet. My roommate and I threw many, many parties, finally freed from the tyrannical oppression of RAs. I had sex in a closet while people were in the adjoining room, standing in the bathtub while people were coming in and out of the bathroom during a party, and in my bed "
                , Footnote "Although this last one doesn't have the same shock factor as the previous two."
                , Body ". And during the week, when the party turnout wouldn't have been as high, I went on Tinder dates."
                , Body "Many of these Tinder dates went strangely. But there's one that really stands out, with a girl that my friends and I call Alien Girl "
                ]
            , Paragraph
                [ Body "We chatted on Tinder, briefly, exchanged numbers, and then made plans to meet up, all in the span of three days "
                , Footnote "In hindsight, this should have been a red flag."
                , Body ". We plan to meet in the Boston Common, so I come home from work a bit early, get changed, and head over. I text her that I'm there. No response. I hang around and enjoy the summer breeze. She doesn't text me back. Fifteen minutes pass, and I text her again. No response. I start to head home. Just as I make it back, she texts me that her phone died, but she found a place to charge it, and is now at the Common. Ugh. I tell her to meet me at the T stop near my apartment, and she says she's on her way."
                ]
            , Paragraph
                [ Body "We go for a walk, as the sun sets. She tells me that she works as an aesthetician, and I ask what that means "
                , Footnote "A bad idea. If you don't know what someone does for a living, just smile and nod. They're not going to be excited by your desire to learn about them, they're going to be offended that you are ignorant of the thing that they spend the majority of their waking hours doing."
                , Body ". She tells me. She tells me some interesting stories: how she gets naked at parties often "
                , Footnote "Hot."
                , Body " how she once slept with her boss for a promotion "
                , Footnote "Less hot."
                , Body ", how she drinks most nights "
                , Footnote "I'm honestly not sure whether this one is hot or not."
                , Body ". I try to keep up, telling some of the tamer stories from the parties my roommate and I had been throwing that summer. She shows me some photos of her in a bikini on her phone, under some pretext, but covers the image preview on the bottom of her phone, presumably because there were nudes there."
                ]
            , Paragraph
                [ Body "At this point, I'm conflicted between my desire to run far away and my desire to sleep with her "
                , Footnote "She was hot, and I was in a competition with my roommate to see who could have the most unique sexual partners in a week. I understand now that this competition was a bad idea, but at the time, I saw no problems with it."
                , Body ". The desire to sleep with her wins out, so I invite her up to my apartment."
                ]
            , Paragraph
                [ Body "We go back to my room, and start making out. We gradually shed our clothing and make our way into the bed. She makes some offhand comment, along the lines of \"you're kinkier than I thought you would be\", which is not actually a compliment. I banter with her a bit. When her mouth isn't full, she banters back. Offhandedly, she mentions that she has had sex with seven guys in the past week "
                , Footnote "Word of advice: if you are sleeping with seven unique people in a week, don't tell them. It devalues the experience."
                , Footnote "I got tested immediately after this encounter"
                , Body ". I stutter out some half-hearted rejoinder, saying that I was at three "
                , Footnote "The first two was a Harvard girl who was going to start her freshman year next year (with ID to prove it, although we didn't actually have sex because she was 17 and I hadn't learned about Massachusetts' shockingly young age of consent yet), and a girl who wanted me to slap her in the face during sex (with bruising to prove it). I honestly can't remember the third, although I remember saying three and don't remember it being a lie."
                , Body "."
                ]
            , Paragraph
                [ Body "I don't know if it was becasuse she was vegan, or something else, but she had the weirdest skin texture of anyone I've ever touched. She was thin "
                , Footnote "She said she modeled. Now that I think about it, it's quite likely she did porn."
                , Body ", but her skin hung weirdly off her body, oddly supple. Perhaps she had no muscle, anywhere, but regardless the reason, her skin felt loose on her body. And the texture of it, itself, was slightly odd, just enough to make me vaguely uncomfortable about touching her "
                , Footnote "Not enough to make me stop, though: remember that I was 20 years old, horny as hell, and in a competition over number of unique sexual partners."
                , Body ". This is why we called her alien girl. Everything about her was pretty alien to us."
                ]
            , Paragraph
                [ Body "Eventually, she stops, and tells me that she's not sure whether she wants to have sex with me or be my friend, becasuse I seem really cool and she wants to keep hanging out with me. Given that we've already exchanged pleasantries "
                , Footnote "Oral pleasantries, to be specific."
                , Body ", the distinction between that and sex seems minimal. I tell her that. She laughs, and starts putting her clothing on. She gets dressed and leaves. She texts me on her way out that she left a bag at my place, and that she wants to come back to pick up the bag and \"finish what she started\" "
                , Footnote "Direct quote from my text history."
                , Body ". I look in the bag: a loaf of bread, shampoo, hand sanitizer, bouillon cubes, a birthday card, cinnamon. Alien."
                ]
            ]
        , Model
            "\"Problematic\" is Problematic"
            "problematic"
            Listed
            [ Paragraph
                [ Body "The word \"problematic\" is uninformative and difficult to discuss earnestly. Often, it means \"racist\" or \"sexist\" or \"ageist\" "
                , Footnote "Although the people calling things problematic tend to care more about racism and sexism than ageism."
                , Body ". Those things are all problems, so it is true that they are problematic. But if you point at a book or movie or blog post, and call it problematic, you would do better to specify the problem."
                ]
            , Paragraph
                [ Body "If you call something problematic, I can't know exactly what you think the problem is. If I agree, we may think that we both share an opinion about whatever it is we're talking about, even when we think there are different problems. Imagine two people (with very different views about society) both reading East of Eden. One turns to the other and says its depiction of society is problematic, meaning that Steinbeck was sexist in his depictions of some characters "
                , Footnote "I agree with this. Cathy's only motivation in life was to manipulate other people. It didn't even seem as though she was doing it for her own benefit, most of the time, just that she got off on being manipulative."
                , Body ". The other agrees, because they think that it advocates for Christianity and blind devotion. These two people think they agree with each other about the particular problems in East of Eden, but they don't. They just agree that there is at least one problem."
                ]
            , Paragraph
                [ Body "This also makes it hard for authors to defend their works. If you call someone's novel problematic, for instance, your complain is not concrete enough to be addressed. They can't earnestly claim that there are no problems in their novel, and having to enumerate everything that might possibly be problematic and defend it would be a fool's errand. If you say that a novel is sexist, an author can ask why you think that. If there are particular scenes or characters or pieces of dialogue that you think are sexist, you can point those out. Then, the author can defend themselves, or realize that there is a problem. Calling something out as sexist seems to require bits of evidence to point at, while calling something problematic is vague enough to be true of everything. Asking someone to point out the sexist parts of a movie seems like a reasonable request, but asking them to point out the problematic parts doesn't even make sense as a question."
                ]
            ]
        , Model
            "The Villain on 16th Street"
            "villain-on-16th"
            Listed
            [ Paragraph
                [ Body "It was a brisk November night. I was walking around downtown Denver, with my girlfriend at the time "
                , Footnote "I am not entirely sure that she was my girlfriend. When I asked her what our relationship was, she said that it was confusing and difficult and she didn't want to talk about it. But we went on dates and had sex and such, so I'm guessing girlfriend is the right word."
                , Body ", taking some photos "
                , Footnote "An underrated date idea. It's free, it gets you to explore new places, and it's nice to hvae subjects in the photos you're taking. A side benefit is that it produces raw material to be refined into Instagram posts."
                , Body ". We try to shoot in various locations: a ballroom in a hotel "
                , Footnote "It looked unoccupied, from the street. We go in and ask the front desk staff if we can take photos in it. After five minutes of being told to talk to different people, we eventually get told that the ballroom is in use today, but if we come back some other day when it's empty, we can totally shoot photos there. This impresses the girl, because she was certain that they would reject us out of hand."
                , Body ", standing in the shrubbery at the base of a statue, and in front of a building that was lit all blue. As we loop back from these photos spots, we walk up 16th Street "
                , Footnote "Technically, it's \"16th Street Mall\",  but it's just a street, not a mall, so the name feels like false advertising to me."
                , Body "."
                ]
            , Paragraph
                [ Body "Along the way, I see this long, white hallway, brightly illumated with an interesting pattern on the walls. It looked like it would make a good backdrop for a photo. So, we step inside, into a plain, white lobby. On one wall, there is a sign saying \"Thank you for your patience. Be amazed\". I was unsure what exactly we were being patient for, or what the amazement was –\u{00A0}the lobby was completely empty, lacking any corporate branding. There was an elevator in the lobby, and the hallway, and that was it."
                ]
            , Paragraph
                [ Body "Behind a large desk, there were two security guards "
                , Footnote "I would say receptionists, but they both were wearing large jackets with a private security company's logo, so I guess they were security guards?"
                , Body "both wearing beanies with their heads down, avoiding eye contact. I wander over, and ask them if I can take photos in the hallway. They don't seem to hear. I ask again, and this time, one of them looks up. She takes out her AirPods, and asks me to repeat the question, so I do. She looks deeply confused. \"You want to take photos here?\", she asks, incredulously. I point at the hallway, talk about the various ways in which it is pretty, and explain why I would want to take photos. I have a DSLR around my neck, so it doesn't seem completely implausible. She turns to the other security guard, and nudges him. He looks up and removes his AirPods, then says that I'd have to talk to building management about taking photos there. I decide that this is more trouble than it would be worth, and turn to walk away. "
                ]
            , Paragraph
                [ Body "But I am struck by curiosity. Why is the lobby so empty? What patience are they thanking me for? What is this building for and who owns it? I actually ask the last question, and I am met with a blank stare from both security guards. One says, slowly, that she isn't sure. The other says that \"it is many things\". That sounds ominous, and you would think that the security guards for a building would know at least a little about what happens inside."
                ]
            , Paragraph
                [ Body "There is only one possible explanation for this building. It is an evil villain's lair, hidden in plain sight on 16th Street. It is many things, both an evil headquarters and a front for said headquarters. They are thanking us for our patience while they try to take over the world, at which point we'll be amazed. The AirPods in the guards' ears are used for mind control. It is clear: there is a villain on 16th Street."
                ]
            ]
        , Model
            "Shitty Design"
            "shitty-design"
            Listed
            [ Paragraph
                [ Body "Bathrooms in America are poorly-designed. Obviously, I have seen only a small subset of the America's bathrooms, and even then only a subset of the men's bathrooms. But there are some consistent design flaws that bathroom designers keep making, and it's pissing me off. " ]
            , Paragraph
                [ Body "Let's start with urinals. There are some pretty simple ideas here, like putting dividers between the urinals, but people tend to get most things about urinals right. One mistake I've seen, though, is having urinals next to the sinks. If you're going to do this, please, dear god, put a divider between the urinals and the sinks. I understand that there won't be someone peeing in the sink "
                , Footnote "Hopefully. I'm not making any promises here, though."
                , Body ", but I still would prefer to see zero dicks while I wash my hands "
                , Footnote "There are other contexts where I do want to see dicks, but when I am using a bathroom, I feel exactly zero desire to see dicks. Other than my own, I guess, it would be quite strange if I used the bathroom and didn't see my own dick."
                , Body ". For the most part, though, urinals are difficult to fuck up. They're splashy, and sometimes surrounded by a vast ocean of piss, but I'm willing to chalk those two things up to user error."
                ]
            , Paragraph
                [ Body "The doors on stalls should lock. This one seems pretty self-explanatory, and well-justified. The reason I went to the bathroom, instead of just shitting in the kitchen sink, is so that other people won't be looking at me while I'm doing my business "
                , Footnote "Okay, there are some other reasons to prefer bathrooms to kitchen sinks. But not interacting with other people is an important component."
                , Body ". But in many stalls, the lock on the door doesn't line up with the lock on the doorframe, or the lock isn't long enough and so doesn't reach over the massive gap between door and doorframe. This is bad. It signals not just laziness in construction or maintenance, but a fundamental issue with the way we construct our bathrooms. If you have a real door, and a real doorframe, then when the lock doesn't line up, you can fix it by adjusting how the door is attached. Here, the door and doorframe are suspended from the ceiling, and therefore difficult to move, and therefore difficult to fix."
                ]
            , Paragraph
                [ Body "The doors on stalls need handles. If you have a stall, where I can push the door shut, and then lock it, that's already a miracle. I would settle for doors that lock. But while we're talking about improvements we could be making to bathrooms, how about putting a handle on stall doors? I push a door shut, and then lock it, but when I unlock it, I have to stand and stare at the door, hoping that it will open on it's own. If it doesn't, I must resort to pulling on the lock, and the lock is clearly not meant for being pulled. Sometimes my fingers slip off the lock, the door refuses  to open, and I have to grab the top of the door and pull it towards me. There's no reason, in this modern world, why doors should be this difficult to open." ]
            , Paragraph
                [ Body "And the sinks have been getting worse over time. I actually have no problems with faucets that I have to physically touch to make water flow. Automatic faucets, however, seem to be poorly thought out. Typically, there is only one spigot, and no temperature control, so you are left at the whims of the plumbing. On hot days, when I want to splash cold water in my face, the water is scalding, on cold days, when I want to warm my hands, the water is freezing. Give me some control over the temperature, please. Our sink technology is regressing." ]
            , Paragraph
                [ Body "Automatic faucets also usually have the sensor outside of the sink bowl. So, when you are washing your hands, you have to hold your hands above the bowl. This means that water will splash all over the place. The bowl is there to catch splashes, and the height of the sensor makes it fail at that purpose. So in bathrooms with automatic sinks, the entire counter area is covered with water. When you lean up against it, to strain and reach to try and trigger the sensor, some of the water will soak into your pants. There's no need for this. Put a little lip on the countertop, and this problem is avoided. If you're going to push users of a bathroom to wash their hands in a way that will splash water all over the place, the least you can do is keep that water off of them." ]
            ]
        , Model "How to Order Food at Restaurants"
            "restaurant"
            Listed
            [ Paragraph
                [ Body "Lots of people suck at ordering food at restaurants. There are the obviously terrible orderers, the ones that wait for the waiter to come by the first time, and then ask the waiter what they like, and then decide that they don't like that dish and so they need to look at the menu to make up their mind for themselves. Those people are hopeless. But I have some hope for the people that read the whole menu, narrow it down to two or three choices, and then cannot decide between those choices without outside input. When you pick the dish that they get, however, you are implicitly accepting some of the blame, if the meal turns out bad. These otherwise reasonable people made a simple mistake: they read too much of the menu."
                ]
            , Paragraph
                [ Body "Once you read the whole menu, you're screwed. It is almost certain that the restaurant has more than one dish that looks good on their menu. So, now that you've read the whole menu, you have a shortlist of good-looking dishes that you have to choose from. I am not a picky eater, and I think most things would taste good, so for me, this list ends up being roughly half the size of the menu. Now you need to employ some way of choosing between these dishes, given very limited information. But even just looking at them all introduces analysis paralysis. Having more choices makes you value the end result less, and waste time deliberating over the choice. If I'm going out to eat with you, and you are spending too long making this decision, I am being deprived of conversation and therefore slightly sad."
                ]
            , Paragraph
                [ Body "There is a very simple algorithm for choosing what to eat at restaurants. Let's assume that you are just planning on ordering one main dish, no side or drink or anything fancy like that. Scan the menu, noting each item. Don't skim, really consider each dish, savoring the description. Once you see an item that you think will be good, pick it, remember its name, and close your menu. You're done. Don't open the menu again, except perhaps when the waiter comes and you have forgotten the name of the dish you want. If you order like this, you spend minimal time looking at the menu, avoid analysis paralysis, and you circumvent entirely the malaise that results from having a wide selection of good choices, of which you can pick just one. Stop looking once you've found something good, and you'll be happier than if you forgo good options to find something perfect. "
                ]
            ]
        , Model "Quantifying Sexuality"
            "quantifying-sexuality"
            Listed
            [ Paragraph
                [ Body "The Kinsey scale is okay. It's not the worst possible quantification of sexuality, but I think it misses some important elements. It can capture the spectrum from completely straight to completely gay, but misses the intensity of attraction. It can handle any ratio of attraction to same-sex or different-sex people, but it just tells you a ratio, not a magnitude."
                ]
            , Plain "My take on quantifying sexuality would be two-dimensional, a square more than a line. Instead of a one-dimensional representation, where you fall somewhere on the straight-gay line, I think you need two dimensions. One dimension would be same-sex attraction, and the other would be opposite-sex attraction. This allows the expression of more sexual identities than Kinsey's scale does. For example, asexual people are at (0, 0), having no attraction to either, or possibly (1,0) or (0,1) or (1,1), the point being, somewhere in the bottom-left, near the origin. Straight people would have high opposite-sex attraction, gay people high same-sex attraction, bisexual people high at both. But when I say high in that previous sentence, I mean relatively high. Let's say the scale is out of 7, to be more similar to Kinsey's scale. People who have no same-sex attraction and anywhere from a 3 to a 7 opposite-sex attraction might all identify as straight. Degree of sexual attraction is important to defining someone's sexual identify. Asexuality is not a boolean value, I think there's a scale from asexual to whatever the word for the opposite of asexual is. This scale would capture that."
            , Plain "It still has all the same information from the Kinsey scale, expressed as the difference between same-sex and opposite-sex attraction. But this maps more closely to my understanding of sexual identities. I am attracted to men very similarly to many of the girls I know, but I am far more attracted to women than most of the people I know. So, despite being just as happy leaving a party with a guy as my girl friends are, I tend to leave parties with girls. Degree of attraction is important to understanding sexual behavior, and the Kinsey scale misses that."
            ]
        , Model "The Multi-Objective Argument For Polyamory"
            "multi-objective-argument-for-polyamory"
            Listed
            [ Paragraph
                [ Body "I am polyamorous "
                , Footnote "And, honestly, I think you would be, if you had the opportunity."
                , Body ". I don't expect to convince people who are currently happily monogamous "
                , Footnote "Although mono-amorous is really the right term. I think that marriages with more than 2 people start to get confusing. But you can be having sex with multiple people concurrently with far fewer repercussions."
                , Footnote "Also, this includes people are unhappily monogamous, but believe that they are destined for monogamy and so refuse to question it."
                , Body " that polyamory is the best lifestyle for them, but I do want to explain some of the benefits of polyamory."
                ]
            , Paragraph
                [ Body "First, I need to introduce the mathematical notion of a multi-objective optimization problem "
                , Footnote "Don't fret, I promise there will be no greek letters."
                , Body ". An optimization problem is any problem where the aim is to find the minimum or maximum value of some function, for example, Google Maps solves the optimization problem that is finding the shortest route from where you are to where you are trying to go. It searches through many possible routes, and returns the fastest one. This is a single-objective optimization problem, because it is only concerned with one objective: time. Add in some of the other things Google Maps allows you to optimize for, like maximizing or minimizing freeway time, and you get a multi-objective problem. One of the distinctive features of multi-objective problems is that there may not be one unique best solution. With a single-objective problem, either one solution is best, or there are several tied. Imagine two different routes with the same projected time: Google can return either of them, and you won't care. But imagine two different routes, where one is faster but entirely through city roads, and one is slower but is much more scenic. It's impossible to say, definitively, that one solution is better than another "
                , Footnote "These solutions are part of the \"non-dominated set\". A solution that is better than another in every way \"dominates\" the worse point. These terms take on an entirely different meaning when you're talking about dating."
                , Body "."
                ]
            , Paragraph
                [ Body "The typical view of dating is that it is a single-objective optimization problem. Your goal is to find the person that is best for you to date. Under this view, monogamy makes sense, as there is in fact one unique optimal person to date. This is the general idea of soulmates, I think. And, if the single-objective assumption is true, this is a reasonable conclusion: you are only optimizing for one thing, so there is one unique optimum."
                ]
            , Paragraph
                [ Body "But I don't think that you can reduce the dating experience to just one thing. I like to dance with the people I'm dating, to rant about philosophy, to explore the beautiful parts of math and computer science, to talk about books, to have sex with them. These are all separate activities, and no single person is the best at all of them! When I am seeing multiple people concurrently, typically they are very different people. If I am seeing two people, and one is better in every way than the other, I would stop seeing the one that is  less of what I am looking for. But typically, people are amazing in very different ways, and as much as I enjoy the way that each person is, if I were limited to just one person, I wouldn't be able to get everything I want out of the relationship. We have multiple friends, because each of our friends brings out a different version of ourselves, and encourages us to pursue certain goals and accompanies us on certain adventures. It would be absurd to tell someone that one friend should be enough; it is equally absurd to tell someone that one romantic partner should be enough."
                ]
            ]
        , Model "Digital Oblivion"
            "digital-oblivion"
            Listed
            [ Paragraph
                [ Body "I once dated a girl who, whenever she would open her phone, would automatically, without realizing it, navigate to Instagram "
                , Footnote "I want to say that this relationship didn't last long, but it lasted around eight months. Being good in bed can make up for many personality flaws."
                , Body ". This was infuriating. Whenever she volunteered to find directions somewhere, there would be about a minute of time between her pulling our her phone and her actually starting to navigate. Whenever she got a text she had to reply to, it would also take a minute longer than it had to. Even, once, when putting on sex music, she paused to check Instagram. This one made me laugh. It was so clear that she wasn't consciously aware of her actions. It's hard to blame her, because Instagram develops its app intentionally to encourage this type of behavior. It's endemic of a bigger problem."
                ]
            , Paragraph
                [ Body "We're all so damn distracted. Wake up, go to work, come home, order delivery, watch Netflix, and repeat. People can live their entire lives, nominally successfully, holding down a full-time job and and apartment and all the trappings of the middle class, and never think. Maybe you have to do some simple problem-solving for your job, but I mean think about who you are and what you want. It's possible to spend a whole day without having time for introspection, jumping from distraction to distraction, binging "
                , Footnote "We've normalized \"binging\" TV shows, using the name for excessive consumption, and that scares me."
                , Body " whatever show on Netflix was most recently mentioned to you. If you make a passable salary, you don't even have to cook for yourself, you can get it ordered. You don't even have to interact with someone to ask them to make you food, just push a button on your phone and then pick up the food when it arrives."
                ]
            , Paragraph
                [ Body "Take a train, and look around: everyone is on their phones. Most of them are actively engaged in some media content, watching a video or listening to music. When I had an hour-and-a-half long commute "
                , Footnote "Which was terrible, and I would not recommend to anyone."
                , Body ", I tried to listen to music or podcasts to kill the time, but I eventually got bored. I started reading, or just sitting and watching the people and the landscape. But I noticed that I was almost the only person on the train not staring at some device. I felt estranged from society. It was like watching a Black Mirror episode, but no one else seemed worried. The "
                , Link { text = "average American household watches almost 8 hours of television a day", href = "https://www.theatlantic.com/technology/archive/2018/05/when-did-tv-watching-peak/561464/" }
                , Body "."
                ]
            , Paragraph
                [ Body "I've always been morbidly fascinated by global suicide statistics. Despite a better objective quality of life, people in developed countries kill themselves far more often. Why? Perhaps without time to really introspect, it's difficult to see what is really important, but I doubt it. In developed countries, nothing matters. Tomorrow, I could skip work and laze around and drink and smoke all day, and it wouldn't matter. I would show up the day after, and they would be concerned, but not fire me. Nothing I do tomorrow will matter significantly, to my life or to anyone else's. Not to say that nothing matters; I understand that over the next month, I will produce something useful and if I didn't show up to work for a month bad things would start happening to me. People in their 40s, if they're doing everything right, will have a pretty significant amount of money saved up for retirement. Enough money that they could afford to not work for a year or two or ten. But they go in to work nonetheless, the rational parts of their brain telling them that they ought to keep working towards a retirement that's twenty years away. That seems pretty tough. So we come home from work and scroll and watch and swipe until it's time to go to bed, then time for the next day's work. In developing countries, you need to go get water for yourself, or you will die of dehydration, there's not enough time to sit around thinking about whether life is worth living, because your base survival instincts are being triggered every day. In the developed world, we live unworried about where our next meal will come from, unafraid of freezing to death, or of dehydration. Cocooned in this safety net, it's hard to remember what the point of living is."
                ]
            , Paragraph
                [ Body "When I talked before about "
                , Link { text = "good self-harm", href = "./goodharm" }
                , Body ", I think I was really talking about triggering these survival instincts. Cold showers makes you worry about hypothermia, fasting makes you worry about food, and exercise makes you worry about whatever it is your body is being pushed to its limits for. Obviously, rationally, you know that there is nothing to be afraid of, but the subconscious knows. If the state of the modern world is any indication, we overvalue the rational part of the brain, and undervalue the subconscious. All of our best efforts to improve society have resulted in higher suicide rates "
                , Footnote "And, to be fair, lower rates of murder and death by disease and hunger. It's not all bad, it's just not working out for us, psychologically."
                , Body ". Clearly, modern society is missing something, and we shouldn't follow the prescribed path. Avoid digital oblivion, try to find some kind of analog awareness."
                ]
            ]
        , Model
            "Drinking and Writing"
            "drinking-and-writing"
            Listed
            [ Paragraph
                [ Body "I drink when I write "
                , Footnote "Which is a bad habit, when you're trying to write 50,000 words in November."
                , Body ". Sometimes, this means I drink alone, on weeknights. Typically, drinking alone on weeknights is considered a problem. I don't think that applies to me, however "
                , Footnote "Which is what any self-respecting alcoholic would say, but I have a website and a principled argument."
                , Body "."
                ]
            , Paragraph
                [ Body "The reason I frown on drinking alone, generally, is that alcohol makes you okay with not doing anything. Sitting around drunk, or high, you can consume entertainment mindlessly for hours "
                , Footnote "Or, at least, until you sober up."
                , Body ". This is bad, in my view of the world. I am highly suspect of choices made right now that make it easier to laze around later. I used to own only bluetooth speakers and headphones, which lagged a second or so behind what my computer thought was playing. This made watching TV quite difficult, as the audio lag was very noticeable and quite annoying. Listening to music was fine, and so I was able to listen to music as I worked, but I ended up not watching much TV. Later, I got a wired headset because I needed a mic, and this dramatically increased the amount of TV I watched. In this way, getting a better headset was bad for me. Similarly, most drinking is bad for the people doing the drinking, because they tend to do less interesting things with their lives while they are drunk "
                , Footnote "It's also bad for their livers."
                , Body "."
                ]
            , Paragraph
                [ Body "But can you call drinking and writing similarly wasteful? For me, drinking lets the words flow more easily, removes my inner censor, and dulls my hyperactive consciousness so that I don't tab out to a distraction after five minutes of writing. I think drinking might be a performance enhancing drug for writing. If so, if drinking allows me to write more than I would otherwise, it may be helping me do more interesting things with my life. This nullifies one of the principal arguments against drinking alone. It may or may not improve the writing I do produce "
                , Footnote "I am certain that most things I write while drunk are full of misspellings and sentences that violate basic rules of grammar, but I'm going to call that a \"conversational style\", instead of trying to defend it."
                , Body ", but it lets me spend more time writing. It has some value. I would rather drink, and write, than be sober, and fail to write."
                ]
            ]
        , Model
            "PayPal"
            "paypal"
            Listed
            [ Paragraph
                [ Body "I worked at PayPal for eight months, after my second year of college. It was the worst job I have ever had "
                , Footnote "Although I have mostly worked as a software engineer, so despite being the worst job I have ever had, it wasn't all that bad."
                , Body ". My official job title was \"Business Intelligence Engineer, Graduate Student\". The last two words there may be confusing, as I was an undergrad at the time. They were looking to hire a grad student, but the grad students that got interviews were all laughably bad "
                , Footnote "This is probably because Northeastern's ALIGN Master's program allows people with no prior experience to get a master's degree in CS in three years. These students had been taking CS classes for three semesters. I had been taking CS classes for four semesters. While time spent in classes isn't everything, it does matter."
                , Body ". They were so laughable that one of the people interviewing me paused me five minutes into the hour he had with me to tell me how much better I was than the other candidates "
                , Footnote "My ego doesn't need any more inflating, but this definitely inflated my ego. And so did having \"Graduate Student\" in my job title as a third-year."
                , Body "."
                ]
            , Paragraph
                [ Body "I was impressed by the interview "
                , Footnote "Pivotal's interview process is the best of any company, but PayPal's interview was okay too."
                , Body " process. They brought everyone in for an afternoon, and had a matrix of when each person would see each interviewer, so we all saw each person for the same amount of time, and were asked the same question. This is incredible, because it allows some objective measures to be recorded, and then those measures can be correlated in the future to job performance, to see which interview questions need to be changed."
                ]
            , Paragraph
                [ Body "The first part of the interview is a thirty-minute written coding challenge. But get this - the question is how to implement a client-server tic-tac-toe game, and they tell us that it's okay not to write any code. They just want a high-level design. I love this! You can't get a good grasp on someone's programming skills by having them write a new program for thirty minutes, but you can get a good grasp on someone's system design skills in that time. Maybe it's not the most important skill for the job, although it is important, but at least it is honestly measuable. I spend my thirty minutes writing, laugh at some of the questions that the people around me ask "
                , Footnote "For example, \"Is it okay to use Django?\". Clearly, this guy missed the part where they said to not write code. He was sweating heavily, but I didn't hold it against him, because he was clearly out of his league here."
                , Body ", and eventually the time is up. I grab my pencil, and I am shuffled to the opposite side of the office. What I didn't realize at the time is that they were hiring for two distinct positions: Software Engineer (Graduate Student) and Business Intelligence Engineer (Graduate Student). I thought they were just hiring software engineers, and some of them would end up on the business intelligence team. I was wrong."
                ]
            , Paragraph
                [ Body "The interviewees for the Business Intelligence Engineer position make it to the other side of the office. We are stationed in a large conference room, and called out for hour-long interviews, and told we'd have lunch off and another random hour off during the day. I am the only white person in the room, everyone else being interviewed is Indian. This will be important later."
                ]
            , Paragraph
                [ Body "The first interview asks me some basic SQL queries. I have no trouble here, as I have taken at this point three classes that covered SQL"
                , Footnote "Although the first two just expected me to know it, and by the third, I had taught myself most of SQL for independent reasons, so I only attended on the days that there were tests."
                , Body ". The second interview is more behavioral, about projects I've worked on before, and similar. Still no worries. The third interview was one of the best interviews I have ever been subject to. I was given a very vague prompt, and told to roleplay asking other people questions and drawing out system designs to answer it. The prompt was something about making a personnel management system where you can update birthdays and names and photos and such of all the people working at a company. My interviewer gives me some background information, some of which are clearly bogus (that the database password is in /etc/password), I call out the obviously false things, and start drawing up an API design. We go back and forth about the exact system requirements, security measurements. I bring up SQL injection "
                , Footnote "Interviewers love it when you bring up SQL injection."
                , Body ". Then that interview is over, and there is some fourth interview that I don't remember at all. "
                ]
            , Paragraph
                [ Body "At no point in the day did any of my interviewers ask me about the tic-tac-toe server that I spent thirty minutes designing. This should have been a red flag, but at the time, I thought it was just a miscommunication, but it turns out that the tic-tac-toe exercise was a major part of the interviews for the Software Engineers. They didn't really care about our system design skills, it turns out. But I was young and foolish. Their failure to examine my system design skills should have signaled to me that they wouldn't have me designing systems. But it didn't. I go home, taking the T, my mind racing. Because I am still mentally recapping the whole day, and because I was similarly distracted on the way in, I don't realize that the commute takes an hour each way. Long commutes are brutal, but I had never commuted more than fifteen minutes before "
                , Footnote "Not strictly true. I had a part-time job that had an hour-and-a-half commute, but I worked remotely almost all the time, so I didn't notice. And when I did go into the office, the CEO would typically pick me up from the nearest train station on the way in, and we would stay late drinking whisky (and good whiskey, at that, although it was wasted on my nineteen year old self), so I didn't mind the commute too much."
                , Body ", so I didn't realize how brutal long commutes are."
                ]
            , Paragraph
                [ Body "To make a long story short, they offered me the job, and I took it because I had no other offers and I wasn't sure I was going to get any "
                , Footnote "A false fear. The day after I accepted this job, I got offered a much cooler, but slightly lower-paying job. I would have much preferred the other job, I think. My primary reason for believing this is that many of the men there wore chelsea boots."
                , Body "."
                ]
            , Paragraph
                [ Body "The job description mentioned five projects that I would be working on during the eight months of my employment. The first one sounded quite interesting. There is this system that processes over $500,000 of transactions a day, sending reimbursements to gas stations, particularly those in Australia "
                , Footnote "In most countries, when you buy gas, the gas station will pre-auth your card for $100 or so. This ensures that you have enough money to pay for your tank of gas. In Australia, it is illegal to pre-auth for more than $1, so fraudsters would load up prepaid cards with $1.01, fill up a tank with gas, and drive away. "
                , Body ". This system ran at 4:00AM EST, to process this half of the world, and at 4:00AM in whatever timezone Australia is in, to process the other half of the world. And every day, between 4:00 and 5:00, it would fail, paging whoever was on call. This had been happening for three months."
                ]
            , Paragraph
                [ Body "Let that sink in. On my team, there were two other people, and one of the two had been paged at 4AM, every day, for three months. Not every business day: every day. Weekends too. This is horrific. My first goal was to fix this system. Well, my first goal was to avoid getting put on this on-call rotation from hell, but my first project was to fix this system. I ask why none of the engineers (keep in mind, this office had roughly sixty full-time software engineers) had fixed it. My boss "
                , Footnote "Not manager. Not team lead. She was my boss."
                , Body " tells me that she has no clue, but that she thinks that they don't care, because their performance is measured strictly in terms of the number of tickets that they complete, and they don't know Python "
                , Footnote "This part was shocking to me. I knew it was a Java shop, but I didn't know that no one even knew Python! It was like they had all been living under rocks. This is because they all had been living under rocks."
                , Body ". This was when I had my first revelation about this job. I was only hired here to fix a political problem, namely the engineers refusing the help the business intelligence team."
                ]
            , Paragraph
                [ Body "I could easily have fixed the system in an hour. If the system was developed by reasonable people. But, unfortunately, it was not."
                ]
            , Paragraph
                [ Body "My first attempt to find the bug started out fine. I cloned the git repo with the code, and starting poking around. This code was horrific. Just absolutely horrific. It made me physically uneasy to read this code. There were strings, with data being interpolated in, which were then executed as SQL. I guess this is why they were happy I knew about SQL injection, in the interview. The SQL strings, instead of being multiline strings "
                , Footnote "If you're going to do SQL injection, at least make the queries easy to read! Assholes."
                , Body ", trailed on and on interminably, often three or four hundred characters long. And the entire project is full of these SQL strings, as the entire configuration for this tool is stored in a database "
                , Footnote "This particular branch of PayPal used Oracle Database, a tool which is incredibly powerful and incredibly painful to use. My favorite fact about Oracle Database is that it does not support booleans. They recommend using a column with a single character, that is either 'y' or 'n'. Or was it 'Y' and 'N'? Or 'T' and 'F'?"
                , Body ". And not in a nice, relational table, as you would expect. I could get behind that! Storing your database in a relational database, in a nice tabular format, is the sort of polite thing you do when you expect someone else to have to maintain your code later. But no. It was not that. Instead, it was a key-value store, implemented as a database table with two columns "
                , Footnote "This hurt me."
                , Body ". Even worse, not all of the keys were required, some of the keys being set required other keys to be set, sometimes one key being set to true meant that another key was present "
                , Footnote "This hurt me more. You can just have the optional key, and check for it existing. You don't need to store a 'true' separately from the data, the mere existence of the data signals that the data is there. Idiots."
                , Body "."
                ]
            , Paragraph
                [ Body "At this point, I gave up and stormed off. Not out of the office, just over to the engineering side. I found the person who knew the most about this code, which was still not very much. And I asked them -\u{00A0}who wrote this? They told me that contractors had written it four years ago. And that it was so bad that the contractors were all fired when they submitted it for code review. AND THEN PAYPAL PUT IT IN PRODUCTION."
                ]
            , Paragraph
                [ Body "This was unfortunate. I get access to the machines that actually run this code, and I start poking around on them. First of all, there is no deployment automation on these machines. You get a new version of the code out by scp'ing files into the right place. I cry a little, and then move on. Looking at the files that are currently on the machine, I notice that they are significantly different than what is in git. Turns out, people on my team had been ssh'ing into these production machines, using nano to edit the code, and then not checking it in to git."
                ]
            , Paragraph
                [ Body "I would make fun of them for this, but I think being paged at 4AM every other week for three months is sufficient punishment."
                ]
            , Paragraph
                [ Body "Anyway, most of my refactoring was now useless, as the code I had been working on was not the code in production. This set me back a full day's work. The people on my team knew that what was in git was not identical to what was on the production machines, but didn't tell me. I was a little upset, but what really baffled me is that they would be so useless. Every extra day that I spent working on this project was another day of them being woken up at 4AM."
                ]
            , Paragraph
                [ Body "I eventually find the bug. Someone has added a single row to one of the four database tables that this tool looks at, but hasn't added matching tables to the other tables. So it read that row, tried to process it, and then choked. This became almost immediately obvious once I was given access to the machines that ran the tool, because the error logs were relatively informative, and I could add more easily."
                ]
            , Paragraph
                [ Body "Once I've fixed this bug "
                , Footnote "My team is less grateful than I thought they would be. They really do not value their sleep highly."
                , Body ", I realize that there is no more engineering work to do. I settle in for seven and a half months of moderately-paid drudgery. There are some highlights, though. A coworker microwaves a hardboiled egg, and then it explodes, and she has to take a month off to recover from the burn wounds "
                , Footnote "This one is sad, but also hilarious and informative. Don't microwave eggs, apparently."
                , Body ". I realize that no cares, and so I start showing up at 10:30 and leaving at 4:00, and taking two or three hours off to walk around during work. I am drinking my body weight in alcohol every weekend during this time period, but for indepedent reasons, I think. Overall, I learned a little Python, got to play around with cloud computing on someone else's dime, read an incredible amount during my commute, was paid pretty well for the actual number of hours that I worked, and learned about some questions I should be asking during interviews."
                ]
            ]
        , Model "PayPal's Bullshit Diversity Metrics"
            "paypal-bullshit"
            Listed
            [ Paragraph
                [ Body "About halfway through my job at PayPal, I got the most infuriating email of my life "
                , Footnote "This email explicitly said it was an internal communication, not to be shared outside PayPal. Oops."
                , Body ". It was the yearly report on the PayPal office located in Newton, Massachusetts. It started off with some basic corporate speak about how we're all doing great and so on. Then, it congratulated us for being the most diverse office PayPal had in America."
                ]
            , Paragraph
                [ Body "This was somewhat confusing to me. I looked around my office, and diversity is not at all what I saw. The engineering side of the office was almost entirely Indian men, all between 25 and 40. There were a few white men there, and some Indian women, but for the most part, the engineering department entirely conformed to a slightly outdated stereotype of engineers. The sales team was entirely white men. The marketing team, however, was entirely white women. Again, a perfect conformance to stereotype. The designers were almost all women, and the male designers were all gay. All of the designers were better-dressed than the rest of the office "
                , Footnote "Although sharp dressers aren't a protected class, so this one doesn't matter legally."
                , Body ". The executives were all white men "
                , Footnote "Five for five. All of the executives were white men, not just many of them."
                , Body ". HR was entirely female. You get the idea."
                ]
            , Paragraph
                [ Body "The email, however, went on to clarify what PayPal meant by diversity. It was the percentage of people that were not white men. By this metric, we were killing it. Most of our engineers were Indian men. This was diversity! Never mind that each business unit was almost entirely homogeneous, or that the exceptions to the homogeneity were still stereotype-compliant. We were diverse because we conformed to stereotypes!"
                ]
            , Paragraph
                [ Body "Diversity is important. But any email that says your office is \"74% diverse\" is bullshit and should be treated as such." ]
            ]
        , Model "The Best Time of Your Life"
            "best-time-of-your-life"
            Listed
            [ Paragraph
                [ Body "People keep telling me that the best time of my life will be during college, and then being confused when I want to stick around for a PhD. But this essay isn't about that. This essay is about timescales."
                ]
            , Paragraph
                [ Body "The best hours of your life will be on drugs. Maybe they won't be the most fulfilling hours of your life. The most fulfilling moment of my life to date is when I had a revelation and invented an entirely new type of model for evolutionary game theory, thus proving someone else wrong."
                , Footnote "I want to say that it was the best moment because I invented a new thought for the first time, but I can't deny that proving someone else wrong added to the satisfaction"
                , Body ". Becoming the first person in the world to have ever thought a specific thought was incredible. But it took two months of hard work and reading for me to get to that point. And it doesn't compare to a revelation-filled trip on a lasciviously temperate summer day with a beautiful girl. Discovering this new model is a contender for the best sober experience I've ever had, but it hardly compares to May 11, 2019 . If you're thinking on a timescale of hours, the way to have the best hour is to take drugs."
                ]
            , Paragraph
                [ Body "But on a timescale of days or weeks, it's completely different. The best weeks I've had have been focused, intellectual, productive. Prolific. What weeks do you look back on approvingly? The weeks where you accomplish something, the weeks where you hit some major milestone in your life - those are the best weeks. A week is short enough that you can lock yourself in your room and work and work and work tirelessly, without suffering too greatly. The best weeks of my life have been dead sober "
                , Footnote "I've spent most of my adult life as a college undergrad, so maybe dead sober except for parties on Friday and Saturday."
                , Body ". But those weeks were incredible! There were weeks when I aced homework assignments, was unexplainably productive in art or code or writing, and weeks where I read paper after paper learning about some new field. The best weeks of your life will be sober, focused deeply on some subject. A week is too long to spend under the influence, or at least too long to feel good after spending under the influence."
                ]
            , Paragraph
                [ Body "But a month is too long to spend sober "
                , Footnote "Maybe there's something to unpack here."
                , Body ". The best months of my life have been spent alternating between hard work during the week and wild experiences on the weekends. I didn't pick the 5 day/2 day split, but it feels just about right. The best months of your life will be spent in balance between work and play. The best months on my life to date were May and June 2019, where I was working the hardest and producing the most I ever have, while also partying the second hardest I ever have "
                , Footnote "The summer after my sophomore year, Northeastern randomly assigned me to a massive apartment they meant for four people, and gave me only one roommate, who wanted to throw parties even more than me."
                , Body ". Trying to emulate the circumstances that lead to the best months of your life is a reasonable aim, but emulating anything on a shorter timescale will lead you astray."
                ]
            ]
        , Model
            "Decaf Coffee and Caffeine Pills"
            "decaf-coffee-and-caffeine-pills"
            Listed
            [ Paragraph
                [ Body "I drink decaf coffee. I take caffeine pills. Most people either are completely decaf, or take caffeine pills to supplement coffee (if they like the taste) or replace it (if they don't). But I'm worried about caffeinated coffee. I really, really enjoy the taste of coffee, and the normal version has drugs in it. When I drink coffee, I cannot separate in my mind the taste and the drug, and so I cannot know if I am choosing to drink coffee for the taste or for the drug."
                ]
            , Paragraph
                [ Body "I am uneasy about any action I take that results in the accidental or ancillary consumption of drugs. I like drugs, particularly caffeine, but I want to moderate and be aware of my use. If I take a caffeine pill once per day, at the same time, I can ensure that I'm having a fixed amount of caffeine. If I leave it up to chance, I take wildly varying amounts of this drug, based on how much free time I have to make coffee, whether I hang out at a coffeeshop that day, and all the other factors that affect the amount of coffee I drink. "
                ]
            , Paragraph
                [ Body "Fortunately, decaf coffee does exist, and while it tastes slightly worse than normal coffee (undecaf?), it is still eminently drinkable. So I can separate my desire for this good-tasting drink for my desire from the nominally productivity-enhancing (but probably mostly sleep-deprivation-fighting) drug. Then, I can track the two desires separately. If I notice myself taking more caffeine then usual, that is a signal about the state I am in, not a signal about what types of beverage I want to drink. My drug consumption is deliberate."
                ]
            ]
        , Model
            "Impossibility and Deadlines"
            "impossibility-and-deadlines"
            Listed
            [ Paragraph
                [ Body "Recently, I missed a deadline. I was applying for a scholarship "
                , Footnote "It was really an award for competence in STEM research disguised as a scholarship. The award would be worth more than the scholarship itself."
                , Body ", and this scholarship had one official date that applications were due. But Northeastern had another due date, three months before, which I overlooked in the materials that they sent me. They sent me a reminder email one day before Northeastern's deadline. I didn't have enough time to complete an application in time "
                , Footnote "Among other requirements, I had to write a four-page description of research I had done and write several many-hundred word essays."
                , Body ", so I figured I may as well ask for an extension. I send an email to the organizers, and ask for more time. The response was one of the most infuriating emails I have ever gotten."
                ]
            , Paragraph
                [ Body """"Dear Julian:", it begins innocently """
                , Footnote "If a bit formally. Who uses colons after the greeting in an email?"
                , Body """, "Thank you for being in touch". You're welcome! Another cordial sentence, and then: "In response to your question, has there been something else happening in your life that precluded your handing the materials in on time?" """
                , Footnote "This is where a colon should be used: \"In response to your question\" is just begging for it."
                , Body ". I am shocked, but keep reading. The rest of the email explains how students in my situation have made the deadline before "
                , Footnote "Obviously other people have made the deadline. The point of my email was that I had not."
                , Body ". And it concludes: \"Is something else going on that made it impossible for you to meet the deadline?\""
                ]
            , Paragraph
                [ Body "This really sets me off. In some ways, it is obvious that it would have been possible for me to meet the deadline. I could have known about it months ago, and if I had read the entire ten-page booklet they gave me about the scholarship, I might have noticed it in time. If I knew the deadline, I would have worked on it. There is a note in my journal, now crossed out, to start working on this scholarship, for the week that came two months before the deadline. Two months would have been enough time. Instead, in some ways, I had one day's notice. So, yes, Dr. Whoever "
                , Footnote "A brief aside: it is a bad sign when someone feels the need to put a Dr in their email signature."
                , Body ", it was possible that I could have made the deadline."
                ]
            , Paragraph
                [ Body "But I didn't. And I sent the email asking for an extension on the day of the deadline. The response came after the deadline had ended. So in some ways, it was impossible for me to have made the deadline: I did not, and at this point in time, as far as I understand time, it could not have been any other way. Now that we are in the present, a day after the deadline, with my materials still unsubmitted, it is impossible that I can submit them on time. No matter how hard I try today, and tomorrow, and the day after, I cannot submit these materials on time. It is impossible for the me who you are emailing to meet the deadline.  "
                ]
            , Paragraph
                [ Body "And I dislike the wording of that first question: \"Has there been something else happening in your life that precluded your handing the materials in on time?\". Of course there has! I was busy writing and reading and drawing and drinking and fucking "
                , Footnote "Maybe a bold assertion, but I'm going to assume that I have more sex than someone who signs off their email as a \"Dr.\"."
                , Body ". I didn't see the deadline, until you emailed me a day before. Clearly, they're asking if there has been a tragedy that has prevented me from making the deadline. Is that the only valid excuse? If someone close to me died, or fell ill, would that let me have an extension on this scholarship? What if I was just incapacitated with fear about some world news? Now, I have to plead my case, point out that my life has been quite shitty recently, in order to get a deadline. This disgusts me. I am doing ok, and it's strange to think that doing worse might have been better for me, in this little way. If there's a deadline, there ought to be a deadline, and if there isn't, don't make me act out some misery porn so that you can feel good about yourself for extending it."
                ]
            , Paragraph
                [ Body "I sent back an email with some of this sentiment: \"Dr. Whosit,\", I begin "
                , Footnote "Note: comma, not colon."
                , Body ", \"Many other things have happened in my life.\", and then a description of some of those "
                , Footnote "Although I left drinking and fucking out from the list above."
                , Body ", and then \"No great tragedy has befallen me recently, if that's what you're asking. Nothing made it impossible to meet the deadline.\", I say, honestly."
                ]
            , Paragraph
                [ Body "And they reply, thanking me for my candor: \"My colleagues and I are grateful for your honest reply. Unfortunately, this means that in fairness to our other candidates that we cannot accept your late application at this time.\". Apparently, extending the deadline for me is fair to the other candidates if someone I know has died, and unfair otherwise."
                ]
            , Paragraph
                [ Body "With good planning, my scholarship would have been done a month before the deadline, and even a major tragedy happening a month before the deadline would not have stopped me "
                , Footnote "Okay, depends on how major. With a medium-to-large tragedy, though, I could probably manage to click Submit."
                , Body ". Impossibility is a funny word, and the way that this doctor used it seems to rob me of some agency."
                ]
            ]
        , Model
            "Refactor Long Papers"
            "refactor-long-papers"
            Listed
            [ Paragraph
                [ Body "Whenever I read a long philosophy paper, let's say over twenty pages, I feel somewhat disconcerted when I reach the end. If your paper is twenty pages long, you probably make one core argument, and then try to prove each premise, and then conclude. For shorter papers, this can work well, because you only have enough room to make arguments that directly support premises to your main argument. For longer papers, it's easy to go astray, and try to demonstrate that A is true, because you've just set up that A implies B, B implies C, and C implies a premise of your main argument. This is great and all, but when I get to the end of your argument for A, I have forgotten the link to the main argument, and for that matter, probably forgotten the main argument itself."
                ]
            , Paragraph
                [ Body "Maybe this is my fault. After all, I am part of a generation notorious for its short attention spans. But I don't think so, or at least, I don't think my generation is to blame. I'm used to the programming world. I can understand programs far longer than 20 pages, because in the programming world, a function definition that lasts up more than 30 lines is considered a bad idea "
                , Footnote "A citation should go here, but I figure you either already know this is true, or you don't program enough to fight me on this."
                , Body ". The general idea is that each function should be understandable in its entirety. If you have something complicated to express, move it to its own function, and call it here. Each unique idea can take up one line, then. Maybe you do something arcane, call a function you've defined just to help this function be easier to understand. But still, I can read this function, and even if I know there is something else to understand, I can infer what it does from its name and the type of its inputs and outputs. I can understand how this function wires other things together, and then separately understand the things it wires together. Good code is code that is understandable in isolation."
                ]
            , Paragraph
                [ Body "Papers often do not have this fractal quality. You can't really understand the main argument without understanding the side arguments at the same time, and the side arguments seem irrelevant and inconsequential if you miss the link to the main argument. Maybe this is because the tools we "
                , Footnote "Programmers."
                , Body " use to edit text let you navigate to a function's definition with a single keyboard shortcut. You can't do this with papers, at least not easily, because papers are represented as a single line, a series of words. Sure, there are section titles, and direct or indirect references to other sections of the paper, but the fundamental data is a single series of words. A program, on the other hand, is fundamentally a tree, and so it is easey to navigate as one."
                ]
            , Paragraph
                [ Body "It would be really neat if someone made a tool for writing papers in some tree-like format. You have one main argument, and this is the root of the tree. Then, you have supporting arguments, which are on a second layer. Then, those arguments have supporting arguments themselves: the third layer. And so on, to whatever depth you wanted to go to. This would allow papers with a fractal structure of arguments for premises, and arguments for presmises in those arguments, and so on to be more comprehensible. Programmers realized a while ago that a series of words was not the best data representation for programs, so we created tools for treating our programs as words. Programming is unique, in that the tools programmers use are created by programmers, so when a programming problem can be solved by software, someone writes software to solve that problem. Philosophers, on the other hand, don't write software, and so this need goes unfulfilled."
                ]
            , Paragraph
                [ Body "I am a programmer, but I'm not going to make this tool. That sounds like a lot of work, and I'm quite busy complaining about how this tool doesn't exist yet."
                ]
            ]
        , Model
            "Coffee"
            "making-coffee"
            Listed
            [ Plain "A bleary morning, you rub the sleep out of your eyes. Your vision isn't blurry, but it is unfocused. You stumble out of your bedroom, and flip a switch. The newfound, harsh light requires a lot more from your eyes, but helps you come to grips with the fact that today has started."
            , Plain "You make coffee."
            , Plain "Perhaps you have an espresso machine, and perhaps you grind your own beans. Take out the beans, put them in a grinder, and watch the portafilter fill with grounds as you turn and turn and turn the crank, and wake and wake and wake up. Tamp the grounds down into the portafilter, cautiously now - you don't want to miss slightly, the tamper misaligned, the grounds spilling on your counter, creating a new task that must be this morning. After all, it would be no good to leave the grounds there until you come back from your office, for a counter left unclean would haunt you during the day."
            , Plain "The portafilter is ready, so you swivel it into place in the machine, press a button, and now you wait. Hot water at high pressure forces its way through the grounds, and into your cup, promising a bitter black cure for bleariness. Perhaps you froth milk, for you are a latte-drinker, so you pour the milk into the pitcher, heat the steam nozzle, and then work precisely, exactingly, holding the pitcher at the right angle and the right depth to froth the milk without a mishap. Many mishaps are possible, in the making of coffee. Some merely deprive you of coffee, or require you to clean up and try again, some could be more explosively disastrous."
            , Plain "But all goes well, you've done this a thousand times, literally if you've been making coffee every day for three years, figuratively if you are new to the idea but still well-experience, falsely if you have just purchased this espresso maker. The milk is ready, and the espresso is ready, and you yourself are ready to combine the two, so you do. The kitchen smells of espresso and milk, and, the whole world forgotten, you take the first sip of your latte, the first truly relaxed moment of the day, the reward for your efforts."
            , Plain "It is okay. You've done better."
            , Plain "They pour coffee grounds and water into a machine, push a button, and minutes later, a beep tells them their coffee is okay. They pour some milk in, take a sip. It is okay. But it is a very different okay than yours."
            ]
        , Model
            "Gunshots"
            "gunshots"
            Listed
            [ Paragraph
                [ Body "The summer after my freshman year, I lived in Roxbury, the ghetto of Boston. I was working full-time as an intern, making $16 an hour. This was good money, in some senses, because it was significantly above minimum wage, and bad money, in many senses, because I was living in an expensive city and also because I was technically an independent contractor, which meant a 15% tax on top of regular income tax."
                ]
            , Paragraph
                [ Body "I found a place to stay for the summer. Dorms were around $1200 a month "
                , Footnote "Absurd, for a room that was essentially as small as the one I'm about to describe."
                , Body ", so I found an apartment. Rent was $450 a month. This is absurdly low, for a prominent American city. But let me tell you a bit about this apartment first. My room fit a twin bed, with about two feet of space on two sides of the bed. Two feet at the foot, two feet on one side. The room, then must have been around 5.5 by 9.5 feet. That comes out to 52 square feet "
                , Footnote "Which is, technically, illegal. Rented bedrooms in Boston have a lower bound on square footage, and this room was significantly under that limit."
                , Body "."
                ]
            , Paragraph
                [ Body "The rest of the apartment wasn't much better "
                , Footnote "Although the other rooms were signficantly bigger."
                , Body ". The kitchen had an oven and a microwave, although the oven itself didn't work for the whole summer. The burners on the stove worked, in that they emitted gas, but the spark didn't work, so you had to light it yourself. For the first half of that summer, there was a regular lighter in the kitchen, but after the fiftieth time I burnt my knuckles lighting the stove, I bought a long-necked lighter. The kitchen was covered in a layer of grime, coated thick enough that you could feel the oil on the cabinets above the stove when you touched them. After opening the cabinets, if I wasn't careful, my knuckles would end up greasy."
                ]
            , Paragraph
                [ Body "I lived across from Section 8 housing. One day, stumbling home drunk "
                , Footnote "I never appreciated the sense of fear that many women feel while walking home alone and drunk until this day. Walking around Boston proper, I never felt afraid, but walking back to Roxbury, through the back alleys and dimly-lit streets and drug dens that line the path back to my house, I felt the fear."
                , Body "I met a man on the street who I had seen before. He always sat on his front porch, in one of those folding camping chairs, and would say hi to me in the mornings. He notices that I am drunk, and points it out. I had some alcohol in my backpack "
                , Footnote "I brought alcohol to one of my friends over at BU, and we got far too drunk together, wandering around the campus listening to music. I miss summer nights."
                , Body ", and offer to share "
                , Footnote "I had a small amount of vodka on me, maybe $4 worth, so I figure that the story that will come out of this will be worth the $4."
                , Body ". He invites me inside, with a thick accent that I can't place. I'm drunk, and figure this guy is harmless, so I say \"sure\", and walk in. His apartment is crowded oppressively, with knickknacks and empty liquor bottles and takeout boxes and trash taking up most of the available flat surfaces. He gets out three glasses, I pour vodka into them, he pours some mixer in. The third glass is explained a few seconds later, when a woman walks in. This man is quite small, she is easily six inches taller than him; I have no clue of their relationship. We drink and talk about Boston and Roxbury and life and travelling "
                , Footnote "He was from Cuba, or something."
                , Body ". After maybe fifteen minutes or so, I finish my drink. I think about what I should do next, and then suddenly I remember something my roommates have said. They claim that people who sit out on the street like this guy are taking notes for criminals about when houses are empty and when people leave for work, and then they sell this information. I am overtaken by fear that he has told someone I'm there, and I'm going to get robbed "
                , Footnote "I have my laptop in my backpack, so I am concerned about robbery. Normally, I carry  small amounts of cash, so as long as the robbers don't take my driver's license, I wouldn't really mind being robbed that much."
                , Body ". So, I tell him that I should head home "
                , Footnote "Although the normal almost-excuse \"It'll take a while to get home\" doesn't apply here, because I live down the street, so I had to be quite blunt about my desire to leave."
                , Body ". He asks for my number, I think \"why not?\", and give it to him. I head home. The next day, while I'm at work, he calls me and asks if he can borrow five dollars. "
                ]
            , Paragraph
                [ Body "My job that summer was a pretty sweet gig, except for the small paycheck. I was working at a startup with ten employees. The copmany made websites for college career centers "
                , Footnote "Or, if you look at their marketing materials, platforms for college career centers."
                , Body ". I mostly just did whatever I thought would be helpful. There were some assigned tasks, like answering the scattering of customer questions we got, but for the most part, they let me roam free. One day, the sales guy asked me to help him send emails through HubSpot. I obliged, and spent the better part of a morning helping him, doing manual repetitive tasks, and then in the afternoon I wandered off to try and automate the task. I managed to automate sending emails to people, in a script that ran off an Excel spreadsheet. I felt good about myself, earned lots of respect from the people at the company, and improved my resume. That was one of the better weeks of my life. There was another intern that summer, a small Asian girl who was incredible enthusiastic about everything. She would often throw both fists in the air triumphantly while screaming \"Yes!\", about mundane things like someone saying that a sales call went well, her accomplishing some task, or whenever the staff of the WeWork we worked at brought us snacks."
                ]
            , Paragraph
                [ Body "And one day, walking out of work with my coworker, it sarted to rain. This coworker, who was the CEO, offered me a ride home, telling me that \"Cotton in the rain is a bad look\", because I was wearing a peacoat. I declined, and biked home as always, savoring the splash and spray as I biked, the smell of fresh rain on the ground, and the muted grey hues of the empty city. But I appreciated the offer."
                ]
            , Paragraph
                [ Body "The first Friday after I moved in, I came home from work, and settled in at the kitchen counter "
                , Footnote "No room for a desk in my room."
                , Body " to read Effective Java "
                , Footnote "I am quite prolific when lonely."
                , Body ". I get through a few pages, and then hear what I think is a firework going off. But then I hear a scream, a few more \"fireworks\", the scream ending, and a car pulling away. Some of my roommates step out of their rooms. I am distraught, pulling up police scanners to see if what I just heard was in fact a shooting. They tell me that there are shootings semi-regularly here. The police scanner informs me that someone is dead, and that bullets entered an apartment at 131 Eustis Street. I lived at 133 Eustis Street. Bullets entered the apartment next to mine, and a man was killed outside. I take some deep breaths, remind myself that I'm still safe, and keep reading. Eventually, I go to bed, and eventually, I fall asleep."
                ]
            , Paragraph
                [ Body "The next day, I walk in to work, and there is blood on the street."
                ]
            ]
        , Model
            "A Light Dusting"
            "a-light-dusting"
            Listed
            [ Paragraph
                [ Body "I like the snow. Waking up to a transformed world, everything looking different from how it looks in your memory of yesterday, brings out a childlike joy. The snow brings out a contrast between the vertical and the horizontal, for the vertical is left unchanged, and the horizontal is covered, white, puffy."
                ]
            , Paragraph
                [ Body "I like stepping in the fresh snow, watching it compact underfoot, feeling agency over my environment. Normally, in a city, the ground doesn't care at all when you walk on it, but after a snowstorm, each step creates change in the world. Even when other people have walked where you are walking, there is the noise of water pushed out from underfoot by the forces of gravity and your stride, each step providing a certain sense of satisfaction, a sense of affecting change in the world. Compacted snow squeaks underfoot, a cute noise for a footstep. The ice, when it forms, is scary, I lose my balance and slide and skitter and grab at whatever is nearby, but when I recover from the gyroscopic moment, I smile. The recovery is fun, even if the initial fall, or what seemed as though it would be a fall, makes your heart pound loud and your breath catch."
                ]
            , Paragraph
                [ Body "I like to exhale, and watch the steam of my breath dissipate slowly as I walk. I don't smoke, so this is the only time I see my exhalations as physical objects. I remember the sixth grade, walking to the bus station, inhaling deeply and then blowing out all the air in my lungs, prancing around and pretending I was a dragon with a breath of steam."
                ]
            , Paragraph
                [ Body "I like to feel my cheeks turning red. The wind blows savagely at my face, the tip of my nose goes numb, but not frostbitically. I feel the inflammation of my cheeks, my body working overdrive to preserve my extremities. The flush in my cheeks feels warm, like cupping a mug of tea in my hands, despite the bitter winter, the subfreezing mode of the air. What I feel is not the external world, but my body's reaction to it."
                ]
            , Paragraph
                [ Body "I like the imprints my boots leave on the ground. I look back, as I walk, not out of fear but just watching the deep, geometrical imprints of my boots. The texture on the bottom, providing grip, is full of thirty and sixty degree angles, lines becoming diamonds that turn back into lines, a beautiful insignia printed upon the ground with each step. I look back, and marvel at the clearness of my passing, the obviousness of my footsteps, the subtlety of the my boot's scream of individuality. On snow that is particularly imprintable, you can read the name of the company that manufactured them."
                ]
            , Paragraph
                [ Body "I like seeing other people react to the cold weather. The factions are apparent. There are the businesspeople, socially coerced into a particular outfit, or uniform, wearing some coat over a suit or a dress or a pantsuit. Their typical uniform, with additions to account for the weather. They all seem cold, their cheeks red, their scarves ornamental, not functional. And there are the people who understand the cold weather, the people's whose routine requires that they spend some time outside, long enough to come to terms with today. They wear beanies, or hoods, to protect their ears, scarves pulled up around their faces like TV gangsters wearing bandanas, to protect their face. Large coats, almost comically puffy, keep the snow out and the body heat in. Anyone wearing a wool coat in a snowstorm knows that they will only have to walk from a car to their office. The people wearing parkas have a signficant hike ahead of them."
                ]
            , Paragraph
                [ Body "I like the very end of walks in the snow. Stepping inside, rubbing your hands together to bring them back to normal operating temperatures, them warning your nose and your ears with those same hands. The relief I feel, relief that the world is no longer attacking, that I am somewhere safe and warm. The deep joy of a mug of some hot beverage, each sip warming you from the inside out, the fire in your belly a celebration of victory over the hostile environs."
                ]
            , Paragraph
                [ Body "I like seeing the city dressed like me. All white and black and grey, muted but bold. My body, clad in black and white, mirrors the contrast within the city, the clean lines of snow mounded atop tables and benches and chairs mirroring the goal of my coats and shirts and boots. I see sharp corners on buildings, highlighted by the mounds of snow pooled around their bases, the red and brown texture of brick cutting over to vast expanses of white. I see my black parka, zipper texturing the transition between darkness and light. The bright white of my shirt peeking out from behind the zipper and scarf, making itself heard."
                ]
            , Paragraph
                [ Body "And I like the smug superiority I feel over the people who can't face the minor hardship of a walk in the snow."
                ]
            ]
        , Model
            "Grand Theft Sunflower"
            "grand-theft-sunflower"
            Listed
            [ Paragraph
                [ Body "I am an art thief."
                ]
            , Paragraph
                [ Body "During my sophomore year of college, I stole some art from Boston's Museum of Fine Art. It wasn't anything incredibly important, though. They had an temporary installation from Takashi Murakami, a Japanese artist who does a lot of cartoon drawings. He like sunflowers, and he created one  work that was a series of sunflowers that were planted in the ground in front of the MFA. Imagine a flat piece of metal, roughly sunflower-shaped, with a large smiling face. Like the image below, but entirely yellow. "
                , Image { href = "../assets/murakami_sunflower.jpg" }
                ]
            , Paragraph
                [ Body "A friend and I walked up to the MFA's lawn, late at night, and unscrewed one sunflower from the ground. There were dozens of sunflowers left on the lawn, so we figured this one wouldn't be missed. We draped a blanket over the sunflower, because the bright yellow smiling sunflower was somewhat conspicious, and carried it over to Ryder Hall. This was one of the buildings that overlooked the main open green space at Northeastern. We made it past the security guard at the entrace, up the stairs, and onto the fourth floor. There were some classrooms on the fourth floor that had windows that would open wide enough to let you step out onto the roof of the third floor. We clamber out, bringing the sunflower with us, and finally are standing on this roof."
                ]
            , Paragraph
                [ Body "We revel for a moment. This roof has an awesomeview, letting you look into some dorms where some shenanigans are always happening, and across campus into Boston's skyline. It's nice. We take the sunflower out to the edge of the roof, and stick it into an open pipe right on the edge, staring down at Centennial Commons."
                ]
            , Paragraph
                [ Body "In the middle of a Boston winter, seeing a sunflower smiling down at you while you walk to class is pretty nice. Trudging through snow, people crowding around me, knowing that I have lots of homework I have not done, seeing a metal sunflower on the fourth floor of a building, remembering the night that I stole it and put it there, felt pretty damn good."
                ]
            , Paragraph
                [ Body "I am somewhat upset that stealing the sunflower never got me laid."
                ]
            , Paragraph
                [ Body "But it did make that winter more bearable. One day, it dissapeared, so I assumed that campus police found out about it and removed it. But that was not the case. At a friend's acquantance's party, a few months later, I see a similar sunflower leaning against one of their walls. I ask where they got it, and they tell me they took it from the roof of Ryder. I am somewhat upset about this. I was trying to be a Robin-Hood-esque figure, stealing art from the museums and putting it where the people would see it. These assholes took it just to decorate their home."
                ]
            , Paragraph
                [ Body "I went back to the MFA to take another sunflower to decorate Ryder's roof with. But at this point, they had security guards. Apparently, someone had stolen over a dozen sunflowers, and was trying to sell them on eBay. I didn't know this at the time, though, so I went back in to take another one, and a man stopped me. I ended up in relatively little trouble, because I was white and young and clearly a college student. The attempted theft was allegedly a felony, because the sunflowers sold for many thousands of dollars, because they were art. They banned me from the MFA."
                ]
            , Paragraph
                [ Body "Attempted art theft is the coolest way to get banned from a museum."
                ]
            ]
        , Model
            "Quick vs Smart"
            "quick-vs-smart"
            Listed
            [ Paragraph
                [ Body "School rewards being quick to get a working understanding of concepts. In a math class, the aim is not to truly understand calculus, but to understand the series of symbol-manipulations that you have to carry out in order to transform a problem description into the right number "
                , Footnote "At least, this is the immediate goal. I had to get an intuitive understanding of linear algebra in order to do anything in my machine learning class. But in my linear algebra class, I mostly learned how to do the right symbol manipulations."
                , Body ". Even in English classes, there is a formula to be followed for essays. In elementary and middle and high school "
                , Footnote "I don't know if kids in elementary school write essays, but I'm including them anyway. They have English classes, probably, and so I'm assuming they have to do some putatively creative work."
                , Body ", you write the five-paragraph essay. In college, the format changes, but there is still just a single format expected "
                , Footnote "Some philosophy papers I've written have been less formulaic, but mostly because I wanted them to be. The standard format would have worked."
                , Body ". So what people call \"smart\" in school is primarily speed of understanding. The school system rewards being able to pick something up, at a shallow level, immediately."
                ]
            , Paragraph
                [ Body " But this is not the only type of intelligence, and it is not the same type of intelligence that is required to be exceptional at one thing. To be really good at something, you need to build an intuition for what you're working on, to know why the symbol manipulations do what they do. From what I've seen, the reason people struggle so much with calculus is that they don't understand algebra. They understand how to \"turn the crank\", go through the steps, and end up with the correct answer, for the problems in algebra where they know what symbol manipulations are required to produce the answer. To solve new problems, or old problems that are disguised as new problems, you need to actually understand what you're doing. In middle and high school, I remember finding shortcuts for many algebriac steps, understanding that I could go from step 1 straight to step 4 and not have to bother with all the intermediate nonsense. Teachers got mad at me for this, because I wasn't manipulating the symbols in the way they wanted me to."
                ]
            , Paragraph
                [ Body "The incentive structures in schools lead to memorizing the set of steps to solve particular problems, and not putting any effort into understanding the material beyond what is required to execute that set of steps. While there are long-term benefits to having a deeper understanding, for example, how understanding algebra will help you later, in calculus, those benefits are far away and not guaranteed. So it is completely understandable that people would not really try to learn algebra deeply in the hope that one day it will help them in calculus. What we need is a school system that directly and immediately rewards deep understanding."
                ]
            , Paragraph
                [ Body "Many interventions would change these incentive structures. For example, giving people new types of problems on exams would require that people can apply what they've learned in new situations. People would complain that this is unfair, because you haven't taught them the exact material that is on the test. This is to be expected; anything that creates more work for students will naturally get some pushback. Students are incentivized to complain about anything that will lower their grade. Students with a deeper understanding would probably do better in later classes, and if this is true, the complaints might stop "
                , Footnote "They almost certainly won't. But they might, and I want to believe that they will."
                , Body "."
                ]
            , Paragraph
                [ Body "Schools have accidentally equated quickness and intelligence, and should change their incentive structures to reward deeper learning. But they probably won't, because students would complain, and classes would become more difficult."
                ]
            ]
        , Model
            "Personal Website Analytics"
            "personal-website-analytics"
            Listed
            [ Paragraph
                [ Body "This website doesn't have any analytics or tracking. If you're reading this right now "
                , Footnote "And you are, based on the fact that you're reading this."
                , Body ", I have no clue. I don't know where you're from, what link referred you here, how long you stayed, how much you read. I know nothing about you, or your presence on this website."
                ]
            , Paragraph
                [ Body "Personal website shouldn't have any user tracking. Adding user tracking would take me some non-trivial amount of time "
                , Footnote "Because I'm an idiot and wrote this whole website by hand in Elm, instead of using a static site generator or a hosted blogging website."
                , Body ", and remove some privacy from visitors to my website, so there would have to be a pretty significant upside to justify it. And I don't think there is one."
                ]
            , Paragraph
                [ Body "It would be nice to know how many people visit this website, I guess. Tracking how many people click in to each essay from the essays page would help me refine my clickbait. But I don't want to write clickbait here, I want to write for the sake of writing. I'm writing primarily for myself, and publishing that writing on the internet keeps me honest and gives me a reason to make sure it's good. I don't care about maximizing clicks, clicks get me nothing. I want to publish interesting enough ideas and stories that people actually reach out to me. So, I can measure the number of emails I get from people visiting the site "
                , Footnote "To date, one."
                , Body ", and that will be a better measure of my goals here than clicks would be. If people are actually thinking about what I write, and reading it deeply, they might email me, people who click and skim and leave aren't interesting to me."
                ]
            , Paragraph
                [ Body "And I'm pretty sure that viewership is low. Certainly, not high enough to warrant figuring out how to get advertising on this website. So, keeping track of viewership will probably not inspire me. But by intentionally not recording pageviews, I know that people may be looking. If I write something and publish it to an audience that could be any size, I feel some obligation to make it halfway decent. In the same way that open-sourcing some code requires good practices by the people writing the code, publishing your writing puts a lower bound on quality. I don't want other people to think that I write garbage, so I have to ensure that the things I write are not in fact garbage. If I kept track of viewership, I might realize that zero people have ever seen anything I've written, and I would feel no shame if a website with no viewers proudly displayed some garbage I had written."
                ]
            , Paragraph
                [ Body "If I were running a business, I would keep track of pageviews and conversions and all that. But here, I just want to write, and have people read it. I don't need to know how many."
                ]
            ]
        , Model
            "Office Moments"
            "office-moments"
            Listed
            [ Paragraph
                [ Body "Walking down a hallway, passing someone else, you feel forced to smile, they feel forced to smile. You see the void in their eyes, and can only imagine they see the void in yours. The smiles, the grimaces, linger on your face only until just after you pass each other, and then both faces fall slack. "
                ]
            , Paragraph
                [ Body "Walking by a conference room, you see someone sitting alone. You make eye contact. As she turns her head back to her laptop, you initiate the mandatory smile, she sees it too late to turn her head back, and so she just smiles at her laptop. And then stops smiling, once she realizes that the smile isn't being directed at anyone. But your smile, at the sight of her sitting alone in a conference room, is heartfelt. "
                ]
            , Paragraph
                [ Body "Walking down a hallway, someone else coming the other way, you nod your head, but they aren't looking, so you nod again when they are looking, or you thought they would see this one but they didn't, so you nod again. Off to the side of the hallway, someone sees it all, and nods at you. Finally, acknowledgement."
                ]
            , Paragraph
                [ Body "You try to scan something. It scans blank. You flip the paper over in the scanner, and try again. Blank one more time. You flip it back to the original layout, and the scan isn't blank, but it is upside down. You flip the paper, and it scans upside down again. Guess what happens when you flip it back to the original layout. Also, one of your coworkers has been silently watching you bungle since attempt two, and audibly laughing since attempt four. The paperwork scans. You go to send it, and you see an email telling you that there is a new form that has to be filled out, and the old one doesn't matter anymore."
                ]
            , Paragraph
                [ Body "You go to a lunch and learn about \"apis\". You know what an API is, but you want to learn more. The lunch and learn is actually about \"apis mellifica\", the latin name for the common honey bee."
                ]
            , Paragraph
                [ Body "Your teammates drink with you at lunch. Back at the office, you settle in for a few more hours of work. You are pairing with someone who drank more than you. They are completely competent. You fall out of your chair."
                ]
            , Paragraph
                [ Body "You print a paper. You try to staple your paper, but there are no staples in any of the staples. One of your coworkers laughs about printing reminding him of the '90s. You find the staplers. A week later, you print another paper, and the same coworker makes the same joke. You try to staple your paper, but there are no staplers, although there are boxes upon boxes of staples."
                ]
            , Paragraph
                [ Body "You play a game of ping pong, with your team. You play another game of ping pong, with other people. You play a third game of ping ping, with your team again. Someone asks if you ever work. You ask them if they want to play ping pong. They say yes."
                ]
            ]
        , Model
            "She Didn't Let Me Fuck Other Girls, So I Broke Up With Her"
            "sorry"
            NotListed
            [ Paragraph
                [ Body "My first college girlfriend was my first serious excursion into polyamory. The summer before college, I was seeing two girls at the same time "
                , Footnote "They were co-directing a musical, a fact I only learned when I went to the musical to support one of them and then saw them both at the afterparty. I ran away. They both knew I was seeing the other person, but it was very strange to interact with them both at the same time."
                , Body ", but the inherent brevity of any summer-after-high-school relationship meant that everyone knew this was just temporary, and we were all fine with open relationships."
                ]
            , Paragraph
                [ Body "I met this girl before the first day of classes. There was free ice cream somewhere, and I was standing in line with a friend. Two cute girls "
                , Footnote "Maybe more accurately, one hot girl and one cute girl. "
                , Body " cut us in line, and so I started hassling them about it. We banter back and forth, exchange phone numbers "
                , Footnote "The beginning of freshman year is the time when people are the most unflinchingly social."
                , Body "."
                ]
            , Paragraph
                [ Body "I'm going to skip over most of my freshman self's attempts at flirting, because they were real bad. But the important bit is, I ended up dating this girl "
                , Footnote "The one I described as hot, earlier, not the cute one. (This is me bragging.)"
                , Body ". She struggled to make friends, so she joined a sorority "
                , Footnote "A common story, and one that makes you wonder how fraternities and sororities are so socially successful, when most of their members are people who couldn't make friends naturally."
                , Body ". This was a problem for our relationship, because I find group membership quite unnerving  "
                , Footnote "I was raised as an atheist, and I generalized my distaste for American Christianity to all social groups."
                , Body ". Whenever she would tell me about the cultish things that sororities do "
                , Footnote "They have rituals where they wear all white and recite chants and sing songs together, illuminated only by candlelight. You can't deny that this is at least a little cultish."
                , Body ", I would point out that those things were cultish, and this would cause a minor tiff. Generally, this tiff would last until we had sex, which probably reveals a lot of other things that should be unpacked, but I'm going to leave all that aside."
                ]
            , Paragraph
                [ Body "She and I were in an open relationship. This was clear from day one "
                , Footnote "Okay, day four. I got around to defining the relationship eventually, but on days one through three I was mostly just happy that someone wanted to make out with me."
                , Body " of our relationship. We had a good time together, but it was clear that I was getting more play than she was "
                , Footnote "When I'm drunk, I make out with the nearest attractive person who's willing to make out with me. So I'm going to have to give up either monogamy or drinking. If you know me, it's not difficult to guess which I'm planning to keep."
                , Body ". In fact, she didn't want me to sleep around. She first told me that we could keep doing what we were doing if and only if "
                , Footnote "She didn't say if and only if. She was pretty bad at formal logic, actually, I ended up helping her with her homework for \"Principles of Argumentation\", a class which somehow managed to satisfy her math requirement. She was a Communications major."
                , Body " I didn't have sex with anyone else. Everything else was fair game, but sex was reserved with her. I agreed to this, and we continued on with our lives."
                ]
            , Paragraph
                [ Body "Most of my closest friends at the time were female, which didn't help her latent jealousy at all. One day, cuddling in her twin-sized freshman dorm bed "
                , Footnote "I swear, colleges give twin-sized beds to students to try to prevent them from having sex. It doesn't work, but sometimes sheets slide off the slick plastic mattresses and you get rugburn. This disincentives sex slightly, but not enough to stop teenagers from fucking."
                , Body ", she mentions this, and her worries. I probably responded with something glib, and she tells me that she wants to have lunch with me at least four times a week."
                ]
            , Paragraph
                [ Body "This is insane. Maybe slightly less insane than it sounds at first, because we're both first-years, so we eat in the dining halls anyway. If we both worked, if we would have to go out and meet up at some restaurant and pay for a lunch, this would be completely insane. But because I was eating lunch in a dining hall anyway, eating with her was no big problem. The real problem was that she was imposing quotas on me. One week, I met with a group to work on a project on Monday and Tuesday, submitted it on Tuesday night. Friday, drunk in her room, she tells me that she's unhappy that we only got lunch three times that week."
                ]
            , Paragraph
                [ Body "That could have been the end of the relationship, but the sex was really good. "
                ]
            , Paragraph
                [ Body "A few months later, I'm at a party with my friends. She calls me, and I ignore the first call. I felt like I had been neglecting my friends for people I was having sex with, and wanted to spend a night with them. Also, a loud frat basement is a bad place to take a call. So I let it ring. She calls again. I let it ring. She calls a third time. At this point, I am worried "
                , Footnote "Although more worried about \"us\" than about her."
                , Body ", so I pick up. She's having a panic attack, feels sick, etcetera. I leave to go help her. She is freaking out until the moment that I show up. This is when I realize the real issue. She was worried that I was hooking up with someone at this party. We chat for an hour or so, and then fall asleep."
                ]
            , Paragraph
                [ Body "The next weekend finds us at a frat party together, with my friends. She says something that sets me off, although I don't remember what. I pull her upstairs, holding her hand, dragging her behind me. A man loitering on the staircase, shirt mostly unbuttoned, calls out after us \"Oh, y'all getting down, huh?\". No. We go up to a bedroom with two beds, and sit on opposite beds, staring at each other "
                , Footnote "Please ponder this for a moment: this frat had a bedroom with unused beds open to the general public. What were they trying to encourage, you think?"
                , Body ". At my urging, we crawl out the window, and sit on the roof. We talk. And we talk. And at the end of this talking, we have broken up."
                ]
            , Paragraph
                [ Body "A few months later, the summer after freshman year, most of my friends head home, but I found an "
                , Link { text = "internship in Boston", href = "./gunshots" }
                , Body ", so I stick around. This particular ex lives near Boston, though, so we hang out quite often. We remind each other of our last year at Northeastern "
                , Footnote "I guess, the good parts, the parts other than us breaking up."
                , Body ". One day, we're sitting in her back lawn, passing a wine bottle back and forth, and talking about our plans for our futures. She is snapchatting with her new boyfriend, and at one point, he doesn't respond for thirty minutes. She starts telling me how she is worried that he's hooking up with someone at the bar he is at, because normally he's very responsive."
                ]
            , Paragraph
                [ Body "My ex is, at this point, sitting alone with me, drinking. She is drinking with her ex, alone but for the two of us. This would cause some worrying in her boyfriend, I imagine "
                , Footnote "Leaving aside the fact that we exchanged massages earlier that day. It was her idea."
                , Body ". Nonetheless, she is worried about her boyfriend making out with someone at a bar. I think I broke her."
                ]
            ]
        , Model
            "Passive and Active Media Consumption"
            "passive-active-consumption"
            Listed
            [ Paragraph
                [ Body "People binge-watch Netflix shows, but don't binge-read books. When I was in middle school, I read religiously "
                , Footnote "I read lots of sci-fi and atheist nonfiction, so it's perhaps more accurate to say that I read anti-religiously."
                , Body ". Even then, when I went through two or three books a week, I wouldn't say I binge-read. When I watched TV back then, though, I would sit and watch until I physically had to move - I had to use the bathroom, or I was hungry, or thirsty."
                ]
            , Paragraph
                [ Body "Active media consumption requires constant effort to continue the consumption. Books are like this - if you look away for a moment and look back, nothing has happened, because you haven't read. When you finish a page, you have to physically flip it. This contrasts with passive media consumption, like watching TV. In TV, absolutely nothing is required from you, the show will keep playing if you look away, and it requires more effort to stop than to keep watching. Imagine if you had to push a button on a controller once per minute, to keep watching a TV show, the equivalent of having to flip a page. It would probably make you watch less TV."
                ]
            , Paragraph
                [ Body "Every person I know wishes they watched less TV and read more "
                , Footnote "Except for some film majors, who wish they watched less shitty TV and watched more good movies."
                , Body ". But we don't. It is difficult to pick up a book, to find somewhere to read, to sit down for an hour. Reading requires silence, or at least a lack of interruption. It's no fun to be interrupted every three pages by someone talking to you, because you have to hold more context in your head. Watching TV, however, particularly TV shows, missing a few minutes won't be the end of the world. My siblings, when watching Netflix, will run to the backroom and back during the show intro, sometimes missing some of the beginning of the show. Recall that Netflix allows you to skip the intro, and that you can pause it afterwards. Why aren't they skipping the intro, pausing it, and coming back later? No one wants to walk into the bathroom with a deadline. But what they want even less is to turn their passive media into active media, to be required to find the remote and press a button."
                ]
            , Paragraph
                [ Body "And active media consumption lends itself more easily to distraction. People who are addicted to phone use can set their phones aside while gripped in the moment of a movie, because any glance away from the screen displaying the movie to the screen of their phone would mean missing a potentially critical moment. But when reading, glancing away and checking your phone doesn't interrupt anything, and so people who are in the habit of checking their phone often can continue that habit while reading a book. This makes them read slower, and enjoy reading less. This in turn leads those people to spend more time checking their phones or otherwise distracted. A vicious cycle."
                ]
            , Paragraph
                [ Body "Active media consumption requires you to opt back in constantly; the default is not consuming. Passive media consumption doesn't require anything; the default is consuming, and you have to actively opt out to make it stop. "
                ]
            ]
        , Model
            "Mandatory Class Attendance"
            "mandatory-class-attendance"
            Listed
            [ Paragraph
                [ Body "Grading students on attendance is a sign of bad incentive structures. Certainly, you don't actually care about students showing up for the sake of them showing up. You must believe that them showing up will be good for them.  So, ideally, you could just measure those other things."
                ]
            , Paragraph
                [ Body "If you think that your lectures do a great job of explaining concepts, test your students on knowledge of those concepts. If you think your lectures do a good job of explaining certain skills or how to solve certain problems, test your students on their ability to demonstrate those skills or solve those problems. If your lectures are actually useful in imparting skills or knowledge, then your tests will reflect attendance grades."
                ]
            , Paragraph
                [ Body "If your lectures are useless, though, students will stop attending. If students aren't attending your lecture, it is because they don't see the value in these lectures. By imposing lower grades on students that don't show up, you will of course raise attendance. But attendance of lectures isn't the real end goal, it's a proxy for having students be excited about your lectures because the lectures teach students well. Attendance is a good signal, and you can use it as a proxy for the utility of your lectures, but it is not the true end goal."
                ]
            , Paragraph
                [ Body "Participation grades are slightly different. Participation grades reward showing up and talking, so they have value if a goal of your class is to teach students how to discuss the concepts introduced in the class. But if the end goal really is a deep understanding of the material, and the discussions just exist to help facilitate that, don't reward participation in discussions for its own sake. You will reward it indirectly, if your tests work right, because students who participate in discussions will have better understanding and therefore get better grades. So, if participating in discussions really does help students understand, then, generally speaking, students will participate for self-interested reasons "
                , Footnote "Some people are shy, and don't participate in classes often for that reason. Participation grades may draw out their participation, but presumably, they can understand the material on their own. If they can, that's great, if not, they will be encouraged to participate in discussions because it will help them understand."
                , Body ". "
                ]
            , Plain "If you have to add an artificial reward to encourage some action, it's a pretty good sign that that action is not naturally rewarding."
            ]
        , Model
            "Realistic Rationality"
            "realistic-rationality"
            NotListed
            [ Paragraph
                [ Body "I try to act rationally, that is, to act in ways that bring about the worlds I enjoy "
                , Footnote "Although I don't try to believe rationally. I have many inconsistent personal beliefs that I will cling to until the day that I die."
                , Body ". This is the \"self-interested\" type of rationality, the type of rationality that aims at maximizing the things you value. Trying to think rationally often prevents you from acting rationally."
                ]
            , Paragraph
                [ Body "We humans have many built-in, irrational desires. There is no reason derivable from pure logic for you to enjoy the sight of nature, or the taste of food, or sex. Perhaps there are reasons derivable from models of evolutionary processes that show that these desires are inherent, and guaranteed in any evolutionary process. But that just means that all evolved creatures will have these traits. Some sentient being that was brought into existence by fiat would not be guaranteed to have them."
                ]
            , Paragraph
                [ Body "But they are important to take into consideration when making decisions. Maybe it's irrational to care more about money today than tomorrow, to be willing to spend an hour working to earn some amount of money, but not be willing to spend an equal amount of time doing equally boring paperwork to get that same amount of money. If you want something, it is rational to pursue it, even if you can't formulate a logical reason to want whatever it is. It's not irrational to do what will make you happy. Your desires don't have to be grounded in logical reasoning."
                ]
            , Paragraph
                [ Body "Our minds are physically embodied in our brains "
                , Footnote "Or supervene on our brains, or whatever. There's been a lot of time spent thinking about this, and no definitive conclusions, but everyone agrees that the physical state of our brains affects the state of our minds."
                , Body ". So, doing things that affect the physical embodiment of your brain can be rational. There's no logical reason that walking should improve the way you feel, but it does. There's no logical reason that caffeine makes you think more clearly, but it does. So doing those things can be rational, because of the way they impact your brain, due to accidents of evolution."
                ]
            ]
        , Model
            "(Physical) Vulnerability is Sexy"
            "physical-vulnerability-is-sexy"
            Listed
            [ Paragraph
                [ Body "Doing power poses makes people more confident and attractive. Why would that be? I'm going to wildly speculate."
                ]
            , Paragraph
                [ Body "Power poses are expressions of physical vulnerability. Imagine someone sitting at a desk, legs kicked up, arm dangling over a chair. This is a power pose, to be sure, and also a very very bad position to start a fight in. If someone were to physically assault you, and you were sitting in that position, you would be in some trouble. Compare this to poses that express low confidence. For example, sitting quietly, head tilted down, arms folded in your lap. This pose is only a few small movements away from the Muay Thai fighting stance. Wallflowers have their backs to the wall, while people socializing in the middle of a party are quite physically vulnerable."
                ]
            , Paragraph
                [ Body "So, power poses are expressions of physical vulnerability. Why would that be attractive? My assumption is that, in the past, people who are physically vulnerable in public either died, or were so strong and well-respected as a fighter that people would leave them alone. It's like staking your life on the outcome of a game of chess, where you start without a queen. It makes people assume that you are much better at chess than everyone else around you. Power poses probably worked similarly."
                ]
            , Paragraph
                [ Body "This may also be why hugging and confidence are so correlated. People who hug are exposing themselves to all kinds of violence, and while in modern America it is unlikely that hugging someone will result in your death, in the evolutionary context, it was probably much more likely. If you hug someone, you are putting your life in their hands, or at least giving them a major leg up "
                , Footnote "Or arm around."
                , Body "  in any fight they start."
                ]
            , Paragraph
                [ Body "Power poses are expressions of physical vulnerability, which means they signal strength and respectedness, which are attractive."
                ]
            ]
        , Model
            "Writer's Block"
            "writers-block"
            NotListed
            [ Paragraph
                [ Body "Writer's block isn't the inability to write. It is simple to write: you just string words along one after another. If you just want to hit a certain word count, it is quite simple. Just repeat yourself. Or, if you're feeling contrarian, repeat yourself. And if that doesn't work for you, repeat yourself."
                ]
            , Paragraph
                [ Body "Writer's block is the inability to write something you are satisfied with. The aim isn't just to hit a particular word count "
                , Footnote "I am writing this for NaNoWriMo, which ostensibly is about writing 50,000 words in November. But I want them to be good words. I want to write something interesting. So repeating myself, although it has contributed several words to this essay, must be limited to where it has dramatic effect."
                , Body ". The aim is to write something you like. And that is quite difficult. It's easy to look at something you've written and know how much you like it. You can, theoretically, use this ability to produce good content by writing a lot and then throwing out everything that is bad. Write enough, and eventually you'll end up with something you think is good. But that's a recipe for carpal tunnel, not success."
                ]
            , Paragraph
                [ Body "When writing something you plan to publish, as with public speaking, there is the fear of rejection. What if people don't like this? What if I am mocked for presenting this idea? Those are things to be afraid of, but generally the benefits of writing outweigh the costs."
                ]
            ]
        , Model
            "Elm"
            "elm"
            Listed
            [ Paragraph
                [ Body "I really like the Elm programming language. In fact, this whole website was written in Elm! And I want to talk about that experience (which was good), and the decision to use Elm for this website (which was probably bad)."
                ]
            , Paragraph
                [ Body "Before writing this website, I felt very incompetent as a web developer. I thought it wasn't for me, that there was something about me that prevented me from doing web development. This was partially true and partially false. JavaScript isn't for me, as both imperative languages with a focus on mutability and GUI development are types of programming I am very bad at. But the web itself is fine "
                , Footnote "This is not, strictly speaking, true. The web is a flaming pile of garbage, and it's amazing that any website works at all."
                , Body ". I can create websites, and understand that type of programming, it's just not my favorite."
                ]
            , Paragraph
                [ Body "Writing code in Elm, I only had to deal with one unknown, only the web nonsense. I didn't have to deal with web nonsense at the same time as a programming paradigm that doesn't mesh with the way that I think. I was still offroading, in a sense, but at least I wasn't having to learn how to drive stick while offroading for the first time "
                , Footnote "I learned how to drive in a car with a stick shift. It was difficult enough in an empty parking lot."
                , Body ". Elm is a simple language, minimal without losing much power. Based on Haskell, Elm is just a pure functional core and nothing else. Its compiler sympathizes with your errors, giving cute and helpful messages explaining where you went wrong. The language guide explains the foreign way of thinking required to program in Elm as if it is normal, and convinces you. Elm is empty of extraneity, and it is beautiful."
                ]
            , Paragraph
                [ Body "The web is still my least favorite place to program. The web wasn't designed for programmer productivity, because it wasn't designed. It works well enough, most of the time, and the fact that you're reading this attests to that "
                , Footnote "Although I've spent hours wrangling with GCP to make http work. As you're reading this, it's possible that its not over https. Making websites work is hard, please have sympathy."
                , Body ". But I spend most of my time "
                , Footnote "Most of my time that I spent programming at least, but it's possible that it's most of my time."
                , Body "in languages that have been designed to make my life easy, for platforms that succeeded because they were easy to develop for. The contrast is stark."
                ]
            , Paragraph
                [ Body "But Elm made me realize I could do it. I unlearned my helplessness. I no longer recoil from the mere mention of web programming. I'm not good at it, not yet, but at least now I know that I could be. "
                ]
            ]
        , Model
            "Capitalism and Servility"
            "capitalism-servility"
            Listed
            [ Paragraph
                [ Body "My office has lunch catered once a week "
                , Footnote "And breakfast every day, but that's not relevant to this tory."
                , Body ". Normally, the caterers bring in a bunch of precooked food, set up the serving trays and plates and such, hang out for an hour, and then leave. I feel totally comfortable with this."
                ]
            , Paragraph
                [ Body "One day, everything about the lunch felt a bit off. The caterers normally wear normal clothing that happens to be all black, but today they were wearing blazers. That wasn't completely unorthodox, I would understand if their uniform changed "
                , Footnote "Although the idea of uniforms does seem strange. The interface I have with caterers is entirely through the food. They set food out and I eat it. What they wear doesn't affect me at all."
                , Body ". They were also slightly more obsequious, slightly more snappy to respond to trays of food being emptied and apologizing for the tray running empty. This was confusing, because the tray of food running out was not their fault, and in fact meant that the food looked appetizing. There was one truly unnerving aspect of that lunch. One of the caterers was constantly circling all the tables in the dining area, picking up and throwing away everyone's plate when they were done. Many things can be done to improve service, but throwing away your plate takes only a few seconds, and you have to walk by trash cans to leave the dining area anyway. Clearing our plates wasn't a helpful act, in that it didn't make our lives noticeably easier. It was a servile act, signaling that the caterer's time was worth less than ours. "
                ]
            , Paragraph
                [ Body "This was, strictly speaking, true. The people eating the lunch were software engineers, who got paid a lot more per hour than the caterer's. Nonetheless, I felt icky. It's one thing to have someone do work that helps you, and another thing to have someone do work where the main point is to make you feel valued. I like being self-sufficient, in many ways, and I don't like imposing on others for things that would be just as easy for me to do by myself. I value the janitorial staff, and wouldn't want to occasionally have to mop the floors of my office, but I can walk my own plate to a trash can when I'm done eating. As long as someone else empties the trash can later."
                ]
            , Paragraph
                [ Body "Also, I wasn't done with the plate. I was planning on going back for seconds and reusing the plate. When the caterer took my plate, I actually protested about this. She told me to get a new one. Which is fine, of course, I can get a new disposable plate. But I was planning on using the other one! Sometimes, I form internal plans or understandings about the future, and having other people try to be helpful can disrupt that."
                ]
            , Paragraph
                [ Body "I also feel icky when waiters at fancy restaurants put napkins on people's laps for them. I am here to eat good food. But other people must be there to feel elite, because if everyone felt the way I did, waiters wouldn't put napkins on people's laps for them."
                ]
            , Paragraph
                [ Body "Anyway, that's why I think I wouldn't enjoy having sex with a prostitute. "
                ]
            ]
        , Model
            "Steve Jobs Is An Underrated Fashion Icon"
            "jobs-fashion"
            Listed
            [ Paragraph
                [ Body "Steve Jobs "
                , Footnote "Who must be rolling in his grave after some of Apple's recent design decisions."
                , Body " dressed like shit. But nonetheless, he is an underrated fashion icon."
                ]
            , Paragraph
                [ Body "Let me first substantiate my claim that he dressed like shit. He wore the same thing every day, and that thing was distinctive. If you're going to dress the same way every day, at least wear something nondescript. You can pull off a grey t-shirt and blue jeans for a week without causing a fuss, but wearing a turtleneck and dad sneakers is such a look that you really ought to only wear it once per month "
                , Footnote "Really, you should never wear this outfit, but I think Steve had already decided it was doing to happen. I'll let him have it some of the time."
                , Body ". Also: black turtleneck, blue jeans? No thanks. Black turtleneck with black jeans I can respect, or even a light-wash jean. But no, Steve Jobs wouldn't do something that had been done before, so he sullied the subtle formality of a black turtleneck with the rest of his outfit. It's like he spent all of his fashion innovation budget on his top, and then gave up on his bottom half. "
                ]
            , Paragraph
                [ Body "If you wear a black turtleneck today "
                , Footnote "In case it wasn't clear, I wear black turtlenecks sometimes. Really, mine is a dark grey, but it's close enough."
                , Body ", everyone will call you Steve Jobs. It's funny the first time "
                , Footnote "It's not actually funny the first time. The first time I hear it in a day I don't feel any compulsion to kill whoever is calling me Steve Jobs. So it's at least less murder-inducing the first time."
                , Body ". But everyone has closely associated this particular clothing item with a person. No one else in the popular mind today has a clothing item to tightly tied to their personal brand. Steve Jobs doesn't just wear a black turtleneck, you can't think of the idea of a black turtleneck without thinking of him. And this is why he's an underrated fashion icon. "
                ]
            ]
        , Model
            "Optimizing Life"
            "optimizing-life"
            Listed
            [ Paragraph
                [ Body "In How to Do Nothing, Jenny Odell says that life is \"more than an instrument and therefore something that cannot be optimized\" "
                , Footnote "I haven’t read this book, so I’m arguing just against this quote, not against the book itself."
                , Body "I disagree, at least partially."
                ]
            , Paragraph
                [ Body "While life as a whole cannot be optimized, and trying to maximize your happiness is likely to make you less happy, there are things that can be optimized. For example, when doing some drudgery, if I can find a better way to complete the drudgery, my life will probably be the better for it. When I say drudgery, I mean uninteresting repetitive toil."
                ]
            , Paragraph
                [ Body "But some things that appear to be drudgery are not. For example, I believe that walking is wonderful for the soul. While walking takes a lot of time, and biking or taking a bus or driving is faster, spending less time on an activity is not always a good thing. An extreme case is going to the gym: I spent lots of time there, but that time is not something to be optimized away. Although, if I could get an equivalent increase in both physical strength "
                , Footnote "Or, being more honest about my motivations for going to the gym, attractiveness."
                , Body "and mental clarity from spending less time in the gym, I might prefer that. It’s nice to spend less time doing things you don’t value intrinsically. Anything you do strictly for instrumental value you can optimize away safely, anything with side effects you cannot."
                ]
            , Paragraph
                [ Body "There are some cases where spending less time doing something causes a problem in and of itself. For example, reading books. If I were to try to read faster, I would not get the chance to really see what I was reading, to ponder the taste of each sentence in the books I’m reading and pick up on the tasting notes I would otherwise miss. I like spending time noticing the little quirks of a sentence, and thinking about what making a sentence I really like so good, and imagining good ideas coming into the mind of the authors. So those things, I certainly can’t optimize. Reading faster means reading less closely, and so trying to improve my life by reading faster is counter-productive."
                ]
            ]
        , Model
            "Injury, Suffering, etc."
            "injury-suffering-etc"
            Listed
            [ Paragraph
                [ Body "It was a commute like any other"
                , Footnote "Of course, anyone writing that something is \"like any other\" is surely about to tell you how this was is unlike the others."
                , Body ". I walked into my office, ogling the more attractive strangers"
                , Footnote "Any girl wearing Chelsea boots gets my undivided attention for a few seconds."
                , Body ". Walking across the Millenium bridge "
                , Footnote "A bridge in Denver, colloquially known as the Millenial bridge."
                , Body ", or rather, walking down the stairs on the park side of the bridge, I slipped and fell."
                ]
            , Paragraph
                [ Body "I fall a lot "
                , Footnote "I have a hoodie that says \"Good at Falling\". It's referencing an album by the Japanese House, but it seems more personal than that."
                , Body ". But this fall was particularly bad. I fell backwards "
                , Footnote "Thank god, as much as I believe in one. Falling forwards down the stairs would have been much worse."
                , Body ", landing on my backpack. As I fell, I reflexively threw my hand out. Unfortunately "
                , Footnote "For me, although I suppose fortunately for some cyclists."
                , Body ", this bridge happened to have a ramp to push bikes on. This ramp was essentially several 2-by-4's resting on the stairs. This meant that there were some nice, triangle-shaped pinch points on the bridge. As I threw my hand out, breaking my fall, I also start sliding down the stairs. Then, I stop moving, and looking to my side, I notice my finger caught in one of these triangles. I pull my finger out, and look at it for a little while: I see bone. I pull out my phone  "
                , Footnote "With my left hand, because my right hand is somewhat busy bleeding."
                , Body ", and I dial 911 "
                , Footnote "In hindsight, I didn't need immediate medical assistance. But when you see your own bones, it's reasonable to call an ambulance."
                , Body ". I wait outside my office, holding paper towels on the wound, and bleeding through the paper towels. I say hi to at least a few of my coworkers, as they walk in. Being severely injured is a good way to gauge your friendships with people: some of my coworkers ignored me standing in front of the building, some commiserated, and some offered to drive me to a hospital."
                ]
            , Paragraph
                [ Body "Eventually the paramedics show up "
                , Footnote "These motherfuckers took thirty minutes to show up. I was fine with waiting, while I was still hopped up on adrenaline. But ten minutes in, when the adrenaline wore off and the pain started to kick in, I started to question the notion of ambulances that take a half-hour to arrive."
                , Body ". They clean the wound, wrap some gauze around my finger, and tell me that I'll need to get stitches. I hop in an Lyft to an urgent care center."
                ]
            , Paragraph
                [ Body "I wait in the urgent care lobby for about five minutes, and then I'm called in. My urgent care doctor was a pro: without making a big deal out of it, he stuck out his left hand, and we shook left hands. I tried to comment on this, but he was busy poking and prodding at my hand."
                ]
            , Paragraph
                [ Body "He sent me in for an X-ray, and then we looked at the X-rays together. I had a fractured bone. Back in the room, he poked at my finger, and told me he couldn't see my tendon. This was a problem."
                ]
            , Paragraph
                [ Body "He tells me there are two options: either one of the hand surgeons he just texted calls him back, or -- and then he's cut off by his phone ringing. He picks up: \"Hi!... How are your kids? How's Anna?... That's good to hear. You free two weekends from now? Want to golf? "
                , Footnote "I am somewhat disturbed by this exchange. It's weird to think of doctors as real humans, as opposed to just machines that perform surgeries. In the same way that becoming a software engineer gave me a deep-seated mistrust of all software, realizing that doctors are real humans made me much more afraid of all medical procedures."
                , Body "... Alright, I'll pencil you in. Anyway, you working today? I have a patient who really ought to see you.\" This phone conversation resulted in me getting to a surgeon the same day, but was still somewhat unsettling."
                ]
            , Paragraph
                [ Body "Anyway, I get surgery or whatever. I have spent too much time talking about the injury, and not enough on what was supposed to be the big takeaway."
                ]
            , Paragraph
                [ Body "I get home from the surgery on Friday night. You're not supposed to drink after surgery "
                , Footnote "Not that this stopped me, on future nights, but day-of, I gave up alcohol."
                , Body ", so I cancelled my plans for the night, stayed in, and mused. I wanted to figure out how I felt about this injury."
                ]
            , Paragraph
                [ Body "I realized this: I had two choices. I could either complain about the injury, the negligence embodied in Denver's anti-snow efforts "
                , Footnote "I'm suing the city, but I'm not mad about it."
                , Body ", or I could be thankful that I didn't suffer any permanent injury, or break any bones "
                , Footnote "I fractured a bone, but compared to the severed tendon, it's no big deal."
                , Body ". I chose the latter. Fix your frame of reference to the sentence \"I slipped on ice at the top of a staircase.\", and you'll realize I got a pretty good deal. I'm not happy about being injured, and all things considered I would rather have ten working fingers. But given that I fell at the top of a staircase, going down to 90% of my fingers working for a few months is a small price to pay. Things turned out alright."
                ]
            , Paragraph
                [ Body "More generally, once the injury happened, I had two choices: be happy about it, or be unhappy about it "
                , Footnote "Maybe there was a spectrum of choices, between the two. Regardless, I had to pick where on the happy-unhappy scale I lay."
                , Body ". Given that I was already injured, being unhappy wouldn't help. I wish I could say that I rationally decided that being happy "
                , Footnote "A nurse called me \"glib\"."
                , Body " about it would be better than being unhappy about it. That's not the case, I'm just riding a wave of positive emotional inertia, so having a minor, non-life-altering injury is something I can shrug off. But given the opportunity, I'd pick glibness over suffering. I'd rather be unjustifiably happy than unjustifiably sad. And any sadness that won't help you live a better life in the future is unjustifiable."
                ]
            ]
        , Model
            "Hellscapes"
            "hellscapes"
            Listed
            [ Paragraph
                [ Body "What does a hellscape look like to me? Well that's easy to answer. Imagine this: a row of cars, all different, and a row of their owners, all the same. Behind them, another row, behind them, yet another. On and on, forever, people buying these metal boxes to try and be different, and they're all failing."
                ]
            , Paragraph
                [ Body "Oh, a hellscape? Sure, I can answer that. Looking out into the distance, rolling mountains, the distance between them visible in gradations of greyness as they fade into infinity, the layered clouds rolling down to meet them, white on white on white, a thin blue line of the sky the only thing making one clearly distinct from another. But you can't see any of it, because someone built a goddamn highrise."
                ]
            , Paragraph
                [ Body "Maniacal laughter. I see you're writing this down, so I want to be clear, for the record: I said the words maniacal laughter, I didn't laugh maniacally."
                ]
            , Paragraph
                [ Body "Uh, what's a hellscape?"
                ]
            , Paragraph
                [ Body "Imagine, okay, imagine with me for a second. Have you ever been in a call center? Okay, right, so, it's underground, right, like, no windows, just desk after desk after desk. People are on headsets, and this isn't just any call center, but it is a scam call center, and all the employees know, but they can't leave. What do you mean why not? They can't leave because they'd get fired, their families would go hungry. They can't leave. They have to keep scamming. Oh, uh, to answer your question, a hellscape is the looks in their eyes."
                ]
            , Paragraph
                [ Body "A hellscape is the Trump administration. I don't think that requires any elaboration."
                ]
            , Paragraph
                [ Body "A hellscape would be a Hillary administration. I don't think that requires any elaboration."
                ]
            , Paragraph
                [ Body "Imagine World War I. The hellscape is the battlefield, the trenches, or at least that's what people think. That's the physical hellscape. But the emotional hellscape, the mental hellscape, that's sitting in a throne or whatever the fuck presidents sit in, and deciding that ten thousand more people will die. Maybe. Depends, which is worse: killing someone, or signing a piece of paper that will kill thousands? One is worse for the world, but that doesn't matter for hellscapery: what matters is which is worse for you."
                ]
            , Paragraph
                [ Body "A hellscape? That's easy. According to the OED, a \"hellscape\" is \"an extremely unpleasant place or scene\". Can I go now?"
                ]
            ]
        , Model
            "The Semiotics of Gift-Giving"
            "gift-giving"
            Listed
            [ Paragraph
                [ Body "Why do we give gifts? In our currency-mediated world, where anyone can convert money into physical objects very easily, cash should be king, but giving cash as a gift is seen as uncouth."
                ]
            , Paragraph
                [ Body "Perhaps it is a signaling game. Giving someone a gift signals that you care about them. Less so during the holiday season, when giving gifts is basically mandatory, but it is always a costly signal of affection. If you give someone a good gift, you are demonstrating that your internal model of who they are is very accurate. If you are devoting lots of brain space to modeling someone and remembering the things they like, you are more likely to be empathetic to that person. So, giving good gifts is a way of signaling that you will be empathetic."
                ]
            , Paragraph
                [ Body "Perhaps it's not a signaling game. Maybe, instead, it's a way for people to justify buying nicer things. If it is typically best for you to economize, but optimal to also occasionally buy a luxury, having a nice indicator of when to buy nice things or not can help. It's similar to the concept of a Schelling point in game theory: here we want to pick \"economize\" usually, and \"luxuriate\" occasionally, and having one particular day that we decide to pick \"luxuriate\" helps us strike that balance. And maybe we ended up exchanging nice gifts because buying them for yourself feels wrong, like you're being excessively bougie, while buying nice things for other people feels like a kindness. This might also help explain why we don't give cash as a gift. If the point is to absolve yourself of the guilt of buying things that are slightly nicer than usual, you have to be given the item by someone else. Money is infinitely fungible, so when people give you money, you can't really justify luxuriating in it."
                ]
            ]
        , Model
            "The Gift No One Has Given Me"
            "gift-no-one-has-given"
            Listed
            [ Paragraph
                [ Body "When people ask me what I want as a gift "
                , Footnote "Generally, a bad question to ask, as part of the point of giving gifts is proving that you have a good mental model of what the gift's recipient wants."
                , Body ", I always say the same thing: Buy a book, read it, annotate it, and then give it to me. I have yet to have anyone take me up on this offer."
                ]
            , Paragraph
                [ Body "It is a bit strange, perhaps, to ask for a gift that intrinsically requires labor on the part of the gift-giver. Most gifts require some amount of effort; even if you're just clicking \"Order\" on Amazon, you have to decide what it is that you are going to buy, get the package, and wrap it. But that effort is all non-intrinsic, not part of the gift itself. With the annotated book, the gift is itself labor-intensive. And maybe that's a good thing! A book costs ~$20, but an annotated book from my friend I would value at much more than $20. This means that my friends can produce new value in the world by reading, and that getting me gifts is relatively cheap."
                ]
            , Paragraph
                [ Body "An annotated book is a wonderful thing, because it captures some of your consciousness as you read. It's very similar to reading or discussing a book with someone, because you get insight into their thought process and their responses to parts of the book. It captures a person at a point in time. For example, the best way for me to see what I was like in high school is to re-read my twice-annotated copy of Cat's Cradle, and see what my younger self wrote in it "
                , Footnote "Mostly drivel. He didn't have a great grasp on literature."
                , Body "."
                ]
            , Paragraph
                [ Body "And, giving someone a book you've read as a gift means you have one more thing to talk about. Possibly, if the book is really good, you can start trusting each other's taste in books a bit more. "
                , Footnote "Mostly drivel. He didn't have a great grasp on literature."
                , Body "."
                ]
            ]
        , Model
            "Six Eggs"
            "six-eggs"
            Listed
            [ Paragraph
                [ Body "Every day, for the last six years of my life, I have eaten six eggs "
                , Footnote "I've missed some days, but I've had more than six on other days, so let's call it an even six."
                , Body "."
                ]
            , Paragraph
                [ Body "It started in high school, when I was rowing. Once I hit the varsity team "
                , Footnote "Nothing to brag about, actually, because you were a novice for one year and then on varsity. It signaled seniority, not skill."
                , Body ", we had morning practices at 5 AM, twice a week "
                , Footnote "On Monday and Friday, the worst two days to have morning practices. I would have preferred two consecutive days, so that my sleep schedule could be even mentioned in the same sentence as the worst \"consistent\". And not Fridays, because waking up at 4:20 (nice) makes you want to go to bed early, and Fridays aren't generally your going-to-bed-early nights."
                , Body ". I needed to eat something, so that I could make it through two hours of rowing, but I also wanted to sleep as much as possible. Cereal and equivalents "
                , Footnote "Looking at you, muesli."
                , Body " wouldn't do, because I wanted something with lots of protein. Eggs were the obvious option."
                ]
            , Paragraph
                [ Body "I started with four eggs, but that wouldn't quite tide me over until lunch, and I didn't want to eat two breakfasts. So I went up to five eggs, and then six. Six felt right: ~600 calories "
                , Footnote "I eat extra large eggs, because I'm an extra-large guy. You can scale your eggs appropriately."
                , Body ", but still enough food to keep you full for quite a while. I started eating six eggs on the days I didn't have morning practice, and it gradually became habit."
                ]
            , Paragraph
                [ Body "People try to tell me that eating lots of eggs is unhealthy. I respectfully "
                , Footnote "Disrespectfully, if we're being honest."
                , Body " disagree. High blood cholesterol is caused by eating saturated fat, not cholesterol "
                , Footnote "Bet you thought this was going to be a citation."
                , Body ". Eggs are great for you, a solid source of protein and fat. They'll get you through your workout and help you build muscle. And they cost like 10 cents each  "
                , Footnote "As long as you're fine with a little animal abuse happening in the farms you buy them from."
                , Body "."
                ]
            , Paragraph
                [ Body "And eggs are easy to make, relatively. Find me another meal that's 100% protein and fat, and this cheap, and takes maybe 15 minutes to cook, and I'll start eating it every day for dinner. And they taste good! You can add spinach, or cheese, or sausage, or bacon, or mushrooms, or just about anything, to add some variety."
                ]
            , Paragraph
                [ Body "Eggs aren't my favorite food, but they are the food I eat most consistently."
                ]
            ]
        , Model
            "Commuting In Denver"
            "commuting-denver"
            Listed
            [ Paragraph
                [ Body "Commuting can be a great way to get a feel for a city. In Boston, I took the T, an hour each way "
                , Footnote "A terrible idea. I limit myself now to very very short commutes."
                , Body ", and I got to see, although not meet, a large slice of the city's population, that I wouldn't have met any other way. And in Denver, my commute also taught me a lot about the people of Denver."
                ]
            , Paragraph
                [ Body "Let me tell you the story of one woman who I saw while walking into my office. She was walking her dog, which is a pretty normal activity. But what made me look more closely was her outfit and posture: she was wearing those giant, tinted-gradient sunglasses, two-and-a-half inch squares blocking out the sun; and a fluffy vest, you know, the real fluffy ones that look super soft "
                , Footnote "Note that this sentence has both a colon and a semi-colon, putting it at 1.5 total colons."
                , Body ". Her pelvis was thrusted forward, her legs seeming to pull her body around, her torso lagging a bit behind. Her dog (remember now that she was walking a dog) was on a leash, but she wasn't holding the leash –\u{00A0}it was clipped to her belt. It was clipped to her belt because both of her hands were busy. In her left hand, a thermos, probably coffee or tea. Reasonable. In her right hand, a White Claw. Given that it was 8:35 on a Tuesday, I'm going to go ahead and say: unreasonable."
                ]
            , Paragraph
                [ Body "And there were the two guys I saw. One man was wearing a navy suit, brown shoes, looking sleek and clean and honestly pretty hot. Then, there was the other guy. He was wearing a tank top, really a wife-beater "
                , Footnote "What a terrible name for a piece of clothing."
                , Body ", and khaki cargo shorts. He looked a bit scruffy, like he hadn't shaven in a few days. The two men were engaged in conversation, at a volume slightly lower than a shouting match. Maybe they knew each other? Their encounter was happening about a block in front of me, so I was paying attention. Suddenly, the guy in the cargo shorts pulls a beer out from one of his cargo pockets, the man in the suit drops to one knee, accepts the beer, stabs it with his keys, and shotguns it right there on the street. I walk by as he finishes it, throws it into the street, and then says \"Fuck you\" to his friend "
                , Footnote "For a loose definition of friend that includes anyone who has given you a beer."
                , Body "And continues on to what I imagine must be his office job, slightly buzzed "
                , Footnote "Or so you would think, but at altitude, one beer gets you pretty fucked up. I only hope for his career's sake that it was a light beer."
                , Body "."
                ]
            , Paragraph
                [ Body "Then there were the skaters. One path into my office took me past Denver Skate Park, and so I saw lots and lots of skaters over the six months I lived there. First, an important digression: one of the concrete slabs making up the sidewalk had been lifted slightly, raising it about an inch over the next piece of sidewalk. My first month there, I tripped on it often. One week, after stumbling over this slab four times in a row,Monday through Thursday, I walked by on Friday. I thought to myself, \"Alright, pay attention, don't trip again, you got this\". I did not, in fact, \"got this\". I was distracted by the skaters, as I always was, and I stumbled, again, over this one piece of concrete. One of the skaters turns to his friends and says \"check out this guy, five-day tripping streak!\" I don't think he meant for me to overhead, but I did."
                ]
            , Paragraph
                [ Body "Back to the main story, though: these skaters were drinking and smoking at all hours. On my way into work? Some skater was sipping a beer in her car. On my way back from work? Some skater was sipping a beer in his car. On my way back from my apartment to the bars near my office? Same deal. "
                ]
            , Paragraph
                [ Body "I guess the moral of the story is that people in Denver drink more flagrantly than other people do."
                ]
            ]
        , Model
            "Useful Thoughts"
            "useful-thoughts"
            Listed
            [ Paragraph
                [ Body "Cognitive behavioral therapy contains many useful tools for changing the way your brain works. I'm going to talk about one, namely, asking yourself whether your thoughts are useful."
                ]
            , Paragraph
                [ Body "First, a brief sidebar about depression. I think that depression is a sliding scale, going from perfect eukrasia, which is being completely able to actualize whatever brain state you want, and never suffering from weakness of will, to the opposite, perfect akrasia, being completely unable to control your brain state or accomplish the things you want to accomplish. Under this definition of depression, no one is fully depressed or fully non-depressed, but that's alright. If you buy this definition, you should believe that becoming less depressed is a good thing, regardless of how mentally well you are."
                ]
            , Paragraph
                [ Body "Cognitive behavioral therapy proves a useful tool for this: asking yourself whether your thoughts are useful. This has two important pieces. First is figuring what a \"useful\" thought would even mean to you, which requires that you understand what your goals are. It also implies that your thoughts are the sort of thing that can be useful or not useful, and the sort of thing that you have some control over. This alone helps a lot, because whether or not you actually have control over your thoughts, it's better for you to believe that you do."
                ]
            , Paragraph
                [ Body "Second is the ability to realize which thoughts are useful, and to focus on those. When you worry about some future plan or event that might go wrong , there are two distinct types of worrying. The first is worrying about things you have no control over, where worrying won't help the outcome is the predicted catastrophe does happen. One example is flying on a plane, and worrying about the plane crashing. Yes, it would be bad if the plane crashed, but worrying about it won't make it any less bad. This type of worrying is not useful. The second is worrying about outcomes you could make contingency plans for. For example, if you're going on a camping trip, and think \"What if some of our food spoils?\", you can actually intervene and help prevent this by packing fresh or non-spoiling food, and perhaps a little bit extra. This type of worrying is useful."
                ]
            ]
        , Model
            "Walking in Winter"
            "walking-in-winter"
            Listed
            [ Paragraph
                [ Body "In the winter, I walk less. It's cold out, after all, and the cold is unpleasant. A summer walk is immediately revitalizing, while a winter walk feels like a bad idea from step one."
                ]
            , Paragraph
                [ Body "But that's not because it's cold outside. It's because it's warm inside. If it were equally cold in my house and on the street, I would have no problem going for walks. In fact, they would warm me up –\u{00A0}I would probably walk even more than I do currently. The typical framing is \"If it were warmer outside, I'd go for a walk\", but the real sentiment is \"If it were equally temperate inside and outside, I'd go for a walk\". This reframing is quite different, and has different implications for how to make yourself walk more. Making the indoors colder, or the outside warmer, would be equally motivating."
                ]
            , Paragraph
                [ Body "The general idea is that humans react more to differentials than to absolutes. If you want to make one options more appealing, make the other less appealing. Let's say you want to wake up and go to the gym. Putting your phone far away from your bed, so you have to get out of bed to turn the alarm off, may make a huge difference. Lying under the covers, all warm and cozy, it's difficult to muster the willpower to get out of bed and go to the gym. But when you're standing by your desk, mostly naked, holding your phone, getting dressed and going to the gym is actually a pretty good deal. Sometimes, making the choices you don't want to make feel worse is easier than making the choices you want to take feel better."
                ]
            ]
        , Model
            "Intrinsic and Accidental Desires"
            "intrinsic-and-accidental-desires"
            NotListed
            [ Paragraph
                [ Body "Only the desire to reproduce is intrinsic, everything else is accidental. Once evolution creates beings with desires, those that desire to reproduce will be more fit than those that don't have that desire. But let's think about every other desire, and see if any of those are intrinsic."
                ]
            , Paragraph
                [ Body "One of the most obvious contenders is self-preservation. Humans tend to want to keep existing, except in some situations far from the situations we evolved in "
                , Footnote "Like all of modern society, hence the developed world's continuing suicide problem."
                , Body ". But self-preservation is not guaranteed by evolution. If reproducing killed you (and this is true of the males in several spider species), then self-preservation would be trumped by the desire to reproduce. Even then, those spiders have a desire "
                , Footnote "For some loose definition of desire, because I don't think spiders are actively thinking."
                , Body " to avoid death at all other times. Imagine, however, a species where the males showed off by performing dangerous acts in order to impress females. One such species is, well, humans. So it would seem that the desire for self-preservation, while likely to evolve, can be trumped by the desire to reproduce, and so is not intrinsic."
                ]
            , Paragraph
                [ Body "There are other desires, like seeing your family flourish, that might be intrinsic. After all, your family shares your genetics, and so any gene that encourages your family's welfare is likely to be increasing it's own welfare. While this seems plausible in our society, where we have relatively nuclear families, people tend to know which kids are theirs, and people can have large impacts on their kids, this seems plausible. But we could just as easily imagine a society where people are unaware of their kids, like a species that laid eggs in a communal pile and raised each child as part of their society, without a clear understanding of who their parents were. There would be no notion of supporting one's family in this world. So, this desire is not intrinsic."
                ]
            , Paragraph
                [ Body "In general, it seems that all desires, other than the desire to reproduce, which are shared between humans are shared only because we all happened to evolve in a specific way, so our biology and culture supports those traits. Those traits are shared because humanity took a common path to get to where we are today, not because they are guaranteed to evolve."
                ]
            , Paragraph
                [ Body "Of course, humans mostly don't have a desire to reproduce – we have the desire to have sex. For the vast majority of the evolutionary timeline, sex was tantamount to reproduction, so we ended up enjoying sex. Because of this, fortunately, we can use birth control and enjoy the benefits of fulfilling our necessary desire without the downsides."
                ]
            ]
        , Model
            "Mug Design"
            "mug-design"
            Listed
            [ Paragraph
                [ Body "Most mugs suck. When I use someone else's mug, there's a high chance that I'll either burn my fingers, be unable to hold it properly using the handle, or the mug will be too small to properly steep a single teabag, or it will have some weird garbage around the rim that makes it difficult to drink."
                ]
            , Paragraph
                [ Body "Handles are an easy one, so let's get them out of the way first. The handle of a mug serves one purpose: it provides a way to hold a mug without burning your fingers. You want the mug to store hot liquids, and you want those hot liquids to stay away from you. If you didn't care about the temperature of the liquid in the mug, you would just use a cup. So, how can you design a handle to protect people's hands? First, it has to have the right amount of spacing between the inside of the handle and the body of the mug. If there isn't enough spacing, then someone holding the mug by the handle will have their fingers touch the inside of the mug, defeating the purpose of the mug. Just having the right spacing is not enough, though. If the handle is too small to fit three fingers (preferably four), someone holding the mug will have to contort their hand and use all the weird small muscles that move your fingers side to side in order to clench the mug in place. Their hands will get tired, they'll have to use their other hand to stabilize the mug, and they'll burn their fingers on the side."
                ]
            , Paragraph
                [ Body "Let's assume we have a mug with a good handle: four fingers fit inside, far enough from the body of the mug to avoid incidental knuckle-burns. The next most important thing is size. While \"not burning its users\" is necessary for a mug to be satisfactory, it is not sufficient. Different mugs serve different purposes, so coffee shops have many different sizes, for tea, black coffee, lattes, cortados, and shots of espresso. However, in a house, people typically just make tea and regular coffee. When making tea, whether you steep it from a teabag or loose leaves, there is usually one correct amount of water to steep: around 12 ounces. But some people have these tiny fucking mugs that aren't useful for anything. A teabag will oversteep 6 ounces of water, so you have to remove the teabag early, but that changes the flavor profile. And who wants only 6 ounces of tea? I don't trust people who own mugs that are excessively small."
                ]
            , Paragraph
                [ Body "The rim of a mug is the second most important contact surface, after the handle. The rim is where I put my mouth, and where the contents of the mug comes out. Most mugs do a good job of this, and have a smooth circular surface. But some have wacky shapes, so that their cat-shaped mug can have cute ears or whiskers or something. This is fine, as long as the interruptions to the rim happen only above the handle and across from the handle: places where people aren't putting their mouth to drink. If there's a lumpy bit where I put my mouth, not only will it confuse me every time I try to drink from your mug, but it also makes it harder to form a seal against the mug with my mouth. If I can't form a seal with my mouth, there's a reasonable chance that I'll spill my drink while drinking. While this is always a bad thing, it's even worse when the thing I am drinking is guaranteed to be hot. I've drank enough nearly-boiling coffee and tea in my life to destroy all of my taste buds that can't handle those temperatures, but I don't make a habit of pouring nearly-boiling coffee and tea into my lap, and I'd prefer if mug designers understood that."
                ]
            , Paragraph
                [ Body "I'm not asking for much. I want mugs to hold a reasonable amount of liquid, so they can serve their jobs as drinking vessels. And I want to be able to use them without being injured. Imagine someone made a computer keyboard that worked most of the time, but occassionally shocked you, and the designers figured that you only wanted half of the number keys, and had sharp bits on the wristrest so that the keyboard could look like a dragon You would sue. Stop accepting bad design for mugs. "
                ]
            ]
        , Model
            "Accidental Underworld"
            "underworld"
            Listed
            [ Paragraph
                [ Body "In February, I went to New York "
                , Footnote "City is implied, I think, but I have some doubts, hence this footnote."
                , Body ". While I was there, I tried to buy bagels, and ended up having one of the most surreal experiences of my life."
                ]
            , Paragraph
                [ Body "I went to [REDACTED] Bagels, a bagel store my friend had recommended. I walked in, and placed my order, than waited and watched the people around me while my order was being prepared. The people there seemed normal"
                , Footnote "For New Yorkers."
                , Body ". [REDACTED] Bagels was full that day, so there was standing room only. I figured that my friend's recommendation must have been good, seeing as lots of locals seemed to like this place. I asked one of the baristas "
                , Footnote "Are they baristas at a bagel shop? Maybe I mean bagelistas."
                , Body "where the bathroom was, and got a pretty innocuous response: \"Oh, they're just downstairs\"."
                ]
            , Paragraph
                [ Body "I found the stairs in the back. They were tiled with those little one-inch square tiles that evoke old-school train stations and new-school train station bathrooms. The stairs were grimy. Not dirty, which would imply that the tiles could be cleaned easily, perhaps just by sweeping. No. These tiles had been around for at least a hundred years, slowly accumulating a layer of whatever that stuff is that coats the soles of shoes you own after you wear them in a city for a month."
                ]
            , Paragraph
                [ Body "I reached the bottom of the stairs, and I most emphatically did not find a bathroom. Instead, I found myself in a dimly-lit, smoke-filled bar. Dozens of people sat around this area, either at the bar or alone at tables, all drinking while typing away on Macbooks. Strange, for a Sunday morning. I pushed through the bar, toward the first doorway I saw, and walked into the next room. Contrasting with the ornate, mildly-cluttered look of the bar, this next room was bare. Dark, wood planks mounted on minimal brushed-aluminum brackets were mounted around the room, on the otherwise empty lavender walls. One wall, in a sans-serif font, proclaimed that this was an \"Algorithmic Perfumery\". A man wearing green-tinted sunglasses stood behind a cash register, staring at me. I turned away, and tried the next exit from the bar."
                ]
            , Paragraph
                [ Body "But there was no bathroom through the second doorway either. This one was a similarly minimal clothing boutique. I tried the third doorway, which lead to another set of stairs down. I took them, figuring that \"downstairs\" might have meant \"down two flights of stairs, and ignore the fuckery going after the first flight\". As I descended, the stairs got much darker, until I reached the bottom, where the light was downright murky. I see a sign for the men's bathroom, and I follow it. Inside the bathroom, a man is standing shirtless in front of the mirror, flexing. When I walk in, he puts his shirt back on and then runs out."
                ]
            , Paragraph
                [ Body "This gives me time to figure out the bathroom itself. It feels strangely out of place: large, bronze fixtures, a proper copper washbasin for a sink, and an oval mirror with ornate carvings in the mirror's frame."
                ]
            , Paragraph
                [ Body "I walk back up the stairs to the bar. Then, I take another flight of stairs –\u{00A0}and find myself somewhere completely different. I am no longer in [REDACTED] Bagels, I am in a different restaurant. I go back down to the bar, find the correct set of stairs, and make it back to [REDACTED]. I pick up my order, and notice that the store is almost empty. When I went down to the bathroom, there was barely standing room, and now most of the chairs were empty. I hurry out." ]
            , Paragraph
                [ Body "The bagels were okay."
                ]
            ]
        , Model
            "Graduation"
            "graduation"
            Listed
            [ Paragraph
                [ Body "I graduated! Which was pretty monumental. Graduation, for most people, is the end of the third-to-last chapter of their life. If you don't go to grad school, change careers in a large way, all that's really left is starting a family. The second-to-last chapter, then, is the time you were graduated but didn't have kids, and the last chapter is the time in which you were both graduated and childful."
                ]
            , Paragraph
                [ Body "But this essay isn't about dividing life into chapters, it's about figuring out what to optimize. I spend a lot of my time optimizing things. How can I type faster, find results on Google "
                , Footnote "Or, more recently, DuckDuckGo."
                , Body "faster, figure out algorithms faster, learn facts faster, read papers faster. And those optimizations are all useful under a backdrop of time pressure created by college. While I was in school, there were good goals to aim for. If I could figure out a way to work faster, I would have more time for myself! But now that I'm graduated and have a job "
                , Footnote "Which is probably a good job, although I'm not sure I've sampled enough of the space of possible jobs to know that for sure."
                , Body ", I am mysteriously out of things to optimize for. Maybe it's the fact that there's a pandemic on, but it really feels like I'm out of high-level goals to optimize for. There's plenty of things to do, I go backpacking and cook and I'm learning to make espresso and there's always more programming to learn. I have goals, to be sure, but it's not clear what I'm optimizing for!"
                ]
            , Paragraph
                [ Body "While I still prefer to be efficient, I'm now assigned 40 hours a week of work. When you're assigned a fixed amount of hours of work per week, finishing early means that you get to do more work, not that you get time off to do what you want. It does kind of work that way in practice, particularly while working remotely –\u{00A0}once you're finished with an \"acceptable\" amount of work in a week, you can dick around and no one will complain. But it doesn't feel that way. If you plan on working 40 hours a week, and that's your primary goal, then optimizing stops being a useful thing for you. You are optimizing for someone else's benefit, not your own."
                ]
            ]
        , Model
            "Carbon Dioxide"
            "carbon-dioxide"
            Listed
            [ Paragraph
                [ Body "Carbon dioxide poisoning is a real thing. When I talk about it, people assume that I either mean carbon monoxide, or that I'm trying to trick them by getting it wrong intentionally.  But I'm not - I honestly, really believe that carbon dioxide poisoning is a real concern."
                ]
            , Paragraph
                [ Body "When humans breathe, we convert some amount of oxygen into carbon dioxide. That carbon dioxide just hangs around, unless some other air comes along to replace it with carbon dioxide levels that are at the same level as baseline, outdoor air. Over time, human carbon dioxide builds up. Everyone knows that this is a bad idea at the extremes: if you lock someone in a box for long enough, they will not be able to get enough oxygen and then they will suffocate."
                ]
            , Paragraph
                [ Body "However, CO2 poisoning can happen at much lower levels than required for suffocation. The baseline on Earth is about 420 parts per million (ppm) "
                , Footnote "For now. It was 320 ppm in 1960, and seems to still be on the rise. "
                , Body ". Raising CO2 levels to even 1000ppm causes people to become sleepier, less motivated, and less capable of high-level thought. If you don't believe me, "
                , Link { text = "read", href = "https://doi.org/10.1289/ehp.1510037" }
                , Body " "
                , Link { text = "some", href = "https://doi.org/10.1289/ehp.1104789" }
                , Body " "
                , Link { text = "of", href = "https://doi.org/10.1080/15459624.2015.1076160" }
                , Body " "
                , Link { text = "these", href = "https://doi.org/10.1016/j.egypro.2017.07.378" }
                , Body " "
                , Link { text = "studies", href = "https://doi.org/10.1016/j.enbuild.2007.04.013" }
                , Footnote "I don't think that providing five non-literature-review studies is enough to prove a point compellingly, but my time on LessWrong has shown me that it is an effective strategy for convincing people you're right."
                , Body ". Also, several people have recorded their CO2 levels while sleeping in a bedroom overnight, and reported the CO2 level hitting "
                , Link { text = "1900", href = "https://www.reddit.com/r/dataisbeautiful/comments/cek4dm/co%E2%82%82_in_my_sons_bedroom_at_night/" }
                , Body "or even"
                , Link { text = "3500", href = "https://vair-monitor.com/2016/09/29/sleeping-closed-room-indoor-co2-analyze/" }
                , Body "ppm. Office buildings, particularly meeting rooms, also worry me."
                ]
            , Paragraph
                [ Body "I'm amazed that I've never heard about this - I have read lots about nootropics, and never heard \"open windows\" being mentioned. And somehow, architects keep making poorly-ventilated buildings. I wrote this article to spread the word about C02, and to see if I could justify spending $140 on a carbon dioxide detector. All in all, it seems like a small price to pay to potentially avoid a brain-damaging toxin, particularly when detecting it would allow me to fix it easily - just by opening a window. Also, the idea of walking into a meeting with a CO2 detector set to beep above 1000ppm, and leaving when it starts to beep, really appeals to me. It's the Geiger counter of the non-radioactive workplace. "
                ]
            ]
        , Model
            "The Queen's Gambit"
            "the-queens-gambit"
            Listed
            [ Paragraph
                [ Body "The Queen's Gambit was a decent show, with a terrible ending. The final episode opens with some suspense: Beth is going to Russia, and the State Department sends a man to accompany her. This is some intrigue - why is he following her?"
                ]
            , Paragraph
                [ Body "The plot thickens on the flight to Moscow. The man reveals that he was brought along because the Russians are considering defecting (or maybe because they are trying to recruit Beth to the Soviet side). Interesting - the show brings a new character in, and has him reveal important information about the main characters. What could come next?"
                ]
            , Paragraph
                [ Body "They arrive in Moscow. Beth wins a bunch. The final match is in progres, and then a recess is called. Beth sees the Russians huddled around a chess board, consulting her opponent on his next move; Beth gets similar consulation from her friends back in America. Seems like bullshit to be able to pause a chess game halfway through to consult your friends, but whatever. The parallels between America and Moscow are revealed. Will she win?"
                ]
            , Paragraph
                [ Body "Yes, of course, because this is a feel-good show where the main character never loses without there being a good reason for it established first. And then, as her opponent concedes, he hands her the black king, and tells her to keep it, shaking her hand closely, enveloping it with two hands. Now, it is as clear as daylight that she has been handed a note - the State Department guy was foreshadowing this!"
                ]
            , Paragraph
                [ Body "Nope. Beth pockets the king, goes back to the car, and nothing is revealed until she leaves for the airport to catch a flight to America. But wait! As she is driving to the airport, she stops the car and asks to walk - leaving the State Department guy to be by herself! Of course, the audience realizes, we were being kept in the dark, she has a note instructing her to meet the Russians and she is going to meet them now! "
                ]
            , Paragraph
                [ Body "Nope. She just walks down the streets, enjoying her fame from winning the World Chess Championships. A group of people playing chess in the streets, mob her and reveal her celebrity status. She sits down with a random person from the crowd, looks him in the eyes, and says \"Let's play.\". And then - credits."
                ]
            , Paragraph
                [ Body "What the fuck? What was all that foreshadowing about? I felt pissed about the ending, and felt that it missed something. I try not to get upset about things being bad unless I can come up with a better version in five minutes. Here, however, something occured to me in my five minutes. If she did receive a note, telling her to meet the Russian champions in the park, and went there after leaving the taxi, they could have had a conversation. He could have said \"come defect to Russia, we can train together\" or \"I want to defect to America, we can train together\". Beth could have said \"I have played a lot of chess. I think it's time I take a break.\", and then walked away. Then, her game with the stranger from the street is meaningful. She is playing chess for fun, not because she has to! She has proven herself, to herself, and can take time off, but she also genuinely enjoys the same."
                ]
            , Paragraph
                [ Body "In my opinion, that ending would have been much more satisfying."
                ]
            ]
        , Model
            "Merging Onto Storrow"
            "merging-onto-storrow"
            Listed
            [ Paragraph
                [ Body "I got a visceral reminder of how two people can interpret an interaction entirely differently over the summer. My friend was driving me and some others back from the beach. We took 93 into Boston, and drove over the bridge. I marveled at the view, like I always do. As we soared across the Charles into Boston proper, my friend mentioned that he can tell who the tourists are because they don't know how to merge on Storrow drive. As someone who has never driven in Boston, I wasn't quite sure what he meant, and asked him to elaborate."
                ]
            , Paragraph
                [ Body "He told me that the tourists saw the sign saying \"Storrow Drive\", got in that lane a half-mile before they had to, and waited in traffic. Meanwhile, locals knew to drive down as far as they could before merging in to the lane that would end up as Storrow. Now, to me, the people who get in line are the kind people who obey the laws of traffic and maximize average speed, and the people who cut in at the last possible moment are doing whatever would save themselves time, even if it costs other people time. To me, the good side to be on is in the lane, and for him, the good side to be on is merging in later."
                ]
            , Paragraph
                [ Body "We both agree, in general, that doing things that benefit yourself at the cost of other people is bad "
                , Footnote "Although, he does work in finance, so I might have to double-check on this one."
                , Body ". We agree that merging in late slows down traffic overall. But to him, this particular action was a \"thing locals did\", and so something that he was supposed to do, and something that signalled he was part of this group. Certainly whatever the locals do is just the local custom, and not complying is rude! We both saw that exact same interaction on the roads, and came to entirely different conclusions about what the \"correct\" or \"acceptable\" thing to do is."
                ]
            ]
        ]



-- , Model
--    ""
--    ""
--    Listed
--    [ Paragraph
--        [ Body ""
--        , Footnote ""
--        ,Body ""
--        ]
--    , Paragraph
--        [ Body ""
--        , Footnote ""
--        ,Body ""
--        ]
--    ]

module Essay exposing (Model, essays, getEssayBySlug, view, viewEssayLink, viewEssayPreview)

import Html exposing (..)
import Html.Attributes exposing (..)
import Route



-- TODO The source of every essay has to be in code in this file. I want to store the text in markdown-esque files
-- TODO footnotes are janky, no links and show up as [1] instead of a superscript
-- TODO footnotes shouldn't have to specify their number, should be automatically generated
-- TODO essay titles should appear above essays when on their pages
-- TODO include date that the essay was written


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
    | TextWithFootnotes (List FootnoteItem)
    | Quote QuoteItem


type FootnoteItem
    = Body String
    | Footnote Int String
    | NumberedList (List String)
    | NumberedListStartingAt Int (List String)


getEssayBySlug : String -> Maybe Model
getEssayBySlug essaySlug =
    List.head (List.filter (\p -> p.slug == essaySlug) essays)


view : Model -> ( String, List (Html msg) )
view { name, content } =
    ( name
    , [ div [ class "blog-content" ]
            [ div [ class "blog-text" ]
                (let
                    accs =
                        List.foldr viewContentItem ( [], [] ) content
                 in
                 List.append (Tuple.first accs) (Tuple.second accs)
                )
            ]
      ]
    )


viewContentItem : ContentItem -> ( List (Html msg), List (Html msg) ) -> ( List (Html msg), List (Html msg) )
viewContentItem item ( bodyAcc, footnoteAcc ) =
    case item of
        Plain contents ->
            ( p [] [ text contents ] :: bodyAcc, footnoteAcc )

        TextWithFootnotes list ->
            let
                fn footnoteItems =
                    case footnoteItems of
                        Footnote index content ->
                            Just
                                (div [ class "footnote" ]
                                    [ div [ class "footnote-number" ] [ text ("[" ++ String.fromInt index ++ "]") ]
                                    , div [ class "footnote-content" ] [ text content ]
                                    ]
                                )

                        _ ->
                            Nothing

                footnotes =
                    List.filterMap
                        fn
                        list
            in
            ( p [] (htmlFromFootnoteItems list) :: bodyAcc, List.append footnotes footnoteAcc )

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


htmlFromFootnoteItems : List FootnoteItem -> List (Html msg)
htmlFromFootnoteItems footNoteItems =
    let
        htmlFromFootnoteItem oneItem =
            case oneItem of
                Body content ->
                    text content

                Footnote index _ ->
                    text ("[" ++ String.fromInt index ++ "]")

                NumberedList list ->
                    ol [] (List.map (\item -> li [] [ text item ]) list)

                NumberedListStartingAt startingAt list ->
                    ol [ start startingAt ] (List.map (\item -> li [] [ text item ]) list)
    in
    List.map htmlFromFootnoteItem footNoteItems


textFromFootnoteItems : List FootnoteItem -> String
textFromFootnoteItems footnoteItems =
    List.foldr (++) "" (List.map textFromFootnoteItem footnoteItems)


textFromFootnoteItem : FootnoteItem -> String
textFromFootnoteItem item =
    case item of
        Body string ->
            string

        Footnote int string ->
            ""

        NumberedList list ->
            ""

        NumberedListStartingAt int list ->
            ""


viewEssayPreview : Model -> Html msg
viewEssayPreview model =
    case model.isShown of
        Listed ->
            div []
                [ p [] [ viewEssayLink model ]
                , p [ class "essay-preview" ]
                    [ text
                        (String.left 160
                            (List.foldr (++) " " (List.map contentItemToString model.content))
                            ++ "…"
                        )
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

        TextWithFootnotes footnoteItems ->
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
            , Plain """What would a programming challenge intended to be delibrate practice look like? I don't know exactly, and to figure it out we first have to define software engineering, so that we can see what it means to get better at it. Let's say software engineering is writing maintainable, correct code. If someone writes more maintainable code than they used to, we would say they're a better software engineer, and the same for writing code that is closer to correct. HackerRank-style challenges will help with correctness, as your code does have to pass a suite of unit tests. But it doesn't help you with _ensuring_ that your code is closer to correct, because you aren't writing your own unit tests. If you have someone else's test harness to lean on, you aren't practicing the skill of testing, which means a programming challenge where you have to write your own tests would be superior. HackerRank-style challenges also don't reward maintainability, in fact, by only judging competitors on speed to solve the problem, writing code that is maintainable will hurt your performance."""
            , Plain """How can we test programmer's on their ability to test? One way of doing this is asking programmers to write unit tests for a problem without writing code, and then seeing if their tests pass with correct implementations and fail on incorrect implementations. This has some issues, most notably that typos in the unit tests would have a huge effect on outcomes, and that the iterative write-test/write-code/debug cycle is the skill we really care about, not writing the tests correctly on the first try. Perhaps it would be better to get developers to write code and tests, or just make the code complex enough that automated unit tests are useful."""
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
            , Plain """There's a general lesson to be learned here. While I still should make the large changes that will give me most of the benefits (pareto's 20%), the smaller changes won't pay off as well. I think in general, in computer science/software engineering, large changes like using one shell over another, or one programming language over another, take the same amount of time as smaller changes, such as changing a minor misbehavior of your terminal or following a specific style guide in your language. Because the time investment is the same, the investment-to-payoff ratio is larger for larger changes. And so, in the future, I'll try to focus on picking the right tool for the job, and less on the color of that tool."""
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
        -- TODO make that XKCD link a real link
        , Model "The GRE"
            "gre"
            Listed
            [ Plain """The GRE is a silly way to evaluate PhD candidates. Let's think about the skills required for day-to-day  research, and contrast with the skills tested by the GRE."""
            , Plain """Research requires a lot of reading, a fair bit of applied statistics, and a lot of writing. The reading is obvious, as papers are the primary way for researchers to formally communicate their ideas. If you lack reading comprehension, you won't be able to keep up with the literature, as you won't understand what the experts in your field are saying. So you must be able to extract the core arguments from a text, evaluate arguments, and understand the vocabulary used in these papers. You have to  used to evaluate statistical arguments, particularly in the health and social sciences, where a lot of the research is based on RCTs and statistical experiments to show the efficacy of a drug or the power of some psychological effect. If you don't know how to evaluate study design, what a p-value means, and how to do Bayesian inference, you will fail to understand many of the arguments being made. And writing is important for the same reason as reading - papers are the primary way to package your ideas for general consumption, so you must be clear and accurate in your writing."""
            , Plain """At first glance, the GRE appears to be testing on those things, after all, the three sections are "Reading", "Quantitative", and "Writing". So surely the skills tested by the GRE are those that will be valuable in a PhD? No. The section headers line up with the abstract notions of the skills, but the details are all wrong. """
            , Plain """Starting with the reading section, there is a large portion that is simply vocabulary and multiple-choice-test-taking skills. One section, about 6 of the 25 questions on the practice exams I've taken, are called the "Section Equivalence" Questions. The question asks you to find a pair of answers that mean the same thing when plugged into the sentence, and lead to a coherent sentence. Let's leave aside the discussion of whether their desired vocabulary is relevant. (Did you know that phlegmatic means "sluggish, unemotional or apathetic"? I didn't, because no one says or writes "phlegmatic".) The question has six possible answers. Looking at the answers alone, avoiding the question, you can get a decent score on these questions. Typically, there are only one or two pairs that mean the same thing, allowing you to narrow down the selection of answers from 30 (picking 2 answers from 6 possible choices) to 2. A 50/50 shot is pretty good: On the GRE, getting 50% of the questions right is around 50th percentile [citation needed]. But notice that we haven't even examined the sentence – this question format is almost entirely a vocabulary question. Once you read the sentence, some mental effort is required to understand which of the two pairs would be coherent, but normally the two pairs are diametric opposites, as in "commonplace/everyday" and "opulent/expensive". Yeah, a quick glance at the sentence is going to immediately tell you which of the two pairs fits. The "text completion" section is similarly based entirely on knowledge of vocabulary, if you assume that people who want to get a PhD have some basic understanding of how sentences work. Maybe my undergraduate classes are atypical, but I had reading comprehension drilled into me by my philosophy professors (after a series of low grades and hanging around in office hours until I got it). Finally, I'll give the GRE some credit: the passage comprehension problems I have no problem with, as reading a passage, performing basic exegesis, and understanding the conclusions that the argument makes and how each point ties in to the whole are fundamentally useful skills. They're useful for PhD students, yes, but also for reading and comprehending news articles and political speeches, so I have no problem practicing my ability to understand arguments."""
            , Plain """ Moving to the math section, I question the usefulness of a majority of questions. As I said above, I am entirely in favor of statistical reasoning being tested. Researchers need this skill. However, beyond the statistics, which are fairly rudimentary, the majority of the questions are about basic algebra and geometry. Don't get me wrong, these skills are incredibly useful in day-to-day life. Algebra is required to budget effectively, understand which of two items in the grocery store is cheaper per unit (https://xkcd.com/309/), and other basic problems of optimization that pop up in your life. Geometry is useful for understanding how much surface area something has before you paint it, intuiting the amount of water in different-sized pots while cooking, and other operations on physical items. But these are not problems that are guaranteed to crop up in someone's research. In evolutinoary game theory, algebra (and calculus) are essential to manipulating the mathematical symbols we represent evolutionary processes with. It's a crucial skill – in the field I want to go into. Neuroscientists, moral philosophers, and historians don't need geometry. So why do we evaluate them on this? You could say that at least the philosophers and historians aren't being evaluated on their quantitative scores as much as the verbal portion of the test - but neuroscientists most definitely are judged on the scores on the mathy parts. I don't object to assessing the quantitative skills of applicants, but the test would be more effective if it tested the quantitative skills that will actually be used most often. Studying geometry for the sake of one test, instead of learning some principles of statistics that will help you forever, seems like a waste. """
            , Plain """The writing section gets at some of the same skills that are used in writing a full paper, but misses the mark in some keys ways. For example, the text editor they give you is crap. No find-and-replace, no spellchecker, nothing. You're given 30 minutes to respond to make an argument for or against a prompt on a random topic, and then 30 minutes to find the holes in an argument. These are both legitimate writing tasks - the argumentative essay is a great way to express your opinions on a subject, and argument analyses demonstrate not only effective writing skills but the ability to understand the flaws in arguments. But giving someone only thirty minutes, while more or less demanding that both essays fit the "intro/three body paragraphs/conclusion" structure, divorces this writing task from academic writing. Papers are written over weeks or months, with multiple rounds of revisions. And they tend to be much longer than the ~750 words I can bang out in a half-hour. This is more about writing to the test, than it is about overall writing ability, although the most egregious lacks will be apparent. For example, a heavy reliance on a spellchecker, a complete misunderstanding of sentence structure, or an inability to make valid arguments will show through in this thirty minute mini-essay. So this task is fine for weeding out terrible writers, who I guess PhD programs assume can't be trained. But do we really need that? If someone is a terrible writer, and particularly if they are bad in the thirty-minute rushed essay responding to a novel prompt with no research, you'll already know. Most of high school involved writing those types of essays, and you know that this applicant got into college. Surely, if their grasp on the English language were so bad as to prevent them from being able to fake it for thirty minutes, they wouldn't be where they are. I understand the purpose of this section for non-native English speakers, but that's what the TOEFL is for."""
            , Plain """I still studied for the test a bit, and had a low-grade fear of doing poorly on it until I took the first practice test. Now, armed with some level of confidence that my score will be good enough, I won't be spending any more of my time on skills that will be useful only in this context. I hope PhD programs use the GRE to reject bad candidates, but don't look at high GRE scores as indicative of anything more than test-taking ability."""
            ]
        , Model "The Outside View"
            "outside"
            Listed
            [ Plain """I believe that watching Netflix, scrolling through Instagram, and swiping on Tinder is overall bad for other people, on average. So why do I do those things sometimes? In general, when I use these apps, I have reasons why my usage is atypical and so not as likely to be harmful as other people's usage of the apps. This is probably irrational. Without strong evidence pointing towards me begin special, it's much more reasonable to assume that the effect of technologies on my welfare is approximately average."""
            , Plain """"Taking the outside view" means looking at the statistics for people in your situation, and applying those statistics to your own position. Instead of asking whether you are being benefited by a particular app, ask whether people on average are benefited. This engages your critical thinking skills, instead of asking in essence whether you have a first-order desire to keep using some app. It may be addictive, and fun, and even enjoyable, but to ask yourself whether it is beneficial to others will reveal whether it is beneficial to you."""
            , Plain """So, if I think that deleting Instagram or Facebook would be good for everyone else, I ought to delete Instagram or Facebook myself. While I was always an atypical user of both, I think, the same reasons that apply to other people apply to me. My typical argument against Instagram is that the constant comparison of aethetics with other people is bad for your mental health and promotes shallow cravings, as well as being a distraction from what really matters in life. Even when I moved my own Instagram account to a "write-only" mode where I would download the app when I had an image I wanted to post, post the image and delete the app again, I still was influenced by the pressures of knowing other people were seeing the images I posted. When taking photos, wandering around the streets of Boston with my DSLR, I was capturing the photos that I thought others would like. And it wasn't the kind of other-related preference like when you take a photo specifically because you think one person you know will like it, which is almost a form of gift giving. "I took this photo, because I thought you'd like it" is quite different from "I took this photo, because I think it implies positive things about my lifestyle to people who glance at it". Because Instagram makes you care about the judgements of everyone, in aggregate, the niche aesthetic moments that appeal only to one friend aren't the highest value."""
            , Plain """It's worth noting that I did, in fact, delete my Facebook, Instagram, and Snapchat. (I'm still working on Netflix.) This is mostly because the Black Mirror episode "Smithereens" really got to me, not because of the rational deliberations presented here. I think that I benefited from these deletions, because I think that other people would have benefitted from them."""
            ]
        , Model "Good Self-Harm"
            "goodharm"
            Listed
            [ Plain """While "Good Self-Harm" seems like an oxymoron, it's not. Look at vigorous exercise, cold showers, or fasting. These three activities all cause pain, and we choose to voluntarily inflict them upon ourselves, so we have the "self" and the "harm". Normally, when people talk about self-harm they mean something like cutting, which is not good for you long term. But these other three, despite being painful, are good for you, and in fact one of the three is even actively encouraged by society."""
            , Plain """I think that intentionally doing things that you know you won't enjoy in the moment is good for you in the time beyond that moment. I am more productive and maybe even happier on days I take cold showers, and other people seem to report similar effects. Getting into the shower, I still feel my muscles clench in anti-anticipation, knowing that the cold will hurt. But I step in anyways, and after a minute or so, I don't feel the cold anymore. More importantly, it gets me in the habit of forcing myself to do things that I don't want to do in the moment. The same brain muscles, I think, get used dragging yourself to the gym, into a cold shower after the gym, and off Netflix to do something you actually want to do."""
            , Plain """So why is only exercise encouraged by society, of the three above? Fasting is seen as weird, especially the three-day-long fasts where I work tirelessly, lift my old PRs effortlessly, and develop a deeper understanding of when I'm actually hungry and when I'm just bored. People don't like fasting, and I can totally understand why –\u{00A0}hunger pangs are painful, and missing out on some social opportunities because I can't eat sucks. But going to the gym also sucks, causes pain, and takes up time that could be used elsewhere. There's no important difference I can see here. There are health benefits to lifting, but there are health from fasting as well (autophagy). There are aesthetic/fitness benefits to going to the gym, but similarly to fasting. Fasting execissively can be dangerous, and a sign of a mental disorder, but so can excessive gym-going. And cold showers take very little time –\u{00A0}almost by design, as one of your first goals after you step into a cold shower is to get the hell out. Not only will you save maybe ten minutes by making your showers faster, but cold showers make you more productive and focused during the day. So, if you're going to the gym, I think you should consider the other two interventions, based on the same reasoning."""
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
            [ Plain """Actualizing upon your second-order desires is easier than most people believe, and has a large positive impact on your life. To clarify, secord-order desires are the desires you have that are about the desires you have, for example, a desire to desire candy less and vegetables more, or a desire to desire learning more. These desires are about something entirely internal, your desires, and so should be easier to achieve than desires that require changing the external world. Unfortunately, people don't have that level of control over their brains."""
            , Plain """Or, perhaps, we do have a lot of control over the future state of our minds, but we don't know how to exert that control. This seems relatively implausible, because the default assumption in most matters of the mind is that each person knows their mind entirely. Looking at examples like walking, however, paints a different picture. Walking is widely known [citation needed] to increase subjective wellbeing and causes happier moods, but most people still walk very little. In fact, when I am in a bad mood, I know consciously that going for a walk will make me feel better, but it still requires mental fortitude to get my shoes on and get out the door. Other things which also would improve my mood, like seeing a friend or drinking good coffee, I don't resist in the slightest. I enjoy going for walks, and they are long-term good for me, and yet I still fail to take as many walks as would be subjective-wellbeing-maximizing. I have a second-order desire to desire to go for walks, but a first-order desire to avoid walks. Actualizing the second-order desire, changing my opinion of walking to be immediately and viscerally positive, would be good for me. But conventional wisdom says that you can't just up and decide to change your mind."""
            , Plain """I think that the conventional wisdom is mistaken. Positive and negative reinforcement work on humans, just as well as they do on other animals. Go for a walk, then eat a square of chocolate. Surely, your disposition towards walking will be better than if you bit a lemon immediately afterwards. If you keep up the habit of having chocolate after you go for a walk, your brain will start associating the walk with the pleasant feeling, and keep valuing the walk even once you discontinue the chocolate routine. Another possible way to change your desires is through cognitive behavioral therapy. If cognitive behavioral therapy can help some people with severe eating disorders, depression, or OCD change their thought patterns, it is clear that it has the power to change negative thought and action patterns to neutral ones. If it can change negative to neutral, why not neutral to positive? For the first three weeks I went to the gym, I told myself that I enjoyed the experience, even as I woke up at 6AM, sore, exhausted, and not seeing agny improvement. And after those three weeks, I started enjoying it. Now, correlation is not causation, and it's possible that I would have enjoyed getting back into working out regardless, but it can't hurt."""
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
            [ TextWithFootnotes
                [ Body """ On Halloween, one of my ex-roommates (call him Roommate), one of my exes (call her Ex), and a guy I met at my college orientation (call him Orientation) and I went to Lincoln, Nebraska. You need to have a very legitimate reason to leave Seattle, Boston, Boston, and Denver (respectively) to go to Nebraska, but we had one: we were going to present posters at a philosophy conference. Our poster was titled "Technology, Social Choice, and Democracy: The Cute Dog Project","""
                , Footnote 1 "I put commas outside of quotes, and you're just gonna have to deal with it."
                , Body """ and it was pretty much what it sounded like: we ran a vote to determine who in Northeastern's philosophy department had the cutest dog. I thought this whole situation was pretty absurd. """
                ]
            , Plain """ My train leaves Denver at 7:10, so I leave my office's Halloween party at 6:50. I stumble up to the AmTrak at 7:08, and when I say "stumble", I really mean stumble. I make it onboard, collapse into a seat, and fall fast asleep. Or at least, wished I could fall asleep. The man next to me was having a hushed but still quite loud phone call, where he describes in detail how his girlfriend has been stealing his drugs. Looking at him, I'm not quite sure how he has a girlfriend, and I imagine that he could benefit from having fewer drugs around. So I'm sympathetic to his girlfriend's choices, if she really exists and really is stealing from him. A few roads ahead, a baby wails. I put on soundproof headphones, and get ready to read. I take my shoes off, trying to get comfortable, and the second I set my sock-clad feet on the ground, I feel cold and wet. Someone has left some kind of liquid on the ground beneath the seat I'm in. I take a deep breath, convince myself that it's just water, and move one seat up. At this point, I'm pretty sure that the weekend is going to be mediocre at best."""
            , TextWithFootnotes
                [ Body "My Lyft driver "
                , Footnote 2 "I feel an ethical imperative to choose Lyft over Uber, because of various bad things that Uber does. Strangely, this is the only company I feel negatively about – my assembled-by-children-in-sweatshops iPhone doesn't make me feel bad in the slightest, and I don't particularly care about the fair trade label on food and coffee. I think that having the two apps on my phone, right next to each other, makes it very obvious that I have a choice between something bad and something worse, whereas in most decisions I make, there's no one pointing to the ethical alternative next door."
                , Body """ didn't change my mind. He picked me up, and started up a conversation. I did not enjoy this. Leaving aside the fact that it was 4 AM and I had possibly stepped in someone's pee, the only thing he wanted to talk about was how boring a town Lincoln was. It's not that he was saying things he thought were interesting, but I found uninsteresting (like "Lincoln has more than four bars!"): he actually told me that he liked Lincoln because nothing much happened at all. Shit. I like when things happen, and I was going to be stuck here for fourty-eight hours. I wasn't too worried, after all, with my collegiate colleagues around, something interesting was guaranteed to happen. """
                ]
            , Plain """ I get to the AirBnB. A small, stout, brick house, sturdy and imposing but not particularly pretty. I open the front door, and I immediately begin to worry for my mental health. The entire house is "The Office" themed. The pillows on the couch all have Michael Scott's face. The posters on the wall are all quotes from The Office. (Later, I would discover to my horror that the shower curtain had a large watercolor painting of Prison Mike printed on it. This made pooping difficult, as whenever you sat on the toilet, you had to stare deep into the eyes of Michael Scott.) The AirBnB was not listed as a themed apartment, but they changed it between my reservation and us showing up. Also, the only bedroom left was the bedroom that has French doors: unfrosted glass windows let people in the living room see into my bed. Perfect."""
            , Plain """ The next morning, running on four hours of sleep, everything seems unreal. The only food in the house is a can of pringles, a box of cheerios, and milk. I eat cheerios, shower (hidden behind Michael Scott's face on the shower curtain), get dressed, throw my backpack on. We head to UNL's campus. On the walk, I find far too many things funny, as you are wont to do when you haven't slept. I laugh, and laugh, and laugh, at each one, until Ex tells me that I seem manic and slightly disturbed. I rein it in. We make it to the coneference without further incident."""
            , Plain """ I say this about many universities, but UNL is just a knockoff Northeastern. As we walk in, I see college students wearing red shirts with a large N as a logo. The only real difference from a Northeastern sweatshirt is the text above, but "Nebraska" looks a lot like "Northeastern" at first glance. But this is just one event, so we laugh it off and keep walking. We remember that UNL's mascot is the "Husker", a frankly terrifying cartoon man who husks corn. Northeastern's mascot is a husky. There are two parallels, this is starting to seem like a pattern. """
            , Plain """ We do philosophy for most of the day, and then Ex and I break off to work on a paper that was due a few days after the conference. We sit down on a couch that a nearby whiteboard informs us is named "Philosophy Couch", and get to work. Or try to. The second we sit down, the lobby burst into motion. We work for five minutes, then look up, and realize we are surrounded by whiteboards. We laugh, but the sort of worried laugh you do when you are unsure whether something that seems harmless is secretly going to cause your death in the next few minutes. We write another paragraph, and look back up: the whiteboards have been scattered back across the room, and people are hanging posters on them. A woman is waving a hair drying against a large, white sack. We discuss a third paragraph, decide it wouldn't add much to the paper, and check back in on the whiteboard situation. There are no whiteboards in the lobby. As we laugh another worried laugh, a man comes up to us, and asks if his amp and guitar are in an aesthetic place in the room. He has them set up in front of a massive screen, perhaps twenty feet wide, currently displaying UNL's homepage, which features UNL's cheerleaders wearing what must technically be called clothing. The monitor is displaying a lot of exposed skin. Given the square footage of exposed skin on the screen, I am almost certain it counts as pornography. I point out this fact, suggesting that it is not very aesthetic. The absurdity of the situation sinks in. I am sitting, slouched, on "Philosophy Couch" on UNL's campus, and strange men want me opinion about the aesthetic of their music equipment. I laugh, not at him, but certainly in his direction, until Ex tells me that I'm laughing manically again. """
            , Plain """ In the center of a lobby, a group of college students wearing all black are setting up a wide variety of percussion instruments. Gradually, as they get set up, an ethereal tune starts to haunt us. This is not music that makes you comfortable, this is music that tells you a ghost is about to kill someone in the horror movie you're watching. We look up again. The woman is still massaging the air balloon with the blowdryer. We decide we're not going to get any more work done, and try to head upstairs, to the room where we dropped our stuff. Along the way, we see a piece of art that can only be described as two robots breaking up. We get ready to head out to a restaurant, looking for a vegan restaurant. There are more vegan restaurants in Lincoln than in Denver, which seems backwards. When your primary export is beef, you'd imagine that restaurants would end up serving a lot of it. When your primary export is tech workers who smoke a lot, it's easier to understand the market forces that allow vegan restaurants to exist. """
            , TextWithFootnotes
                [ Body """ The food is good. Vegan restaurants are always really good """
                , Footnote 3 "Restaurants that serve meat can afford to be bad, because meat done poorly is still okay and many meat-eaters just want a meal, they're not looking for something exceptional. But vegan restaurants, because they're serving such cheap ingredients, need to stand out in other ways to convince you to spend money. Sautéed brocolli isn't going to make me fork over $10. Something interesting, with many ingredients, and good spices, and "
                , Body """. After dinner, we go to another restaurant. This restaurant also serves drinks, but we can't go to a real bar as Ex is still 20 years old.  """
                , Footnote 4 "Lame."
                , Body """. I am shocked at how cheap alcohol is here. Some of the beers on this menu (and they're nice, craft beers, with fun backstories) are five bucks. At nine, Roommate informs us that he's leaving because he has some work to do. We're undergrads, and so we follow him: we're used to the idea of leaving when the people your age start to leave. We are real adults, and we could have hung out with the philosophers, but our instincts lead us outside."""
                ]
            , TextWithFootnotes
                [ Body """ As we walk out, Orientation asks "are we done drinking for the night?", and I immediately tell him that we have at least four more hours of drinking in the night"""
                , Footnote 5 "We had seven more hours, in fact."
                , Body """. Our plan of leaving to do work is postponed, and we find a liquor store. The nearest liquor store is a half mile away"""
                , Footnote 6 "Fucking Nebraska."
                , Body """but we walk it. Once we've made it there, only half-frozen, we split up into underage person and someone to hang out (Ex and Roommate), and Orientation and I go inside. Alcohol in liquor stores is strangely similar in price to Boston and Denver liquor stores, for a place where restaurant alcohol prices mean that poor people can afford to be alcoholics too. When waiting in line, we see a wee lad, clearly not more than 19, holding a bottle of Smirnoff Raspberry. He asks the cashier, "Do you have this in... big?". They do. We laugh, buy our box of wine """
                , Footnote 7 "Rosé, obviously."
                , Body ", and we back to our AirBnB."
                ]
            , TextWithFootnotes
                [ Body "To make a long story short, we drink, play The Office Trivia Game™, and drink some more. We read passages at random from Peter Welch's book \"Observtions of a Straight White Male with No Interesting Fetishes\", and drink every time he says \"sex\". We drink pretty often, and pretty thoroughly. Ex goes to bed at 12:30 "
                , Footnote 8 "Which normally is early enough that I'll make fun of the person going to bed, but in this case, we had been drinking since 6, so I thought it was reasonable."
                , Body ", and Roommate and Orientation and I head out to a bar. We are trying to figure out which bar to go to, and discover another strange parallel between Northeastern and UNL: Boston has a bar called Tavern in the Square, Lincoln has a bar called Tavern on the Square. Obviously, this is a sign, and so we head out."
                ]
            , Plain "We make it to Tavern on the Square just before 1. I order a Ketel One double with ginger beer, not realizing that this is equivalent to asking for a mule until the drinks come in copper mugs. I know what a double mule tastes like, and this was not that. A Nebraska double includes far more alcohol than a Boston double. We drink, and talk, and drink, and I play some darts with strangers, and so on, and then we head outside. They have a Connect Four board that's about four feet tall, and we square up to play. It's important to know that when Roommate and I lived together, a Connect Four board lived on our dinner table, and when we had fifteen minutes of downtime we would talk while we played Connect Four. In other words, Connect Four ability was very important to both of us, and we always had a friendly competition going."
            , Plain "That night, Roommate kicked my ass at Connect Four."
            , Plain """A sidebar, to tell a story that happened on the side of this bar. I challened a girl to Connect Four, beat her (thus regaining some of my Connect-Four-based self esteem), and then we started talking. At one point, she asked if I lived in Lincoln, so I told her I was just here for the weekend. She ponders this for a moment, and then says "Okay, that means we can make out but not have sex". Then, she made out with me for about twenty seconds, and walked away, never to be seen again."""
            , Plain """ Last call rolls around. I was getting bored of yelling about philosophy with the high-school dropouts at the bar, and last call is as good an excuse as any to leave a bar. We head home. But the night is not even close to over yet."""
            , TextWithFootnotes
                [ Body """ Perhaps five minutes into our walk, in the bleak midfall, a pedicab driver """
                , Footnote 9 """Actually, a "Pedicab Chauffeur", according to his business card."""
                , Body """ pulls up. It's the weekend of Halloween, so it's not completely absurd that he's wearing a cow onesie with articulated udders. It's still slightly absurd, but not completely. We chat for hours. Or I should say, Orientation and Roommate chat with him for hours. I meet some strangers, including an Asian man who pretends he doesn't speak English, until he realizes his friends and I are going to be yelling about philosophy for a while, and then caves and joins the conversation. The first English he spoke to me was "Bullshit! You know that's impossible". I was taken aback. I thought, just for a second, that he had learned English in that moment, just to tell me I was wrong. I was impressed at his dedication, and then I realized he had been messing with me. After an hour of conversation, on a street corner, with a pedicab driver and three random folks (they claimed that one of them had just gotten married, but they hadn't given me much reason to trust them, and they were carrying take-out food, so I choose not to believe it), we head home. """
                ]
            , TextWithFootnotes
                [ Body "We make it home, argue about the trolley problem"
                , Footnote 10 "To Roommate, if you're reading this: you are deeply, hopelessly wrong about the trolley problem."
                , Body ". And that concludes the first day. But I still had twenty hours left in Lincoln."
                ]
            ]
        , Model "A Nebraska Double: Shot 2"
            "nebraska2"
            Listed
            [ TextWithFootnotes
                [ Body "I wake up deeply hungover. Through the French doors of my bedroom, I see the living room in disarray. Sitting on the coffee table, taunting me, is the previous night's box of wine. I get up, put it in the fridge, and start to consider the day. I make coffee"
                , Footnote 1 "This is Nebraska, so the AirBnB only has a Mr. Coffee, although surprisingly the coffee itself is from Peet's, which I consider to be quite good. I curse myself for forgetting to bring my AeroPress. I spend a few minutes contemplating my decision to bring only one backpack on this trip. Was it more motivated by my desire to prove to other people that I can travel light or the actual benefits of traveling light? I'm just about ready to indict my desire to travel light as strictly social signaling, and then the coffee is ready, and I stop thinking about it."
                , Body " and take a caffeine pill"
                , Footnote 2 "At this point, I had slept a total of eight hours over the previous two nights. "
                , Body ". I text everyone else, asking if they're awake. Ex stumbles downstairs, and it looks like someone has punched her in the face –\u{00A0}her lip is swollen and split. I ask what happened and learn that she has no idea either, but she woke up upside down in her bed, with her feet on her pillow, and blood everywhere. We try to piece together what could have happened last night after she went to bed, and idly speculate until Roommate shows up. He says that after we got home, he went upstairs and walked past her room, found her lying on her bed, light still on, and importantly with no blood to be seen anywhere. Seeing as in the morning, it looked like at least two-thirds of a murder happened in her room, it seems unlikely that he missed it. So we knew it happened after we got back from the bar (and the associated pedicab-driver-chatting and stranger-harassing), but we didn't know much else. We speculate a bit more, and then realize we're going to be late to Day 2 of the conference, so we head out."
                ]
            , Plain "On that day's walk in, we end up walking behind three UNL students most of the way in. I notice that they are completely ordinary people, very mainstream-looking. I am completely certain that they were not involved in the interpretive dance shenanigans from yesterday, and in fact, that if we suggested that interpretive dance happened on this campus, that they would not believe us."
            , TextWithFootnotes
                [ Body "We show up halfway through the only talk I was actually interested in seeing at the conference, which is unfortunate. I stay engaged through the second half of the talk, and quite enjoy it. The next talk, not so much. I take very aggressive notes, calling out the motte-and-bailey "
                , Footnote 3 "Google it."
                , Body """ argument, with the motte being "If AI is better than us, it will be different than us", and the bailey not even being a coherent enough thesis to recount here. I complain and complain and complain about this talk, in my notebook, doodle a fair bit, and then the talk concludes and I clap anyways """
                , Footnote 4 "When there are only twenty people in the audience, not clapping is conspicuous."
                , Body ". Then, the speaker starts heading back towards her seat. Prior to the walk, she was sitting next to me, so obviously she will be sitting next to me after the talk. I don't realize this, however, and keep sitting and pondering, notebook open to two full pages of scathing attacks on her talk. As the next talk starts, I look down and realize how clear my handwriting is, as I decry that one of her points \"Does not follow!\" and another seems \"Very motte and bailey :(\". I glance over at her, and see that she is looking directly at my notebook. Oh well. I flip to the next page, and start taking notes on the next talk."
                ]
            , TextWithFootnotes
                [ Body "Somewhere in there, Roommate left "
                , Footnote 5 "Didn't even say bye."
                , Body "."
                ]
            , TextWithFootnotes
                [ Body "Lunch is served, sandwiches "
                , Footnote 6 "On pretzel bread. Pretzel bread is deeply underrated in general, so I wanted to take this moment to appreciate it publicly."
                , Body " and chips and all the sorts of things you expect to be part of a free lunch "
                , Footnote 7 "Not quite a free lunch, really. It cost a whole trip to Nebraska."
                , Body ". One of the professional philosophers there was vegan, and someone else brought it up to point out the relative lack of vegan options. Normally, when people say they're vegan, I don't particuarly care. I'm sure that, if we talked for a few minutes, their argument for veganism would at least be inconsistent with some of their other views "
                , Footnote 8 "Note that this does not mean their argument for veganism is wrong, just that I can feel morally superior to them."
                , Body ". But with philosophers, I'm almost certain that any discussion about veganism would go the opposite way, with them pointing out the inconsistencies in my beliefs "
                , Footnote 9 "For instance, I believe that I am not an alcoholic, despite strong evidence to the contrary."
                , Body ". So I stay out of that debate, and eat my meat-laden sandwich in silent shame. Ex (who is vegan now, I think) asks if I want to eat the meat off of a sandwich, so she can eat the bread "
                , Footnote 10 "As previously mentioned, pretzel bread is the shit, so it is not surprising that she would want to eat the slightly soggy sandwich bread over chips or trail mix or whatever else they had that was vegan."
                , Body ". I say yes, and have just the meat and cheese of the sandwich. While holding just meat and cheese, I make eye contact with the vegan philosopher, and immediately turn away to hide my meat-and-cheese habits "
                , Footnote 11 "Despite my shame in this instance, I generally support easting just the inside of a sandwich. Bread is the worst part of any given sandwich. It's just there because it's cheaper than the other ingredients, and makes it socially acceptable to eat the sandwich with your hands."
                , Body "."
                ]
            , Plain "Lunch is over. We sit back down, sit through a talk, and then it's what the conference schedule describes as \"Social Hours (continue conversations, check out posters, etc)\". We head home, to drop off our backpacks and posters before dinner. As we walk in, Ex jokes, \"Well, time to start drinking again?\" Orientation and I do not realize this is humor, and pour ourselves some more wine."
            , TextWithFootnotes
                [ Body "We show up to the dinner buzzed and ready to talk about philosophy. We sit down, order our food "
                , Footnote 12 "I get the scallops, for two reasons. First, Northeastern was paying for this meal, so I felt someone obliged to get the most expensive thing on the menu. Second, there were vegans afoot, and scallops are some of the least meat-like meats."
                , Body ", and wait interminably for the food to arrive. Ex has to leave about an hour after we show up to the restaurant, so she gets her food to go. We order drinks "
                , Footnote 13 "If you're ever in Lincoln (unlikely) and at the Blue Orchid (more likely, once you're in Lincoln) and have someone else paying for your meal (unlikely, despite you being in Lincoln), I would highly recommend ordering three Ginger Flowers. And one for your underage (hopefully, under only the drinking age, and not any other relevant ages) ex. "
                , Body " and talk about philosophy. Someone else is severly drunk, which is always a relief, because it means you're not the drunkest person at the party. They have paper tablecloths, which some people find tacky but I love. We talk about Newcomb's paradox "
                , Footnote 14 "I'm a one-boxer."
                , Body ", and as we talk, we can scrawl decision trees on the paper on the table "
                , Footnote 15 "This both lets us communicate more clearly, and also signal to people outside of our conversation that we're having more fun than they are."
                , Body "."
                ]
            , TextWithFootnotes
                [ Body "We finish up dinner, and head out to a bar, or brewery, or taproom, or something. I continue to be shocked by the price of alcohol here. I order a marshmallow stout "
                , Footnote 16 "It tastes exactly like how it sounds."
                , Body ", which is 13% by volume, and it costs $7. Orientation and I sit with one of the younger philosophers, talking about the philosophy job market, how to teach, and various other academic-oriented ideas. Orientation and I leave for our AirBnB at 10:50 or so, an hour and a half before my train is scheduled to leave "
                , Footnote 17 "And you should be able to infer, from the fact that I said \"scheduled\", that it was delayed. "
                , Body ". We walk by the corner where we met the pedicab driver last night, laugh, and make it home with no event."
                ]
            , TextWithFootnotes
                [ Body "As we walk in, my phone buzzes "
                , Footnote 18 "Not really. My phone is always on do not disturb. I check my phone, and see the message. No buzzing occurred."
                , Body ", and I see that my train has been delayed."
                , Footnote 19 "Well, sort of. The message in the text explains that the train is expected to get in later, but it might be on time, and it may be delayed again. So really it's just warning me that life is uncertain, and not making any definite statements about the train."
                , Body "Of course, at this point, we're back in the same apartment as a box of wine, so obviously we resume drinking "
                , Footnote 20 "There are seven bottles of wine in a box, after all, and we had only drank about five of them."
                , Footnote 21 "And, the more I drank, the better I would sleep on the train."
                , Footnote 22 "Better here means not sleep quality but speed at which I would fall asleep, I wanted my train ride back to be completely uninteresting, and being asleep for all of it seemed like a feasible way to accomplish that."
                , Body ". Orientation and I talk about philosophy a bit more, and commit to writing a paper together"
                , Footnote 23 "Perhaps the most surprising part of this trip was we re-committed to writing this paper while sober."
                , Body ". Then, I head out for the train, because the text that Amtrak sent warning about the delay did not inspire hope that they knew when the train would arrive."
                ]
            , TextWithFootnotes
                [ Body "I arrive at the train station. Our front, there are three massive parking garages, with different colored lights illuminating each one. I appreciate the architecture for a moment. Then, I realize that spending just two days in Nebraska was enough to compromise my aesthetic sensibilities, and I was enjoying what were objectively quite boring-looking parking garages. I head inside the train station, a small rectangle with twenty or so seats on the benches inside. I check my phone as I walk in. The train is delayed again, and I have an hour and a half "
                , Footnote 24 "Of course, the text mentions that it may arrive earlier, or later, or at the exact time that they forecast."
                , Body " to kill. I want to go to a bar, but I don't want to miss my train, and these are conflicting desires. I check a map "
                , Footnote 25 "Okay, fine, Google Maps. But checking a map sounds much more impressive."
                , Body " and see that there is a bar just three minute's walk away. The train, when it shows up, will certainlyl take more than three minutes to board "
                , Footnote 26 "Or at least, that's what I convinced myself."
                , Body " and so, I could definitely get someone in the station to call me when the train arrives, and then make it back in time."
                ]
            , TextWithFootnotes
                [ Body "I ask around. I approach the people closest to me, a couple with a baby, and regale them with my story: I tell them I want to go to a bar to wait for the train, and that if they could call me when it arrives, I would be able to make it back in time. The husband "
                , Footnote 27 "I don't know Nebraska's rate of extramarital pregnancy, but I'm hoping it's low, so I'm going to assume the couple was married. "
                , Body " says \"I'm sorry, but I have a baby\", as if fatherhood was a legitimate excuse to not enable a stranger's alcoholism. The next person down the row, a woman with a large camo-patterened duffle bag that looks vaguely military, says she doesn't have a phone. She is actively using her phone. Whatever. The next person in the row is an old man "
                , Footnote 28 "Although keep in mind, I am 21, so when I say \"old man\", I mean \"over 40\"."
                , Body ", who tells me he doesn't approve of drinking."
                ]
            , TextWithFootnotes
                [ Body "Well, I don't need their help anyway. I head out, and three minutes later, I've made it to a place that the people of Nebraska call a bar. It is ramshackle. I ordered a greyhound, and the bartender pulled out a bottle of Svedka, and at that moment, I immediately started doubting my decision. This bears repeating: their well vodka was Svedka. I sit at the bar, writing in my notebook, realizing that this is the first time I've ever been in a bar alone. I don't mind it at all "
                , Footnote 29 "Which is probably a problem."
                , Body ", in fact, I quite enjoy exchanging glances with the girl sitting on the other side of the bar, in between moments spent writing. I'll skip over my whole conversation with her, which revealed that she was Nebraskan and existentially unhappy about it, and skip to the good part: I made it back to the train station right as the train was pulling up. Everyone else in the station looked at me with disgust. Clearly, they had been hoping that I would miss the train because I was at the bar, thus getting my comeuppance, but I ruined that for them. I board, and fall asleep rather immediately."
                ]
            , Plain "I wake up six hours later. I wake up looking out the window, and the rolling hills of northeastern Colorado wave up and down, white with snow on one side, and the light brown of late-fall grasses on the other. The sun is rising, the clouds are a brilliant, piercing orange, and the whole sky is choosing to take part in the rainbow that normally is relegated to the areas near the horizon. I stand up, dazed, and stare out of the back window of the train, watching it tear over the tracks as the sun slowly meanders its way up, the clouds lose their brilliance, and the rainbow fades. I sit back down, deeply content, and thoroughly sleep-deprived. An hour later, as we pull into Union Station, and I laugh and laugh and laugh, silently, to myself, at what a weekend this has been."
            ]
        , Model "High School Sucks"
            "high-school-sucks"
            Listed
            [ TextWithFootnotes
                [ Body "As a software engineer, I am pampered "
                , Footnote 1 "Breakfast is served in my office everyday. Lunch is usually free, whether catered or delivered. I can leave early if I want, and no one cares if I'm late. And, most importantly, the office has a ping pong table."
                , Body " because the people in charge want to maximize my output and keep me around. High schoolers, on the other hand, are not exactly pampered."
                ]
            , TextWithFootnotes
                [ Body "I think there's something contradictory about those two facts. Surely, we behave as though we want high schoolers to be successful, to learn a lot, and to be reasonably happy "
                , Footnote 2 "People's definitions of reasonably differ a lot. For example, Gunn High School, near where I grew up, only started thinking about it's students emotional health after five students committed suicide in a year. Before that, they assumed that pushing high schoolers to be perfect in their school work while also volunteering and being on a sports team and assigning many hours of home work a night would make them reasonably happy."
                , Body ". If any of those three things are the case, we ought to take better care of them. While there are some social programs that give (poor) high schoolers free lunches, it is framed as an ethical requirement, not an economic benefit. We force them to sit in classes, require that they ask permission to use the bathroom, and they certainly don't have any ping pong tables. People don't think there are self-interested reasons to provide high-schoolers with supportive environments."
                ]
            , TextWithFootnotes
                [ Body "But, high schoolers eventually graduate, and they do go on to their jobs. Generally, we believe that education will make people more productive. And when we talk about education making them more productive, we don't mean that sitting in a chair in a classroom helps, we must be talking about some combination of socialization and learning. Neither socialization nor learning is maximized by the current high school system. If you believe that software engineers are being made more productive by all the benefits they are given, we should give high schoolers similar benefits. But it may not be that these benefits are given to improve productivity. It's possible that the reason companies offer these benefits is to be seen as a more attractive place to work. In which case, assuming you want high schoolers to show up for school, you ought to give them the same benefits. There is no fundamental difference between a software engineer's set of motivations and a high schooler's, so if having these benefits at some location makes one more likely to show up to that location, you can assume it'll be true of the other."
                ]
            , TextWithFootnotes
                [ Body "Perhaps people don't believe that the purpose of high school is to educate. Suppose the purpose of high school is to keep young criminals out of trouble for some part of the day "
                , Footnote 3 "An argument that Bryan Caplan makes pretty compellingly in \"The Case Against Education\"."
                , Body ", surely the more appealing you make high school, the less appealing you make skipping school?"
                ]
            , TextWithFootnotes
                [ Body "There are many reasons to believe that primary school isn't about education"
                , Footnote 4 "And many reasons to believe it about college, but I won't go there today."
                , Body ". For example, Direct Instruction, a teaching paradigm, has been shown to have better outcomes for students than standard high school classes. But no public high schools have adopted it "
                , Footnote 5 "There are some principled objections. For example, the argument that DI is \"teacher-proofed\" and so cannot allow excellent teachers to help students as much as they could otherwise. I'm glad the public school system thinks that having a few excellent teachers makes up for the rest."
                , Body "."
                ]
            , Plain "No one is quite sure what the purpose of high school is. Colleges value doing well in high school, because it predicts future success pretty well. But the people in charge of running high schools have forgotten that they could serve a purpose other than improving your chances of getting into a college. "
            ]
        , Model "Bullet Journaling"
            "bullet"
            Listed
            [ TextWithFootnotes
                [ Body "Bullet journaling is a mindfulness practice, where you sit down once a day to write down the tasks you want to accomplish in that day, and the events that will happen that day, and you add notes throughout the day "
                , Footnote 1 "There's more to it, at least as it's written in their official dogma, but this is the general idea."
                , Body ". During the day, you can plan around the tasks you should be doing and the events that will happen. I started using this system "
                , Footnote 2 "Well, my variant on it."
                , Body " in January 2019 and have quite enjoyed it. Bullet journaling helps me follow through on my commitments, achieve long-term goals, define whether a day was successful or not "
                , Footnote 3 "That's right, motherfucker. This is a high-school-style five-paragraph essay."
                , Body "."
                ]
            , TextWithFootnotes
                [ Body "Bullet journaling helps with follow-through for somewhat obvious reasons. My journal is structured like this: 53 pages, one per week of the year "
                , Footnote 4 "You probably think there are 52 weeks in a year. While this is not technically false, 52 * 7 is 364, so there will always be 53 weeks in a year."
                , Body ", followed by empty pages, which are filled up with my daily to-do and event lists "
                , Footnote 5 "This is a lie. In reality, the first half is structured as one weekly page, followed by seven daily pages, followed by the next weekly page, and so on. But in the second half, I pulled my shit together and wrote all the weekly pages in a row. This is nice because I can reference them all easily, and plan for the future, but annoying because I have to flip back and forth over many pages. But my next bullet journal will be 53 weekly pages followed by daily pages."
                , Body ". Whenever I pick up some new responsibility, I write down the date that I have to take each action it required. So, for example, if a friend tells me he's going to fight lions on October 13, and needs me to film it, I'll write down on whatever week October 13th falls on that I have to film a lion fight. When the week rolls around, I'll see this committment, and know that I'll be a bit busy that Friday. Sometimes, though, you have obligations that are less specific. For example, suppose someone tells me that I have to help them burglarize a house at some point in June. In this case, I'll make a note in the page for some week in May to follow up and ask for details "
                , Footnote 6 """For example, "Which house?", "What's in it for me?", and "How are you not in jail?" """
                , Body ". And when I get a new deadline, I'll write down not just the deadline but also multiple steps and the times I should do them. For example, for my PhD applications, I saw they had deadlines in late December through early January. So in a week in August, I wrote that I should decide which schools I'm applying to, in a week in October, that I should have finished my writing sample "
                , Footnote 7 "Full disclosure: It is November, and the writing sample is still not finished."
                , Body ", and so on, so that each task gets done at the right time, and when December rolls around I'll be ready and unstressed "
                , Footnote 8 "Leaving aside the previously mentioned unfinished writing sample."
                ]
            , TextWithFootnotes
                [ Body "Achieving long-term goals follows pretty naturally from writing down obligations. When I have some long-term goal that I want to achieve, I can split it into small steps, and achieve each of these as they come up in the journal. The fundamental algorithm for achieving hard things"
                , Footnote 9 "Which I stole shamelessly from someone on the internet."
                , Body "is as follows:"
                , NumberedList
                    [ "Find something that is like the hard thing but is easy."
                    , "Modify the easy thing so that it is like the hard thing in exactly one way that you find hard."
                    , "Do the modified thing until it is no longer hard."
                    , "If the original hard thing is now easy, you’re done. If not, go back to step 2."
                    ]
                , Body "Each time you have a new easy thing that is like the hard thing in one way, you can write tasks in your bullet journal to do that thing. Like the example I gave with my PhD applications. Applying to PhD programs is difficult, but deciding which schools would be interesting is not. So, write that down as a  task. Then, the next step is to create a writing sample "
                , Footnote 10 "Writing the writing sample may itself be difficult. But one of the benefits of the general algorithm for achieving hard things is that you can apply it recursively. If modifying the easy thing so that it is like the hard thing in exactly one way is itself a hard problem, you have a whole algorithm just for solving hard problems ready at hand."
                , Body ". Repeat a few more times, and you've done your whole PhD application "
                , Footnote 11 "Keeping in mind that I am writing this essay while procrastinating on finishing the writing sample."
                , Body "."
                ]
            , TextWithFootnotes
                [ Body "The most underrated benefit from bullet journaling, though, is being able to define a day, or week, or year as successful. If you write down your goals for each day in the morning, then in the evening, you can see whether those goals were met or not. If they were, congrats! You've had a successful day. If not, do better tomorrow. It's critical, though, that if you are writing down tasks for your immediate goals and tasks that will eventually achieve your long-term goals, simply following the to-do list in your bullet journal lets you achieve everything you need to! Additionally, you're going to have to complete the tasks that weren't completed eventually. Ending a day with tasks unfinished means more work tomorrow, on top of being unsatisfied with today. So writing something in my bullet journal helps me get it done, because once it's written, I am committed to doing it, and may as well just get it done today. Bullet journaling doesn't just define success, it helps you achieve it."
                ]
            , TextWithFootnotes
                [ Body "I know I said this was going to be a five-paragraph essay, but conclusions are useless when someone can just reread your introduction "
                , Footnote 12 "The reason they're required in high school because high schools are based on colleges, and the reason they're required in colleges is because colleges used to exist to train people to give better legal arguments, and legal arguments at the time were oral, not written."
                , Body ", so I'll end it here."
                ]
            ]
        , Model "The Coolest Kid at the Hackathon"
            "coolest-kid-at-the-hackathon"
            NotListed
            [ TextWithFootnotes
                [ Body "I was the coolest kid at HackBeanpot 2019 "
                , Footnote 1 "Admittedly, being the coolest person at any hackathon isn't that hard. I'm 100% sure that writing about it on your personal website helps."
                , Body ". The hackathon itself was held February 8-10, in the Seaport area of Boston "
                , Footnote 2 "If you're imagining tall brick buildings on the waterfront, you're right on."
                , Body ", and I was there because my friend invited me. I had known this guy for about a year. He was the friend of my old randomly-assigned college roommate, and he was a big data science/machine learning nerd, so let's call him DataScience. Working with us was a guy DataScience knew. I remember only two facts about him: he was a freshman, and his last name had far too many X's in it "
                , Footnote 3 "I think it was only one X, but it was at the beginning of his last name. That's excessive."
                , Body ". Call him X "
                , Footnote 4 "Or Freshman. I don't care what you call him, and he is not an important character in this story."
                , Body "."
                ]
            , TextWithFootnotes
                [ Body "We spend about an hour hanging around DataScience's apartment, figuring out what project we want to work on. We get bullied pretty quickly into making a website that lets you visualize and explore the data you can export from Facebook. Then, we gather supplies. I have a laptop, and so I'm set. DataScience brings an external monitor, and a keyboard, the whole nine yards "
                , Footnote 5 "Or at least, the whole two peripherals."
                , Body ". We decide we're ready, call an Uber, and make bad jokes about programming the whole way there."
                ]
            , TextWithFootnotes
                [ Body "We show up to the Hackathon, and listen to people telling us about the rules we should follow and how to have good sportsmanship and so on. We idle impatiently near the door to the work area. When the introduction is over,  they let us in to the area where we are going to be working for the next 48 hours. We are, obviously, one of the first groups in, and push towards a nice room in the back "
                , Footnote 6 "It shocks me, as always, how many people listen to opening speeches so far from where they need to be immediately after the speech ends. Surely, you must get tired of being stuck in the crowd of people pushing through doors?"
                , Body ". We set up our room, and start drawing plans on a whiteboard."
                ]
            , TextWithFootnotes
                [ Body "People drift in and out of our room, asking questions and telling us about events "
                , Footnote 7 "Hackathons always have a thousand different events. Yoga? Really? How is that going to help me make fun of Facebook? The lectures on a programming tool I understood completely, but the sparkling water blind taste test challenge I did not. "
                , Body " that are happening. We hack and hack and hack into the night. Pizza comes. Normally, pizza arriving at a hackathon is not worthy of a sentence in a description of that hackathon "
                , Footnote 8 "But clearly, I think that descriptions of the normality of pizza at hackathons is worth a sentence."
                , Body ", it's just to be expected. What is incredible about this particular arrival-of-pizza scenario is the sheer quantity of pizza. This hackathon has maybe 200 people present. The pizzas are stacked on a counter in the kitchen, a rectangular prism "
                , Footnote 9 "A word that means \"3D rectangle\" and also \"I want to brag about my math education\". "
                , Body " two boxes wide, seven boxes long, and ten boxes high "
                , Footnote 10 "I counted."
                , Body ". Doing the math, that comes out to 140 boxes of pizza, for 200 people. This is my dream. I take two boxes "
                , Footnote 11 "One meat-lover's, one vegetable-heavy pizza that also included meat. "
                , Body " back to my team's room, and we keep working. At 3AM, I call it for myself, head home, and crash hard. X leaves with me, but DataScience stays there. He gestures frantically at the Google Sheet we made earlier that day, which demonstrates our work/sleep schedules for the weekend. Apparently, it indicates that he is staying there and working until 7AM, a mild insanity which I am far too tired to argue about. "
                ]
            , TextWithFootnotes
                [ Body "I wake up the next morning, shower "
                , Footnote 12 "Unlike many other people attending the hackathon."
                , Body ", and take the train in. On the train with me is my old TA, someone who I never got close to but argued with constantly during labs "
                , Footnote 13 "Which were only labs in the weakest of senses. Computer Science labs always seem a bit forced. The purpose of a biology or chemistry or geology lab session is the lab building – you cannot perform these experiments anywhere else. The purpose of a computer science lab is to force you to write some code, and maybe to have TAs make fun of you while you do it. Imagine an \"English lab\". No one would take it seriously. But somehow, computer science, by virtue of having science in the name, gets away with it."
                , Body ". We chat a bit about the hackathon. I ask him if he goes to hackathons often. He says yes. He asks if I do. I say I don't. I ask him if he graduated. He says yes. I say \"cool.\". It's the sort of conversation that doesn't communicate any information, but makes you feel that you followed a social norm. Normally, this feels good, but I had slept around five hours the previous night, so it just made me feel more tired."
                ]
            , TextWithFootnotes
                [ Body "We eventually make it in to the hackathon. I go in to the room, and find DataScience passed out on the couch. I sit outside, working on my laptop, trying to decipher the changes he made after I went home last night. The last commit is at seven AM "
                , Footnote 14 "And, amazingly, it has a coherent message and no obvious bugs. My 7AM commits tend to have messages like \"code\" and \"it works now\", and include obvious bugs with comments about how I don't care."
                , Body ". I pick up where he left off. I write code for most of the morning, with X once he shows up. I wander around fairly often, to stretch my legs and rest my eyes, and ask other teams what they're working on. I am unsure whether I am flirting with the girls on the teams, partially because I'm at a hackathon, which is a non-flirting-friendly place, and partially because I'm never sure if I'm flirting with anyone."
                ]
            , TextWithFootnotes
                [ Body "At four, I put a piece of paper on my tongue "
                , Footnote 15 "Hopefully, this is enough to count as plausible deniability."
                , Footnote 16 "Reason 1 why I was the coolest kid at the hackathon."
                , Body ", and coding becomes much more interesting. I can see the abstract syntax tree of the code I am writing, and see links between different pieces of the code that connect together, but my memory gradually deteriorates and typing becomes difficult, because I am too interested in watching the letters on each key swim around."
                ]
            , TextWithFootnotes
                [ Body "Now is probably the best time to explain why I was eating paper. That night, I was going to a sorority social "
                , Footnote 17 "Not typically my type of event, but I was invited, and it would be churlish to refuse."
                , Body ", and instead of drinking "
                , Footnote 18 "Because I was going to a sorority event, I wanted to make sure that I wanted to be edgier than the other people there, so I could assure myself that I wasn't one of them."
                , Body ", I picked a different drug. I take a Lyft home, and stare at the driver until she stops trying to talk to me. I stare out the window the rest of the drive. We go over a bridge, as the sun sets"
                , Footnote 19 "It was 5PM. Fuck daylight savings time."
                , Body "and I notice that everything looks like a painting, everything looks beautiful, and I smile to myself and enjoy the rest of the drive home."
                , Footnote 20 "The Uber driver rated me five stars, somehow, despite how strangely I was acting."
                ]
            , TextWithFootnotes
                [ Body "I get home, and change into my sorority social outfit, which is, unsurprisingly, different from my hackathon outfit "
                , Footnote 21 "My outfit for the social included a red velvet turtleneck. The other men at the pre-game were very impressed that I knew how to dress. They were all wearing navy suits, and clearly didn't realize that men's fashion could be anything else."
                , Body ". I step into my room, changing, and then step back out into the living room "
                , Footnote 22 "Or the twenty square feet of space that are considered a living room in Northeastern dorms"
                , Body " and notice the bathroom door is closed. I am slightly concerned about this fact, because it was open when I walked into my room. Of course, I am quite high at this point, and I could be mistaken, but I start to get concerned. After a brief moment that feels like an eternity, one of my roommates steps out of the bathroom. I scream a little, then calm down, then tell her "
                , Footnote 23 "Yes, I have had female roommates. For some people reading this story, this is the most surprising part."
                , Body "I walk ten minutes or so to my girlfriends place, where she is hosting a pregame "
                , Footnote 24 "Or \"presocial\", but that sounds more like a phase of infant development than a type of party"
                , Body "."
                ]
            , TextWithFootnotes
                [ Body "In the distance between my apartment and hers, nothing interesting happens "
                , Footnote 25 "Which is quite strange, for a trip (of the walking kind) during a trip (of the drug kind)."
                , Body "."
                ]
            , TextWithFootnotes
                [ Body "I arrive and stride in, boldly "
                , Footnote 26 "The front door of her apartment building has conveniently had a broken lock for several months, at this point. It makes striding in boldly much easier."
                , Body ". Showing up to a party and immediately walking into a girl's bedroom with the girl in tow gives you some respect from everyone else there "
                , Footnote 27 "Except for Keith. But fuck that guy."
                , Body ". I give her a piece of paper of her own, and take another half of one myself "
                , Footnote 28 "No one ever said undergrads made good decisions."
                , Body ", and we rejoin the party. No one knows. This is the pregame for a sorority event, so everyone is quite drunk. I know too many people here. One guy tries to talk to me about entrepreneurship and a startup idea I mentioned in passing to him "
                , Footnote 29 "Remember Keith, from footnote 27? This is why I don't like him."
                , Body ". Someone tries to Ice me. I take the ice, and walk away, somewhere between confusion (what a strange world we live in, where one type of drink demands immediate drinking) and judgement (fuck these frat-type people). I notice how parched my throat was, and how nice the cold Ice tastes. I drink four more over the next hour. This was a bad decision."
                ]
            , TextWithFootnotes
                [ Body "Slightly tipsy "
                , Footnote 30 "An understatement."
                , Body "and severely high, I notice that everyone is being shooed out the door. The check-in for the social is starting. In my confusion, I had forgotten for a moment that each person going to a sorority event has to check-in at a place on campus before the event. This is to ensure that everyone is sober enough to make it through the night without embarassing the sorority "
                , Footnote 31 "If you are a member of a group that encourages (underage) drinking, at least pick a group that won't shame you for (underage) drinking."
                , Body ". So all forty of the people who were at the pregame head out. We form a line of well-dressed and warm men and scantily-clad "
                , Footnote 32 "But, some would say, still well-dressed."
                , Body "and cold women as we stream towards the auditorium where we have to check in."
                ]
            , TextWithFootnotes
                [ Body "The auditorium has a bad aura "
                , Footnote 33 "I only see or believe in auras while I'm tripping. But in that moment, I knew it was not where I wanted to be."
                , Body ". The check-in table is set up in the business building "
                , Footnote 34 "Already, a bad sign."
                , Body ", and, the second I step in, I feel overwhelmed. Hundreds of drunk freshman throng the lobby, chattering and prattling and jabbering and blabbering. I overhear a girl talk about breaking up with a guy because he cheated on her with her sister. Before I realized she meant sorority sister, I felt very sad on her behalf. After realizing, I just felt regular amounts of sadness. My date and I push forward, trying to get to the desk. As we enter the crowd, I look her in the eyes, and say \"If I lose you in this crowd, I will lose my mind\". I meant it. I grab her hand, and hold it tightly, and she pulls me forward. We make it to the desk, sign in, and rush back outside. But it's too cold outside, so we wait in the airlock between the lobby and the outdoors. Here, we overhear a few more comical stories, meet up with our buddies, and head out."
                ]
            , TextWithFootnotes
                [ Body "You may be wondering what I mean when I say buddy. Each person in the sorority brings a date "
                , Footnote 35 "Or tries to."
                , Body ", and pairs of dates are buddied up. Neither pair can get in to the venue without the other, and the pairs will get in trouble if the other pair doesn't sign out at the end of the night. Our \"buddy\" is a small, energetic Asian freshman, who reminds me in many ways of a girl who was an intern at the same company as me, the summer after my freshman year. Over the course of the night, she earns disapproving looks from three different bouncers, show us her (unconvincing) fake ID, and makes emphatically sure that we know she loves Northeastern."
                ]
            , TextWithFootnotes
                [ Body "We arrive at the venue. The bright gold sign proclaims that the venue is named \"Gilt\". I had been hearing it as \"Guilt\" until this point, and I am given a brief moment of clarity in an otherwise very confusing night. The line is long, and cold, and hilarious. People in front of us occassionally stumble as they walk, and the most egregious stumbles, the stumbles that result in falling over the divider that creates the line, results in people getting kicked out. I'm having a grand old time. I get patted down at the entrance. The bouncer asks me if I have any nips on me. I resist the obvious joke that I have two. After investigating most of my pockets, he lets me in. I don't have any alcohol on me, but I do have three more pieces of paper in my wallet." ]
            , TextWithFootnotes
                [ Body "We show up, and I try to dance, but my heart just isn't in it. There is a feeling of judgement, that dancing at this party isn't doing anything useful, that the music is too loud, that the people are valid, that the entire Greek life system is fundamentally flawed, and so on "
                , Footnote 36 "This sentence uses passive voice, but I'll admit in the footnotes: I'm the person doing the judging."
                , Body ". I loiter on the side of the dance floor. My date and I discuss leaving, and then agree we should leave. We walk out, as people in the line are still coming in. A girl who is in the class I TA sees me, and says hi. I am unhappy with this notion. I wave, and we head out."
                ]
            , TextWithFootnotes
                [ Body "Choosing to leave the event after a mere five minutes inside makes me feel superior to the people who are going to spend more than five minutes there "
                , Footnote 37 "I'm going to not consider what it says about me that I show up to events that I feel good about leaving."
                , Body ". We call an Uber, and as we're waiting for it, see two girls who are barred from entering. The bouncers say that they are too drunk, and kick them out. They walk up the street towards where we are, and hide behind us, afraid of the people in line seeing them. We offer to share our Uber with them, as we're heading to similar places, they agree"
                , Footnote 38 "Well, my date offers. I am nowhere near this kind, and also think it would be rather creepy if I asked two young drunk sorority girls to share an Uber with me."
                , Body ". The Uber arrives, and we drive back. It was horrific. The girls share the story of not being let in, from their perspective. Their perspective was wrong. They begin the ride by saying that neither of them had drank anything "
                , Footnote 39 "In which case, it was rather confusing that they were slurring the words in their defense of their sobriety."
                , Body ", and that they didn't want to go to the social anyway "
                , Footnote 40 "In which case, it was rather confusing that they showed up at all. On the other hand, I was there, despite not really wanting to be, so I can't really make fun of them for this one."
                , Body ". My date and I exchange glances through the rear-view mirror. She consoles them a little. The next iteration of their story, they just had one shot each"
                , Footnote 41 "Shots, famously, being the best way to consume small amounts of alcohol over a long period of time"
                , Body "and they are slightly bummed about not getting in, but mostly worried about their buddy "
                , Footnote 42 "I don't think I said this last time I mentioned the buddy system, but it's paternalist bullshit."
                , Body ". I believe this story slightly more. My date talks about how they'll be laughing about this in a year "
                , Footnote 43 "I certainly am laughing at them a year later, but I don't think this is what she meant."
                , Body ", and not to worry about it. They tell us the next iteration of their story, in which they consumed slightly more alcohol, and feel slightly sadder. But they make sure to blame the bouncer for being a dick, because underaged drinking is expected at sorority events, so they didn't really do anything wrong. This pattern repeats for a few more iterations, and then we arrive "
                , Footnote 44 "Thank god."
                , Body "."
                ]
            , TextWithFootnotes
                [ Body "My ex and I walk in to her apartment, lie down on her bed, and each eat some more paper. Just as I place it on my tongue, there's an urgent pounding at the door. Because I'm high, I assume that the police have found out about us. I start worrying. My date is in the bathroom, so I get up and go to the door. I open it, and it's just a normal guy"
                , Footnote 45 "Actually he was 6'9\": the Northeastern baseball team lived in the same building, so everyone weas freakishly tall."
                , Body ". He tells me there's a fire in the building, and we have to evacuate. I don't hear any fire alarms going off, and we're on the first floor near the door, so I'm not too worried, but I tell my date to hurry up in the bathroom anyway. I pull my shoes on, grab my jacket, and loiter near the front door watching the scene unfold. Various people in various states stumble out the front door. A few minutes in, a fire truck pulls in, and two firemen in full fire gear burst through the front door and run upstairs. At this point, I yell at my date that we really have to be getting a move on, because there's a real fire; she and I run out. Waiting on the street, we are captivated by the beautiful lights on the fire truck and ambulance, and talk about how crazy it is that this night, of all nights, would be the night her apartment building burned down. Her roommates walks out a few minutes later, with a backpack and talking about how she brought her laptop and passport and other valuables out of the building. We feel like idiots for not bringing anything "
                , Footnote 46 "Well, she does. All my stuff is in my apartment, a 10 minute walk away."
                , Body " but it's far too late to go back into the building."
                ]
            , TextWithFootnotes
                [ Body "One of my exes, let's call her Russia, walks out of the building. Well, ex is not strictly the correct characterization. She and I are no longer seeing each other, but at the time I was alternating nights between my date and Russia "
                , Footnote 47 "Russian happened to be my former student, and was at the time TAing the same class as me. It was a strange semester for my sex life."
                , Body ". She and I see each other, and say nothing "
                , Footnote 48 "We had a relationship based on mutual disrespect."
                , Body ". We awkwardly glance at each other for a while, but mostly we're concerned with the building on fire next to us. It's cold out, so my date and I decide to head to my apartment. We make it as far as the street corner, before we encounter Russia and her friends. Fortunately "
                , Footnote 49 "Unfortunately."
                , Body ", my date knew one of my ex's friends, so my ex and I have to studiously ignore each other while they talk. Then, mercifully, we head opposite directions."
                ]
            , TextWithFootnotes
                [ Body "We make it back to my apartment. One of my date's friends wants her to come hang out. There is some drama, someone offended someone, and my date feels the urge to apologize in this moment. Because she's very high "
                , Footnote 50 "And, because I want to have sex with her."
                , Body ", I tell her that this would be a bad idea. She starts panicking in a serious sense, hyperventilating and being unable to form complete sentences. I talk her down from the panic, and we drink some water, and then we merge souls"
                , Footnote 51 "This is a metaphor for having sex on acid."
                , Body ". We come down a bit, hanging around and drinking water. We go back to her place. It wasn't on fire, someone on the third floor just burnt something while cooking. We toss and turn and toss and turn and eventually fall asleep."
                ]
            , Plain "And the next morning, I wake up at 7 AM, and go back to the hackathon."
            ]
        , Model
            "How to Get an A+ in Algorithms"
            "how-to-get-an-a-in-algorithms"
            Listed
            [ TextWithFootnotes
                [ Body "Many systems at Northeastern have poorly-designed incentive structures. The Algorithms course is one particularly egregious example. I got a nearly-perfect grade in that class, primarily because I understood the incentive structures better than most other students."
                ]
            , TextWithFootnotes
                [ Body "Of course, I did in fact study and learn in that class. I use Anki, a spaced-repetition flashcarding tool that makes classes much easier. Each lecture, I would record a few (usually around a dozen) notes, the facts that I figured I ought to remember for the rest of the semester, and make a flashcard for each of those facts. This certainly helped, but I still made several mistakes on the first homework assignment. I understood the material well, but it is easy to make small mistakes while doing math, and even easier to make mistakes while doing math you don't care about. " ]
            , TextWithFootnotes
                [ Body "So, I thought about the way that the homeworks are graded. At office hours, a TA told me that they give you the points if anythign in your answer is correct. This is a terrible idea. In my future homeworks, I would sometimes answer a question two different ways"
                , Footnote 1 "Many of the questions were ambigious enough that there were multiple correct answers, making this less insane than it sounds."
                , Body "and if one of the two was correct, the TAs would mark it correct and give me the points, even as they crossed out the other half of the answer. When a proof was assigned, I would write pages and pages of inscrutable prose, with the correct answer almost definitely in one of the statements I proposed. I would lead with the answer I believed to be most likely to be true, and then write a whole lot more. Why not? If anything in there is correct, I would be given the points, so the more I wrote, the higher the expected grade. For the rest of the semester, I continued this strategy of proof by intimidation, and my grades rose to reflect my deeper understanding of the grading system "
                , Footnote 2 "Not the material. But then again, what college class is actually about the material?"
                , Footnote 3 "Fundamentals of Computer Science is actually about the material."
                , Body "."
                ]
            , TextWithFootnotes
                [ Body "This makes me an asshole. One of the TAs of the course, who I was quite friendly with by the end of the semester "
                , Footnote 4 "This sounds like I slept with him. I did not sleep with him."
                , Body ", told me that he hated grading my homeworks. I explained why I was incentivized to write what amounted to a treatise on each problem, and he grudgingly agreed that it makes sense. Still, I forced him to read far more pages of mediocre solutions to mediocre math problems than anyone ought to be forced to read "
                , Footnote 5 "And it wasn't even typeset or in nice handwriting. I gave them the full cursive treatment. This is because I also understood that if everything I was saying seemed right, and reading it was difficult, the path of least resistance for a grader is to give up and give me a good grade. This is how I got through most of high school."
                , Body ". I would go to his office hours once a week, and we would collectively struggle through my handwriting and hazy memory of the previous week "
                , Footnote 6 "This was a semester where I was drinking most Thursdays, Fridays, and Saturdays, so my memory was weaker than usual. And even usually, I have a terrible memory."
                , Body " to try to find the correct answer in the slew of answer-like things I had written."
                ]
            , TextWithFootnotes
                [ Body "The office hours were absurd for another reason. The TAs were not given the answer key to the homework, but they tended to know the answers to the homeworks "
                , Footnote 7 "Although when they lead the review sessions for the tests, they tended to tell us that most of the information on the test was bullshit we would never use, to justify why they didn't know the answers to the questions on the test review. While I appreciated the honesty, this was not the time for that type of cynicism; I needed to get an A+ in the class so that I could be taken seriously when I wrote a snarky piece about it later."
                , Body ". And, they were fully authorized to tell you whether or not an answer was correct. So, I would show up to office hours with several reasonable answers to each question, and run them by the TAs until one was confirmed to be the right idea. Then, I would demonstrate it was true, complete the steps of the proofs, or draw the pretty picture  "
                , Footnote 8 "Fuck doing graph theory problems on paper."
                , Body " that was required to get the correct answer. But importantly, I only had to do that work for answers I knew were correct, because I could confirm I was headed in the correct path. This saved a significant amount of time on the homeworks, time that was better-spent writing down four alternative solutions that were all along the path the TA described."
                ]
            , TextWithFootnotes
                [ Body "It may be a surprise, given everything else I've written here, but I enjoy learning, and think it has immense intrinsic value. I just also see the incentive structures behind (and instrumental value of) getting good grades. I learned a lot during this class "
                , Footnote 9 "And ended up hanging out with two different girls to study (and not study), an unusually high number for a computer science class."
                , Body " and ended up enjoying it quite thoroughly "
                , Footnote 10 "Although I won't speculate on whether I enjoyed the class because of the girls I met or because of the things I learned."
                , Body "."
                ]
            ]
        , Model
            "Stop Inviting Me to Bible Study"
            "bible-study"
            Listed
            [ TextWithFootnotes
                [ Body "I've been living in Denver for four months. Mostly, it has been like any other city, but more people smoke weed "
                , Footnote 1 "This is an oversimplification. More people have tripped acid, also."
                , Body ". Denver is a revelrous city, with drunk people whirring past on electric scooters "
                , Footnote 2 "Boston really needs to get its shit together with respect to e-scooters."
                , Body " pretty much constantly after 9PM "
                , Footnote 3 "Or, to be honest to Denver's drinking culture, 6PM. My coworkers and I go out for drinks after work, and it turns out that two drinks at 5280 feet above sea level is equivalent to several bottles of wine at sea level. I got the opportunity to test this fact when I visited Boston for my 21st birthday. Drinking at sea level felt like a marathon. I drank a bottle of wine to warm up, had a few Mike's Hard Lemonade's, and then went out and had several drinks at several bars. I stayed vertical. After four drinks at altitude, I get horizontal."
                , Body ". But strangely, despite the higher rate of revelry per capita, people keep asking me to if I want to go to  bible study."
                ]
            , TextWithFootnotes
                [ Body "I am severely athiest. To convince me that God exists, you're going to need a whole new miracle, caught on several cameras. If your argument is predicated on the Bible being God's "
                , Footnote 4 "As an athiest, do I have to capitalize God?"
                , Body "literal truth "
                , Footnote 5 "Which the Bible itself doesn't claim. None of the Bible is Jesus's words, it's all random goons who happened to be nearby and wanted to talk to journalists."
                , Body ", I'm not going to be convined. I signal this, strongly. A man wearing all black, scowling on a street corner, trying to cross the road, is probably not the best target for your Bible study. I was crossing 15th "
                , Footnote 6 "The road itself doesn't matter, but saying a specific road probably convinces you that this really happened."
                , Body ", and a man waiting at the corner with me starts making sketchy eye contact with me. I say sketchy, because he was acting as if he was very attracted to me "
                , Footnote 7 "A common Bible-study-inviter tactic. We'll revisit this."
                , Body ": he would glance over at me, and when I glared back "
                , Footnote 8 "I was having a bad day. I don't normally glare."
                , Body ", he would avert his eyes. We played this game of stare-glare-avert several times, until he eventually started talking to me. He said hello, and so I said hello. He asked me if I wanted to go to Bible Study "
                , Footnote 9 "Okay, I know that the S in Study isn't capitalized, but Jesus-types want me to capitalize so much else. I get carried away with capitalization around God."
                , Body """. I laugh, and say "Sorry! I'm very athiest, doesn't my outfit signal that?", as I gesture towards myself. I am wearing Chelsea boots, a famously atheistic shoe """
                , Footnote 10 "At least, I wear Chelsea boots to signal my athiesm."
                , Body """. He laughs back, and tries to convince me that anyone can wear any clothing. While he isn't strictly wrong, he is missing the point of clothing. Clothing is the most potent social signaling mechanism that we have, and wearing a long(ish) black coat over black jeans with black chelsea boots with a band shirt is a pretty strong social signal. It signals that I don't want to go to Bible study. """
                ]
            , TextWithFootnotes
                [ Body "He won't accept no for an answer "
                , Footnote 11 "Which says bad things about him as a person."
                , Body ", and he presses me. Every Bible-study-inviter has a set of talking points. And, incredibly, they're all different "
                , Footnote 12 "I wonder if they have a handbook. They probably don't A/B test opening lines, because I'm pretty sure they think statistics was invented 6000 years ago."
                , Body ". This guy asks me how long I've been studying math. I think for a second, forgetting for a moment how old I am "
                , Footnote 13 "Everything since I turned 21 has been somewhat of a blur."
                , Body ", then I tell him fifteen years, which I figure is close enough to true "
                , Footnote 14 "I'm sure it doesn't matter for his pickup line. And, in fact, it doesn't."
                , Body ". He asks me if I think I understand all of math, and I reply with a long laugh and a \"Nope!\". He doesn't find it as funny as I do. He asks me how long I've spent studying the Bible. I see where this is going. I say \"long enough\", which he doesn't hear at all. He asks whether I can understand, truly, what the Bible is saying, if I've studied it for less time that I've studied math. I tell him that I think I understand what Animorphs is saying, and I only studied each book in the series for a few hours tops. At least they had good cover art! He is not happy with this. We part ways. I turn left without telling him, and he goes straight. The same social pressures that lead me into conversation with him prevents him from turning around and following me. I make it home. Being asked to Bible study once is not particularly interesting, so I take no note."
                ]
            , TextWithFootnotes
                [ Body "The next day, I'm on CU Denver's campus"
                , Footnote 15 "Sometimes I go to campus, and sit on a bench, and write poetry. Not that it's any of your business what I do on CU Denver's campus."
                , Body ". I hang out for a bit, revel in what I know to be one of the last days of summer "
                , Footnote 16 "This is false. Denver's weather is slightly schizophrenic, so that it can be overcast and snowy on the same day that it's 70 and sunny."
                , Body ", bask in the sun, write a bit. I pack my stuff, stand up and leave, and as I'm walking out, I pass a gaggle of girls. One of them whispers something to another, which I hear as \"go for it!\", so I am marginally excited, as I think someone is about to ask for my number"
                , Footnote 17 "Despite the fact that something like this has never happened before. I suffer from the male predisposition to fantasize about things like this."
                , Body ". But that is not the case. This cute black girl "
                , Footnote 18 "My understanding of churches is that they are typically strongly segregated by race. The man in the previous paragraph was Asian. Are these two different churches? Are they both members of a single group of churches, united by their shared love of Bible study?"
                , Body " walks up to me, and my heart flutters a bit "
                , Footnote 19 "Although, at 5280 feet above sea level, even just walking short distances causes my heart to flutter. So we can't rule out very small amounts of exercise as the cause of this heart flutter."
                , Body ". She asks me whether I want to go to Bible study this Thursday at 3. I laugh, and say I'm athiest. She has no banter. She just looks severely disappointed in me. I walk home, confused about the emerging pattern of people asking me to Bible study."
                ]
            , TextWithFootnotes
                [ Body "I am sitting on a bench on CU Denver's campus. I have had a weird morning "
                , Footnote 20 "I met a man named Imhotep. I am firmly convinced he is a drug dealer. He gave me his number. We have plans to get drinks. He said the phrase \"movin' and groovin'\" at least three times."
                , Body ", and so I am ready for anything. Or so I think. A man "
                , Footnote 21 "White, to emphasive the multicultural nature of the people who ask me to Bible study."
                , Body " asks if he can sit next to me. I say yes, assuming he is going to be an interesting bench compatriot. He is not. He asks what I'm doing on campus. I make something up. He asks me if I want to go to Bible study. By now, I've started formulating responses to this question. I quip \"I'm sorry, but I have a girlfriend. Thanks for the offer, though\". Despite hours "
                , Footnote 22 "Maybe minutes. But I certainly had one full shower worth of thoughts about responses to Bible study invites."
                , Body " of time spent preparing this response, aimed at unsettling the asker enough to scare them away, designed to cut deeply at the (presumed) fear of homosexuality in the person asking me to Bible study, it has no effect. He laughs, and says it's not like that. He wants me to find Jesus inside me "
                , Footnote 23 "A message with deeply homosexual undertones, I might add."
                , Body ". I tell him to please leave my bench. He asks whether I've ever experienced something I would consider unexplainable by science "
                , Footnote 24 "No."
                , Body ". I tell him to please leave my bench. He asks whether I've ever been in a situation of mortal danger, when I thought I was going to die, but I lived "
                , Footnote 25 "I've been to New York and tried to jaywalk, so the answer is definitively yes."
                , Body "."
                ]
            , TextWithFootnotes
                [ Body "Too many people have asked me to Bible study for it to be a coincidence "
                , Footnote 26 "They would tell me that this means I should believe in the Christian God. A meta-argument for Bible study."
                , Body ". I don't know what it is about me that attracts them. But I'd like to say, to all of them: please, stop asking me to Bible study. And, to God: you're going to have to send some more attractive girls."
                ]
            ]
        , Model "Professional StarCraft 2"
            "professional-starcraft"
            Listed
            [ TextWithFootnotes
                [ Body "I am, technically, a former professional StarCraft 2 player. When I was in middle school, I played StarCraft 2 extensively. I was very good for my age "
                , Footnote 1 "I think."
                , Body ", and played in some age-limited tournaments, which I occasionally won. In fact, I won often enough that I recouped my tournament buy-ins, enough to cover the cost of the game itself, and I thought this was a pretty good deal "
                , Footnote 2 "My slightly-older self realizes that spending hundreds of hours playing a game in order to make ~$100 is not a good deal. But as a child, I had far fewer opportunities to make money than I do today. In fact, it was probably illegal for people to hire me. And playing video games is more enjoyable than mowing lawns."
                , Body ". The money was nice, I guess "
                , Footnote 3 "Can you tell I grew up in a firmly upper-ish-part-of-the-middle-class household? "
                , Body ", but playing StarCraft competitively changed my life. Other than being one of the reasons I met one of my best friends through all of middle and high school, it taught me the generic algorithm for getting better at things "
                , Footnote 4 "Although it took me several years to realize that this algorithm could be applied to things other than StarCraft."
                ]
            , TextWithFootnotes
                [ Body "StarCraft 2 has a ladder system that provides immediate feedback on your performance. You can queue up, get matched against a stranger, and play a game. At the end of the game, the winner gains some points, and the loser loses some. Internally, they use Elo rankings, but they publish a different number. Regardless of the exact scoring mechanism, you can see this score trend up and down, and if you play enough games "
                , Footnote 5 "And I was certainly playing enough games."
                , Body ", it becomes quite accurate. I floundered about in the lowest rung on the ladder, playing games and being so incompetent that I didn't even understand why I was losing games when I lost. I thought perhaps I could learn from the real professional games, and so I tuned in to a tournament. The camera flew around, showing different parts of the game map every second, mysterious things were happening, and the close-ups of the players' hands dancing over the keyboard were downright intimidating. Eventually, I found the livestream of a StarCraft personality, Day9. He would watch a replay of a game, commentating on why one player did better or worse than another, and importantly, his content was targeted at bad players like me."
                ]
            , TextWithFootnotes
                [ Body "He introduced me to the general algorithm for getting better at things."
                , NumberedList
                    [ "Choose the thing you want to get better at."
                    , "Choose one piece of that thing that you are doing poorly."
                    , "Practice, focusing only on that one piece, even at a cost to your overall performance."
                    , "Once you are doing better at the one piece, go to 2."
                    ]
                , Body "Armed with this way to create practice regimes, I did. I knew what I was failing at "
                , Footnote 6 "And this part is hard. Often, the hardest part of this algorithm is step 2. If you are unconscious of your incompetence, you don't know what you ought to be improving."
                , Body ", and I could get better at those things! My ranking suffered in the short term, because I was focusing on some small parts of the game while neglecting almost everything else. But after picking enough of those small parts of the game to improve at, my ranking began to climb again. This felt really good. "
                ]
            , TextWithFootnotes
                [ Body "I kept playing StarCraft, occasionally competitively, mostly just with friends, for most of middle school. By the time high school rolled around, I was starting to make the transition into MineCraft and various indie single-player games, which aren't skill-focused. They're about enjoying yourself and making cool-looking contraptions or following a story. There's nothing inherently wrong with this type of game, but they will never provide someone with an environment where they really want to grow their skill at something. StarCraft 2, for me, was like learning how to play an instrument. The reason we want children to learn how to play instruments "
                , Footnote 7 "Except guitar. If you pressure your kid to learn guitar, you want them to get laid. But oboe? Not so much."
                , Body " is to give them this same experience. This is the growth mindset –\u{00A0}knowing that you can improve, and having to figure out how to devise training regimens for yourself. This is an invaluable skill. Much to my parent's disappointment, clarinet did not teach me any of this, but fortunately, I managed to get some of the same outcomes through video games."
                ]
            , Plain "See, Mom? I told you playing video games was good for me."
            ]
        ]

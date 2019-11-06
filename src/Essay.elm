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
    , content : List ContentItem
    }


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


viewEssayPreview : Model -> Html msg
viewEssayPreview model =
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
            [ Plain """Why did I swap to zsh? I believed some of the features in it would save me time in the future. Or at least, that's what I want to tell myself. In practice, I swapped to zsh and iterm2 because Danny told me to. I'm going to try and estimate how much time it will actually save me, and how much time investment it will require."""
            , Plain """Figuring out the time investment seems easy at first glance, all that I'll have to do is note that it took me two hours to install and configure the new set-up day of. But in the weeks following that, I've noticed little things bugging me about the current configuration, which prompts a thirty-minute or so dive into the docs to try and fix it. This post-install tuning will probably be the majority of the time that I spend on managing this terminal setup. My old terminal setup, which had major problems that are now resolved completely, did not provide room for tweaking, and so did not distract me with promises of a better future. For example, playing around with my prompt to include the git branch probably saves me time in the long run - now I won't accidentally commit to master and have to cherry-pick, etc. But playing around with the color, to make it a fun green instead of a plain black, will not noticeably affect my well-being in the future. Admittedly, I could have been playing around with prompts and so on with my old setup, but taking one step to improve the situation implicitly justified continuing to change it over time."""
            , Plain """There's a general lesson to be learned here. While I still should make the large changes that will give me most of the benefits (pareto's 20%), the smaller changes won't pay off as well. I think in general, in computer science/software engineering, large changes like using one shell over another, or one programming language over another, take the same amount of time as smaller changes, such as changing a minor misbehavior of your terminal or following a specific style guide in your language. Because the time investment is the same, the investment-to-payoff ratio is larger for larger changes. And so, in the future, I'll try to focus on picking the right tool for the job, and less on the color of that tool."""
            ]

        --    Written 2019-07-28
        , Model "Invariants for Everyday Life"
            "invariants"
            [ Plain """I learned about class invariants in my "Object Oriented Design" class. A class invariant is a property of a class that is always true. For example, a class that instantiates a static field at compile-time has the invariant that that field is never null. Classes with final fields have the invariant that the value in the field won't change (barring reflection-based shenanigans). You get the gist – properties of a class that are enforced by the code. There are also loop invariants, a notion from algorithms, which are invariants about the state of a program during a loop. For example, if you are building up a list one element at a time, an invariant might be that the list you are building has length one less than the loop index."""
            , Plain """Invariants make code easier to reason about. In object-oriented, mutation-filled code, it can be hard to keep track of which methods change what fields, what values a given method may return, and so on. There are many moving parts, and invariants let programmers interacting with these systems rule out some of the worlds. Knowing of an invariant reduces the number of possible states of the world, which simplifies the process of ensuring your code is correct. In the same way that enumeration values make explicit the limited scope of a piece of data, while strings allow infinite possibilities, invariant properties shrink the state space."""
            , Plain """In everyday life, you can also ensure that some properties are true. For example, properties about the locations of your keys. Some people lose their keys often, because there are nearly infinite places keys can be - in your fridge, in a potted plant, in a desk drawer, and so on forever. However, by enforcing the simple invariant "my keys are either in my pocket, or in the key bowl", you can cut down the space to only two possible states. Admittedly, this is a harder constraint to enforce than an invariant in code, but the benefits are worth it. And following this is only a matter of habit: when you take your keys out of your pocket, put them in the key bowl, and vice versa. Another invariant that was helpful to me is ensuring that my wallet is always full while it is in your pocket: when you take your credit card out to pay at a restaurant, I take my wallet out of my pocket, and don't put it back in until I have put the cards back in. This has helped me overcome my habit of attempting to leave restaurants after I have started to pay but before I have signed. Similarly, I kept bringing my computer glasses case, while leaving the glasses themselves on my desk. So, I started enforcing this invariant: I only close the glasses case when it has glasses in it. Then, if it is open, I can clearly see that there are no glasses. Now, I never bring an empty glasses case with me."""
            ]

        --  Written 2019-08-04
        -- TODO make that XKCD link a real link
        , Model "The GRE"
            "gre"
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
            [ Plain """I believe that watching Netflix, scrolling through Instagram, and swiping on Tinder is overall bad for other people, on average. So why do I do those things sometimes? In general, when I use these apps, I have reasons why my usage is atypical and so not as likely to be harmful as other people's usage of the apps. This is probably irrational. Without strong evidence pointing towards me begin special, it's much more reasonable to assume that the effect of technologies on my welfare is approximately average."""
            , Plain """"Taking the outside view" means looking at the statistics for people in your situation, and applying those statistics to your own position. Instead of asking whether you are being benefited by a particular app, ask whether people on average are benefited. This engages your critical thinking skills, instead of asking in essence whether you have a first-order desire to keep using some app. It may be addictive, and fun, and even enjoyable, but to ask yourself whether it is beneficial to others will reveal whether it is beneficial to you."""
            , Plain """So, if I think that deleting Instagram or Facebook would be good for everyone else, I ought to delete Instagram or Facebook myself. While I was always an atypical user of both, I think, the same reasons that apply to other people apply to me. My typical argument against Instagram is that the constant comparison of aethetics with other people is bad for your mental health and promotes shallow cravings, as well as being a distraction from what really matters in life. Even when I moved my own Instagram account to a "write-only" mode where I would download the app when I had an image I wanted to post, post the image and delete the app again, I still was influenced by the pressures of knowing other people were seeing the images I posted. When taking photos, wandering around the streets of Boston with my DSLR, I was capturing the photos that I thought others would like. And it wasn't the kind of other-related preference like when you take a photo specifically because you think one person you know will like it, which is almost a form of gift giving. "I took this photo, because I thought you'd like it" is quite different from "I took this photo, because I think it implies positive things about my lifestyle to people who glance at it". Because Instagram makes you care about the judgements of everyone, in aggregate, the niche aesthetic moments that appeal only to one friend aren't the highest value."""
            , Plain """It's worth noting that I did, in fact, delete my Facebook, Instagram, and Snapchat. (I'm still working on Netflix.) This is mostly because the Black Mirror episode "Smithereens" really got to me, not because of the rational deliberations presented here. I think that I benefited from these deletions, because I think that other people would have benefitted from them."""
            ]
        , Model "Good Self-Harm"
            "goodharm"
            [ Plain """While "Good Self-Harm" seems like an oxymoron, it's not. Look at vigorous exercise, cold showers, or fasting. These three activities all cause pain, and we choose to voluntarily inflict them upon ourselves, so we have the "self" and the "harm". Normally, when people talk about self-harm they mean something like cutting, which is not good for you long term. But these other three, despite being painful, are good for you, and in fact one of the three is even actively encouraged by society."""
            , Plain """I think that intentionally doing things that you know you won't enjoy in the moment is good for you in the time beyond that moment. I am more productive and maybe even happier on days I take cold showers, and other people seem to report similar effects. Getting into the shower, I still feel my muscles clench in anti-anticipation, knowing that the cold will hurt. But I step in anyways, and after a minute or so, I don't feel the cold anymore. More importantly, it gets me in the habit of forcing myself to do things that I don't want to do in the moment. The same brain muscles, I think, get used dragging yourself to the gym, into a cold shower after the gym, and off Netflix to do something you actually want to do."""
            , Plain """So why is only exercise encouraged by society, of the three above? Fasting is seen as weird, especially the three-day-long fasts where I work tirelessly, lift my old PRs effortlessly, and develop a deeper understanding of when I'm actually hungry and when I'm just bored. People don't like fasting, and I can totally understand why –\u{00A0}hunger pangs are painful, and missing out on some social opportunities because I can't eat sucks. But going to the gym also sucks, causes pain, and takes up time that could be used elsewhere. There's no important difference I can see here. There are health benefits to lifting, but there are health from fasting as well (autophagy). There are aesthetic/fitness benefits to going to the gym, but similarly to fasting. Fasting execissively can be dangerous, and a sign of a mental disorder, but so can excessive gym-going. And cold showers take very little time –\u{00A0}almost by design, as one of your first goals after you step into a cold shower is to get the hell out. Not only will you save maybe ten minutes by making your showers faster, but cold showers make you more productive and focused during the day. So, if you're going to the gym, I think you should consider the other two interventions, based on the same reasoning."""
            , Plain """Harming yourself with the aim of harming yourself is bad, as your end goal is to damage yourself, which is not good for your long-term prospects. "Harming yourself", like you do at the gym or in a cold shower, has the end goal is improving yourself, so it can be justified more. """
            , Quote (QuoteItem "William James" (Just "Principles of Psychology") """Keep the faculty of effort alive in you by a little gratuitous exercise every day. That is, be systematically ascetic or heroic in little unnecessary points, do every day or two something for no other reason than that you would rather not do it, so that when the hour of dire need draws nigh, it may find you not unnerved.""")
            ]
        , Model "Two Notebooks"
            "notebooks"
            [ Plain """It's weird enough in 2019 to have one notebook, so actively using two earns me some sideways glances. First, the backstory of my attempts to be productive and keep a list of the things I need to do. """
            , Plain """I started bullet-journaling on January 1, 2019, but not because of a New Year's resolution. I had heard the news that Google Inbox was getting shut down in March, and wanted to have a smooth transition from my Google-Inbox-based life organization scheme to a different one. I had heard about bullet journaling from Cal Newport, and I thought I'd give it a try."""
            , Plain """First, let me explain my old bookkeeping system. People would send me emails sometimes, and when they did, I would either archive it immediately after reading (if it didn't require action), or leave it read in my inbox (if and only if the email represented a task). If someone gave me a task in a non-email format, such as professors assigning homework in class, I would put that task as a "reminder" in Google Inbox, and it would show up inline, interwoven with the emails. So, the end result was a list of all my tasks, in reverse chronological order by assignation date. The snooze feature allowed me to keep my inbox free of tasks due far in the future, while not forgetting about them entirely. All was well and good."""
            , Plain """Without Google Inbox, though, you can't see your emails and reminders in one place. (Currently, there is only one place in google that you can snooze your reminders - even though there are about six places where you can add and remove and complete them.) So, I needed a new system. I experimented with other within-Google solutions, like Keep and Reminders and even the calendar. But nothing satisfied my aim of having all my tasks in one place."""
            , Plain """This lead me to bullet journaling. I could write down the immediate tasks for a day each day, and I used Google reminders still for longer-term aims. It seemed reasonable. Eventually, I realized that centralizing the task list was good, and that writing down your goals in a book helps you form the habit of checking that book and doing tasks out of it. So, I stopped, made a calendar assigning one page per week until the end of the year, and resumed the daily sections afterwards. Now, I can plan more than one day in advance, and I'm able to schedule tasks for far in the future. For example, whenever I swap out toothbrushes, I write a note to swap toothbrushes, assigned for three months in the future."""
            , Plain """Bullet Journaling™ wants you to use one notebook for everything – you write down your daily task list, then you doodle a bit, take some notes on math you're thinking about, jot down an idea for an essay, and then write down tomorrow's daily task list five pages of nonsense later. I couldn't quite deal with this, as I started getting into research, as having to flip through dozens of pages of arithmetic to find what I needed to do today and what I failed to do yesterday was difficult. Hence, the second notebook. This notebook allows me to have my math from yesterday next to my math from today, and frees up the other notebook to have one day's tasks next to the other. Even though there are more physical items involved, increasing the physical clutter on my counter, the decrease in intellectual and organizational clutter makes it worth it. """
            ]
        , Model "Achieving Second-Order Desires"
            "desire"
            [ Plain """Actualizing upon your second-order desires is easier than most people believe, and has a large positive impact on your life. To clarify, secord-order desires are the desires you have that are about the desires you have, for example, a desire to desire candy less and vegetables more, or a desire to desire learning more. These desires are about something entirely internal, your desires, and so should be easier to achieve than desires that require changing the external world. Unfortunately, people don't have that level of control over their brains."""
            , Plain """Or, perhaps, we do have a lot of control over the future state of our minds, but we don't know how to exert that control. This seems relatively implausible, because the default assumption in most matters of the mind is that each person knows their mind entirely. Looking at examples like walking, however, paints a different picture. Walking is widely known [citation needed] to increase subjective wellbeing and causes happier moods, but most people still walk very little. In fact, when I am in a bad mood, I know consciously that going for a walk will make me feel better, but it still requires mental fortitude to get my shoes on and get out the door. Other things which also would improve my mood, like seeing a friend or drinking good coffee, I don't resist in the slightest. I enjoy going for walks, and they are long-term good for me, and yet I still fail to take as many walks as would be subjective-wellbeing-maximizing. I have a second-order desire to desire to go for walks, but a first-order desire to avoid walks. Actualizing the second-order desire, changing my opinion of walking to be immediately and viscerally positive, would be good for me. But conventional wisdom says that you can't just up and decide to change your mind."""
            , Plain """I think that the conventional wisdom is mistaken. Positive and negative reinforcement work on humans, just as well as they do on other animals. Go for a walk, then eat a square of chocolate. Surely, your disposition towards walking will be better than if you bit a lemon immediately afterwards. If you keep up the habit of having chocolate after you go for a walk, your brain will start associating the walk with the pleasant feeling, and keep valuing the walk even once you discontinue the chocolate routine. Another possible way to change your desires is through cognitive behavioral therapy. If cognitive behavioral therapy can help some people with severe eating disorders, depression, or OCD change their thought patterns, it is clear that it has the power to change negative thought and action patterns to neutral ones. If it can change negative to neutral, why not neutral to positive? For the first three weeks I went to the gym, I told myself that I enjoyed the experience, even as I woke up at 6AM, sore, exhausted, and not seeing agny improvement. And after those three weeks, I started enjoying it. Now, correlation is not causation, and it's possible that I would have enjoyed getting back into working out regardless, but it can't hurt."""
            , Plain """The value of being able to change your habits at will (or with a little bit of work) is very high. So I believe it's definitely worth the time taken to at least explore whether you have power over your habits. Low cost, high reward, and trying really hard to change your habits for a few weeks isn't going to hurt anything. """
            ]
        , Model "Endnotes and Footnotes"
            "notes"
            [ Plain """In non-fiction, don't use endnotes, because flipping to the end of a book to get slightly more context is annoying, and footnotes are just convenient. In fiction, though, endnotes and footnotes can be used stylistically, because they feel very different to the reader. A footnote is playful, like a parenthetical, requiring just a glance down at the bottom of a page to see what the author is talking about. An endnote is daunting, a quest laid out in front of you, requiring flipping to the very end of the book to investigate something."""
            , Plain """The archetypal use of footnotes is found in Terry Pratchett's writing. Pratchett uses footnotes for witty side commentary, and there is a real reason that the commentary is in a footnote instead of in the text itself – comedic timing. For instance, in "Small Gods", Pratchett writes "Kitchens and storeroom and craftsmen's workshops belonging to the Church's civilian population honey-combed the citadel. (1)", footnoted to "It takes forty men with their feet on the ground to keep one man with his head in the air". Incredible! The footnote sets you up with context that has been set by the "footnote culture" of non-fiction, where footnotes provide additional detail that some readers might wish to skip. Going in to that footnote, I thought I was going to get a description of the kitchens and such, but instead I was hit with an aphorism. """
            , Plain """The quintessential deployment of endnotes is in Infinite Jest. Eleven hundred pages, and one of those hundred is entirely endnote. These endnotes aren't just clarifications and citations and the drudgery that I have come to expect from non-fiction's endnotes, though, they give the story character. It's a book you must read with two bookmarks, one for the main text and one for the endnotes, because there is an endnote on average once per three pages, and flipping over a thousand pages is no fun. I will try to characterize the content of these endnotes, although they try their hardest to elude description. Some of them are definitions, as early in the book when drugs are being introduced, a chemical specification and a brief overview of the effects of the drug. But as the book wears on, they gradually fade into stories, completely separate side plots from the main book, ten-page excursions into a piece of backstory that might help you understand a moment that just happened in the main text. But these bits of story tucked away in the back of the book are precisely those stories that fit there, hidden away in the endnotes – stories of embarassments of characters, of intrigue and spies and so on. The endnotes, too, begin to feel like a mockery, David Foster Wallace pointing out without pointing out that you are willing to flip pages just for him, just to consume some media."""
            , Plain """Footnotes and endnotes are both ways to extend a book for some readers but allow readers to opt-out without the guilt that comes from skipping main text. But, particularly in fiction, they add different textures to the story you are telling. """
            ]
        , Model "Quantum Outfits"
            "quantum-outfits"
            [ Plain """There is an interesting connection between wave function collapse and picking an outfit to wear. The wave function collapse algorithm, which is a classical algorithm based on quantum mechanics, works roughly as follows: starting with a probability distribution over some set of choices, pick one thing at random and update the probability of choosing the rest based on their conditional probability, conditional on the choice you made. For example, this algorithm is often used to generate tilemaps for video games: pick one tile, then choose the rest based on the probability that they are adjacent. This allows patterns like roads to be generated, if the probability of a "straight road" tile coming after another "straight road" tile is sufficiently high, you end up with long uninterrupted roads. """
            , Plain """ You should be able to see how this maps to choosing an outfit. Typically, the way I choose outfits follows a very similar algorithm. First, I pick one piece of clothing that I will wear. Let's be honest: I always pick black jeans. Then, I pick other clothing items, conditional on this first one: perhaps black boots, which will go well with the black jeans. Then, the wave function collapses further: I decide to wear a white band shirt. Finally, I need a coat, and I pick that coat conditional on the clothing that I know I will be wearing underneath it."""
            , Plain """There is no instrumental value to knowing about the connection between wave function collapse and outfit choice, as far as I can see. But I still think it's neat. Perhaps it can help you understand why you never wear some parts of your wardrobe. If you don't pick a piece of clothing first when dressing, you run the risk of having the wave function collapse somewhere else. In my case, shirts that go with blue jeans, but not black, never end up being worn, because my wave function starts its collapse around black jeans. """
            ]
        , Model "A Nebraska Double: Shot 1"
            "nebraska"
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
                , Body """. I am shocked at how cheap alcohol is here. Some of the beers on this menu (and they're nice, craft beers, with fun backstories) are five bucks. At nine, my old roommate informs us that he's leaving because he has some work to do. We're undergrads, and so we follow him: we're used to the idea of leaving when the people your age start to leave. We are real adults, and we could have hung out with the philosophers, but our instincts lead us outside."""
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
                , Body """ pulls up. It's the weekend of Halloween, so it's not completely absurd that he's wearing a cow onesie with articulated udders. It's still slightly absurd, but not completely. We chat for hours. Or I should say, Orientation and Roommate chat with him for hours. I meet some strangers, including an asian man who pretends he doesn't speak English, until he realizes his friends and I are going to be yelling about philosophy for a while, and then caves and joins the conversation. The first English he spoke to me was "Bullshit! You know that's impossible". I was taken aback. I thought, just for a second, that he had learned English in that moment, just to tell me I was wrong. I was impressed at his dedication, and then I realized he had been messing with me. After an hour of conversation, on a street corner, with a pedicab driver and three random folks (they claimed that one of them had just gotten married, but they hadn't given me much reason to trust them, and they were carrying take-out food, so I choose not to believe it), we head home. """
                ]
            , TextWithFootnotes
                [ Body "We make it home, argue about the trolley problem"
                , Footnote 10 "To Roommate, if you're reading this: you are deeply, hopelessly wrong about the trolley problem."
                , Body ". And that concludes the first day. But I still had twenty hours left in Lincoln."
                ]
            ]
        , Model "A Nebraska Double: Shot 2"
            "nebraska2"
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
        ]

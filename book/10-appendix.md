# Appendix: Datasets and packages {#datasets-appendix}

All datasets used in this book are publicly available, either as:

* Datasets in the `languageR` R package [@languageR] associated with R.H.\ Baayen's book _Analyzing Linguistic Data_: `english`, `regularity` datasets

* Open Science Framework ([OSF](https://osf.io/)) projects: all other datasets

## `english` lexical decision and naming latencies {#engdata}

Description from [`languageR` documentation](https://www.rdocumentation.org/packages/languageR/versions/1.4.1/topics/english):

> "This data set gives mean visual lexical decision latencies and word naming latencies to 2284 monomorphemic English nouns and verbs, averaged for old and young subjects, with various predictor variables."

The original source of this data is the [English Lexicon Project](http://elexicon.wustl.edu/).

To load this data and learn what different columns mean, execute in R:
```
library(languageR)
?english
```

In this book we often use `RTlexdec`, `Word`, `AgeSubject`, `WrittenFrequency`, `LengthInLetters`.

## Dutch `regularity` {#dregdata}


This dataset, originally from the study reported by @baayen05semantic, is described in the
 [`languageR` documentation](https://www.rdocumentation.org/packages/languageR/versions/0.2/topics/regularity):
 
> Regular and irregular Dutch verbs and selected lexical and distributional properties.

To load this data and learn what different columns mean, execute in R:
```
library(languageR)
?regularity
```

In this book we often use `WrittenFrequency, `Verb`, `Auxiliary`, `Regularity`.

## European French phrase-medial vowel `devoicing` {#devdata}

This dataset is from @torreira2010phrase, a study examining phrase-medial vowel devoicing in European French. The data is posted as `french_medial_vowel_devoicing.txt` in the OSF project [@frenchDevoicing_data].


### Background

This is an example from the paper:

<center>
![](../images/phrase_medial_devoicing_euroFr.png)
</center> 

The `french_medial_vowel_devoicing` data (or just `devoicing`) consists of:

* 550 French syllables with voiceless obstruent onset (/pktsf/) followed by a high vowel (/iuy/),
    
* located in non-final position within the intonational phrase (IP)
    
* extracted from a corpus of spontaneous conversational speech [@torreira2010nijmegen].
    
One way to measure to what extent "devoicing" has occured is syllable duration.

The research question considered for this dataset (in this book) is: 
**are function words (e.g. "qui", "tu", "si") shorter than other syllables**?

Variables:

* Response variable: **syllable duration** (`syldur`)

* Predictors:

    * **word type** (function/content) (`func`)
    
    * **speech rate** (`speechrate`)
    
    * **onset type** (`c1`)
    
    * **vowel type** (`v`)

## North American English `tapping` {#tapdata}

This data is from a speech production experiment by @kilbourn2017speech, @kilbourn2017effect examining tapping in North American English. The dataset, which contains only a subset of the data analyzed in the publications, is posted as `tappedMcGillLing620.csv` in an [OSF project](https://osf.io/8rjxu/) [@tapping_data].


### Background

In North American English, the sounds [t] and [d] can sometimes be optionally pronounced as [ɾ] (a "tap") if followed by a vowel:

* "For those of you who'd like to ea**t** early, lunch will be served."
  
* "For those of you who'd like to ea**ɾ** early, lunch will be served."
  
According to earlier work, tapping interacts with syntax (e.g @scott1984segmental):
  
* Sounds OK: "For those of you who'd like to ea**t**, early lunch will be served."
  
* Doesn't: "For those of you who'd like to ea**ɾ**, early lunch will be served."

It seems that a syntactic juncture following [t/d] makes tapping less likely. But is this true? The effect could result either from

1. A syntactic juncture

2. A stronger prosodic boundary, which correlates with a syntactic juncture


@kilbourn2017speech, @kilbourn2017effect report a production experiment to investigate.

Participants produced sentences like:

* "If you plit, Alice will be mad"
    
* "If you plit Alice, John will be mad"
   
for nonce words like "plit" ending in [t] or [d], followed by a vowel initial word (such as "Alice").

The two manipulations are:

1. `syntax`: the nonce word can be
    + *intransitive* (next word = following clause)
    + *transitive* (next word = complement)
        
2. `speakingRate`:
    + Normal vs. fast

The research questions are:

1. How often did participants tap, depending on `speakingRate` and `syntax`
    + This is a categorical response variable, `tapped` (0/1).

2. Does tapping rate depend on the prosodic juncture between words?
    + The juncture strength is estimated using the duration of the preceding vowel: `vowelDuration`.


## `halfrhyme`: English half-rhymes {#halfdata}

This data is from a speech perception experiment by @harder2013, examining what determines how "good" English speakers think imperfect rhymes are (e.g. *time*/*tide*). The dataset is posted as `halfrhymeMcGillLing620.csv` in an [OSF project](https://osf.io/k5pnu/) [@halfrhyme_data].

<!-- TODO future: dataset background -->

## `givenness` data: the Williams Effect {#givedata}

This data is from a speech production experiment, reported in @wagner12illusion, examining how information structure affects which words in a sentence are pronounced with more emphasis ("prominence"). The dataset is posted as `givennessMcGillLing620.csv` in an [OSF project](https://osf.io/r4j2w/) [@givenness_data].


### Background

When we speak, some words have more emphasis than others: this is an aspect of prosody that linguists call  *prominence*. (For example, if you read the preceding sentence out loud, "prominence" is probably emphasized.)  The opposite of prominence is *reduction*, where a word is produced without any emphasis.

Constituents (e.g. words) that are "given" or salient in discourse (usually because
they have just been used before) and constituents whose referents are
highly salient (for example the referent of a pronoun may be given,
even if the pronoun itself has not been used before) are often
prosodically reduced. For example, the referring expression "John"
in the second sentence below is unlikely to carry an accent, and
instead prominence (shown with bold) is likely to be shifted to "greeted" (shifted
compared to where prominence would have fallen had the first sentence
not been there):

* "John finally arrived at the function. Mary **greeted** John."


A systematic exception is cases in which a constituent is
contrastive in addition to being given. Consider the following case,
where the second sentence marks a double contrast: the agent "Mary" is contrasted with the agent of the previous sentence, "John"; in addition, the patient "John" is contrasted with the patient of the previous clause, "Mary". The fact that both  "John" and "Mary" are given does not seem to be relevant if they are also contrastive:

* "**John**  kissed **Mary**. Then **Mary** kissed **John**."

We can conclude that contrast (marked by accenting) trumps givenness
(marked by lack of accentuation). This dataset was used by  @wagner12illusion to investigate a  systematic class of apparent counterexamples, which were first
observed by  @williams1980remarks. According to Williams, in these cases accenting a contrastive referring expressions sounds odd, contrary to what we would expect from a semantic point of view:

* \# "**John** kissed **Mary**. Then **John** was kissed by **Mary**." 

That is, even though "Mary"" is contrastive in the second
sentence, and thus should be accented, doing so sounds odd (indicated by "#").  Wagner's experiment, and the analysis used in this book, tests whether this *Williams effect* is real, and how it
affects the way speakers produce sentences like the one above (with "#").

The dataset `givenness` contains data from the experiment, where participants produced sentences of four types:

1. John greeted Mary, and then John was greeted by Mary.
2. Mary was greeted by John, and then John was greeted by Mary.
3. John greeted her, and then John was greeted by her. 
4. She was greeted by John, and then John was greeted by her. 

These four sentences constitute one `item` in the dataset.  There are many other items, each with a similar structure: two clauses, with a meaning equivalent to "noun$_1$ verbed noun$_2$, and then noun$_2$ verbed noun$_1$", for different choices of the verb, noun$_1$, and noun$_2$.  

The sentences convey the same information in four ways.  In each one, the second clause is passive, and the NP referring to Mary appears at the end of the clause.  The four sentences differ in whether the NP referring to Mary is a full NP (her name) or a pronoun, and whether the NP referring to Mary appears at the end of the first clause, or not:

* Full NP, at the end of clause 1
* Full NP, not at the end of clause 1
* Pronoun, at the end of clause 1
* Pronoun, not at the end of clause 1

This experiment thus has a *2x2 design*, with two within-item variables. Within an item, the sentences corresponding to #1-#4 were seen by four different `participant`'s, and each participant saw exactly 1 sentence from each item. (Thus, this experiment has a *latin-square design*.)

<!-- We will ignore variability between participants in the current analysis.  We will also ignore that observations from the same participant or the same item do not have independent errors, so that we can analyze this data using the tools we have so far. -->

The experiment examined what factors affect whether the final NP
(henceforth the *target NP*) in sentences like #1-#4  is accented (as in (a) below), or whether the accent is shifted to an earlier word (as in (b)-(d)).


a.  **John** greeted **Mary**, and then John was greeted by **Mary**.
b. **John** greeted **Mary**, and then **John** was greeted by Mary.
c. **John** greeted **Mary**, and then John was **greeted** by Mary.
d. **John** greeted **Mary**, and then John was greeted **by** Mary.


We consider four **predictors** which could affect whether stress is shifted:

*  `conditionLabel` (within-item variable 1): Whether the target NP (the last word of the second clause) previously appears
    * at the beginning of clause 1 (level *Contrast*)
    * at the end of clause 1 (level *Williams*), in which case both clauses end with the same word.

* `npType` (within-item variable 2): Whether the second NP is a full NP or a pronoun
    * corresponds to the two-level factor `npType` (levels: *full*, *pronoun*).
    
*  `voice`: Whether clause 2 is active or passive.  
    * (Note that the values of `voice` and `conditionLabel` together determine whether clause 1 is active or passive.)

* `order`: Stimulus presentation order (within each
  participant).^[Although  `order` can only take on a discrete set of
    values, in applications in this book we treat it as a continuous variable (not a factor).]


Why these predictors? In order:

* The main motivation of the experiment was the observation that
  sentences like the one marked with "#" above sound odd, at least when the final word is
  accented. 
    * Cases like `conditionLabel`=*Williams*, where two consecutive sentences end with identical accented phonological chunks, are dispreferred. 
    * This is the called the *Williams effect* by Wagner.

* Speakers might be more or less willing to stress pronouns versus full NPs in general, or the Williams effect might differ in strength between the two types of NPs. 

* Whether clause 2 is active or passive affects what word stress would be shifted to, which might also interfere with the Williams effect.  

* Finally, how much participants shift stress might change over the course of the experiment as they get used to the stimuli.

Examples in this book consider two **response** variables indicating whether prominence has shifted.

1. The variable `shifted` is a binary (0/1) factor indicating whether a research assistant heard prominence as shifted or not (levels *shift*, *noshift*)

2. The variable `acoustics` is a linear combination of various acoustic cues that acts as a proxy for `shifted`.^[Importantly, `acoustics` was artificially constructed, and should not be used for any re-analysis or replication of the original experiment. In the original experiment, only `shifted` was annotated, and various acoustic prosodic measures (pitch, duration and intensity of different words) automatically extracted. We constructed the `acoustics` variable for teaching purposes, by running a logistic regression predicting `shifted` from the prosodic measures, and taking the resulting linear predictor from the regression to be `acoustics`.  This allows us to analyze the same dataset using both linear regression (response = `acoustics`) and logistic regression (response = `shifted`).]
    *  The acoustic cues are related to pitch, duration, and intensity of the target NP.
    * A higher `acoustics` value correlates with a lowering of the promiinence of the target NP relative to earlier words in clause 2, and hence whether stress has been shifted from the target NP (higher `acoustics`) or not (lower `acoustics`).



## `alternatives` {#altdata}

This data is from a speech production experiment, reported in @wagner16informationstructure,
examining  how information structure affects which words in a sentence are pronounced with more emphasis ("prominence"). The dataset, which contains only a subset of the data analyzed in the paper, is posted as `alternativesMcGillLing620.csv` in an [OSF project](https://osf.io/dha2j/) [@alternatives_data].

### Background

In the experiment, participants read sentences like "She brought a new bicycle", where an adjective modifies a noun. The manipulation was whether a contrast to new was mentioned in the context. There were three conditions, captured by a three-level variable `context`:

1. *New* (No previous mention of bicycle)
    + Ex: "Guess what John's aunt, who is incredibly generous, brought for his birthday: A new bicycle!"

2. *NoAlternative* (previous mention of bicycle, but no true alternatives to "new bicycle")
    + Ex: "Guess what John's aunt, who produces expensive bicycles, brought for his birthday: A new bicycle!"
    
3. *Alternative* (previous mention of bicycle, with a true alternative to "new bicycle":)

The question of interest is: when do speakers shift prominence from "bicycle" to "new"? In condition (1), presumably they don’t, because "bicycle" has not been mentioned before. But what about (2) vs. (3)?
    
The **response** is the binary (0/1) variable `shifted`, which catpures whether prominence was shifted to the adjective (as perceived by a research assistant).

<!-- ## Givenness {#givedata} -->

<!-- The dataset givenness contains data from an experiment where participants produced sentences of four types, shown below: -->

<!-- 1. "John greeted Mary, and then John was greeted by Mary." -->
<!-- 2. "Mary was greeted by John, and then John was greeted by Mary." -->
<!-- 3. "John greeted her, and then John was greeted by her. -->
<!-- 4. She was greeted by John, and then John was greeted by her. -->

<!-- These four sentences constitute one *item* in the dataset. There are many other items, each with a similar structure: two clauses, with a meaning equivalent to “noun1 verbed noun2, and then noun2 verbed noun1”, for different choices of the verb, noun1, and noun2. -->

<!-- The sentences convey the same information in four ways. In each one, the second clause is passive, and the NP referring to Mary appears at the end of the clause. The four sentences differ in whether the NP referring to Mary is a full NP (her name) or a pronoun, and whether the NP referring to Mary appears at the end of the first clause, or not. This experiment thus has a **2x2 design**, with two within-item variables. The experiment examined what factors affect whether the final NP (henceforth the *target NP*) in sentences like those above is accented (as in sentence 5), or whether the accent is shifted to an earlier word (as in sentences 6-8). Bold denotes an accented word. -->

<!-- 5. **John** greeted **Mary**, and then John was greeted by **Mary**. -->
<!-- 6. **John** greeted **Mary**, and then **John** was greeted by Mary. -->
<!-- 7. **John** greeted **Mary**, and then John was **greeted** by Mary. -->
<!-- 8. **John** greeted **Mary**, and then John was greeted **by** Mary -->

<!-- We consider four *predictors* which could affect whether stress is shifted: -->

<!-- * `conditionLabel` (within-item variable 1): Whether the target NP (the last word of the second clause) previously appears (a) at the beginning of clause 1 (level `Contrast`); (b) at the end of clause 1 (level `Williams`), in which case both clauses end with the same word. -->

<!-- * `npType` (within-item variable 2): Whether the second NP is a full NP or a pronoun. corresponding to the two-level factor npType (levels: `full`, `pronoun`). -->

<!-- * `voice`: Whether clause 2 is active or passive. (Note that the values of `voice` and `conditionLabel` together determine whether clause 1 is active or passive.) -->

<!-- * `order`: Stimulus presentation order (within each participant). -->

<!-- Why these predictors? -->

<!-- * The main motivation of the experiment was the observation that sentences like 3 sound odd, at least when the final word is accented. Cases like `conditionLabel=Williams`, where two consecutive sentences end with identical accented phonological chunks, are dispreferred. This is the called the *Williams effect* by Wagner (2012). -->

<!-- * Speakers might be more or less willing to stress pronouns versus full NPs in general, or the Williams effect might differ in strength between the two types of NPs. -->

<!-- * Whether clause 2 is active or passive affects what word stress would be shifted to, which might also interfere with the Williams effect. -->

<!-- * Finally, how much participants shift stress might change over the course of the experiment as they get used to the stimuli. -->

<!-- The *response* indicating whether stress has shifted is `acoustics`: this continuous variable is a linear combination of various acoustic cues (related to pitch, duration, and intensity of the target NP) and correlates with a lowering of the prominence of the target NP relative to earlier words in clause 2, and hence whether stress has been shifted from the target NP (higher `acoustics`) or not (lower `acoustics`). -->

## VOT {#votdata}

This dataset contains voice onset times (VOTs) measured for speech from from a corpus of speakers of different British English dialects---a subset of the VOT data analyzed by @sonderegger2017medium.  The data is posted as `votMcGillLing620.csv` in an [OSF project](https://osf.io/dmxuj/) [@vot_data].

### Background

The dataset contains VOT measurements (in msec) for 4728 word-initial voiceless stops, corresponding to 424 word types and 21 speakers. Only stops beginning with /t/ and /k/ are included (/p/-initial stops have been omitted, to simplify the analysis). Like most corpus data, this dataset is very unbalanced: there are between 1 and 1505 tokens per word type, and 35-619 tokens per speaker.

The dataframe contains columns corresponding to a number of variables, at the level of the speaker, the word, and individual observations:

* Speaker-level
    + `maleSpeaker: 1/0 for tokens from male/female speakers.
    + `speakingRateMean`: Mean speaking rate (syllables/second) for data points from this speaker.

* Word-level
    + `poaVelar`: 1/0 when place of articulation is velar/alveolar (i.e. /k/ vs. /t/)
    + `followingHigh`: 1/0 when the following vowel is high/non-high (e.g. tea vs. tap)
    + `stressedSyll`: 1/0 when the syllable containing the stop is stressed/unstressed (including function words).

* Observation-level
    + `speakingRateDev`: Speaking rate for this observation, minus `speakingRateMean` for this speaker.
  
The expected effects of some of these predictors on VOT can be intuitively understood as consequences of reduction. Faster speaking rate means (by definition) that speech is compressed, including the part of stop consonants corresponding to VOT. This compression might take place across speakers (those who talk faster, on average, have lower VOTs), within speakers (faster speech $\Rightarrow$ lower VOT, for a single speaker), or both. Unstressed syllables are often realized as more reduced phonetically, corresponding to shorter segments (and hence shorter VOT). The source of the place of articulation effect on VOT is more mysterious (there are several proposed explanations), but has been observed across many languages [@cho1999vot]. The source of the gender effect is also not totally clear, and studies differ on whether men truly have lower VOTs than women, or whether they just speak faster than women, on average (which is true for English) [@morris2008voice].

The response is `logVOT`. We log-transform VOT because this brings its distribution closer to normality, and because VOT can only be positive for voiceless stops, hence a model which can predict negative VOT would not make sense.


## Transitions {#transitionsdata}


<!-- TODO FUTURE: make description of this dataset more understandable for readers -->

This dataset is from @roberts2015effects, a study examining the factors which determine the speed of turn-taking in conversation. The data is posted as `transitions.txt` in an [OSF project](https://osf.io/dve6h/) [@transitions_data].

The dataset contains around 20,000 conversational transitions between speakers engaged in spontaneous conversation during telephone calls, from the [Switchboard Corpus](https://catalog.ldc.upenn.edu/ldc97s62) [@godfrey1997switchboard]. The dataset contains fifty variables, but ohly some of them are used in this book. Here is a brief explanation of some relevant variables (columns):

* `dur`: the duration (ms) of the floor transition between turn A (the turn preceding the floor transition), and turn B (the turn following the transition)
* `spkA`: the id of the speaker of the turn preceding the floor transition.
* `spkB`: the id of the speaker of the turn following the floor transition.
* `sexA`: the sex of the speaker of the turn preceding the floor transition.
* `sexB`: the sex of the speaker of the turn following the floor transition.
* `dialActA`: the dialogue act (e.g. yes-no question, wh-question, statement, answer, backchannel) of the turn preceding the floor transition.
* `dialActB`: the dialogue act of the turn following the floor transition.
* `uttNSylA`: the number of syllables in the turn preceding the floor transition.
* `uttNSylB`:  the number of syllables in the turn following the floor transition.
* `uttDurA`: the duration (ms) of the turn preceding the floor transition.
* `uttDurB`: the duration (ms) of the turn following the floor transition.


Letter A is used in the name of variables referring to the turn **preceding** the floor transition, whereas letter B is used in the name of variables referring to the turn **following** a floor transition.

## Packages

In addition to `languageR`, packages used in this book include:

* `arm` [@arm]

* `bookdown` [@bookdown]

* `rms` and `Hmisc` [@Hmisc; @rms]

* `influence.ME` [@influence.ME]

* `lsmeans` and successor `emmeans` [@lsmeans; @emmeans]

* "Tidyverse" packages `ggplot2`, `dplyr`, `tidyr`, etc. [@ggplot2; @dplyr; @tidyr]







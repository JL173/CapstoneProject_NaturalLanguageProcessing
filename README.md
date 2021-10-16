# CapstoneProject_NaturalLanguageProcessing

https://www.tidytextmining.com/index.html

## Task One - Obtaining and CLeaning the Data

- Tokenisation of text
	Identifying appropriate tokens such as words, punctuation, numbers. Writing a function that takes a file and returns the tokenised version.

- Profanity Filtering
	Removing profanity and other words we do not want to predict

#### Initial Stages of text processing

- Remove numbers - Simplify document
- Tokenisation - cut character sequences into word tokens
- Normalisation - map text and query term to same form
- Stemming - have different forms of a root to match
- Stop words - may omit very common words (or not)
- Filter words - remove words using a pre-defined filter

For bag-of-words - Create a TermDocumentMatrix

## Task Two - Exploratory Analysis

- Get some expectations of the data
	Some words are more frequent than others
	There will be spelling and grammatical errors
	There may be duplicate/replicate tokens

- Exploratory Analysis
	Perform a thorough analysis of the data, understanding the distribution of words and relationship between the words in the corpora.
	
- Understand the frequencies of words and word pairs
	Build figures and tables to understand variation in the frequencies of words and word pairs in the data
	
Questions to consider
- What are the distribution of word frequencies?
- What are the frequencies of 2-grams and 3-grams in the dataset?
- How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?
- How do you evaluate how many words come from foreign languages?
- Can you think of a way to increase the coverage -- identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?



## Task 3 - Modeling

- Build basic [n-gram model](http://en.wikipedia.org/wiki/N-gram) - using the exploratory analysis you performed, build a basic n-gram model for predicting the next word based on the previous 1, 2, or 3 words.

- Build a model to handle unseen n-grams - in some cases people will want to type a combination of words that does not appear in the corpora. Build a model to handle cases where a particular n-gram isn't observed.

Questions to consider
- How can you efficiently store an n-gram model (think Markov Chains)?
- How can you use the knowledge about word frequencies to make your model smaller and more efficient?
- How many parameters do you need (i.e. how big is n in your n-gram model)?
- Can you think of simple ways to "smooth" the probabilities (think about giving all n-grams a non-zero probability even if they aren't observed in the data) ?
- How do you evaluate whether your model is any good?
- How can you use [backoff models](http://en.wikipedia.org/wiki/Katz%27s_back-off_model) to estimate the probability of unobserved n-grams?

Hints, tips, and tricks

As you develop your prediction model, two key aspects that you will have to keep in mind are the size and runtime of the algorithm. These are defined as:
- Size: the amount of memory (physical RAM) required to run the model in R
- Runtime: The amount of time the algorithm takes to make a prediction given the acceptable input

Your goal for this prediction model is to minimize both the size and runtime of the model in order to provide a reasonable experience to the user.

Keep in mind that currently available predictive text models can run on mobile phones, which typically have limited memory and processing power compared to desktop computers. Therefore, you should consider very carefully

1. how much memory is being used by the objects in your workspace
2. how much time it is taking to run your model.

Ultimately, your model will need to run in a Shiny app that runs on the shinyapps.io server.

Tips, tricks, and hints

Here are a few tools that may be of use to you as you work on their algorithm:
- object.size(): this function reports the number of bytes that an R object occupies in memory
- Rprof(): this function runs the profiler in R that can be used to determine where bottlenecks in your function may exist. The profr package (available on CRAN) provides some additional tools for visualizing and summarizing profiling data.
- gc(): this function runs the garbage collector to retrieve unused RAM for R. In the process it tells you how much memory is currently being used by R.

There will likely be a tradeoff that you have to make in between size and runtime. For example, an algorithm that requires a lot of memory, may run faster, while a slower algorithm may require less memory. You will have to find the right balance between the two in order to provide a good experience to the user.


## Task 4 - Prediction Model

The goal of this exercise is to build and evaluate your first predictive model. You will use the n-gram and backoff models you built in previous tasks to build and evaluate your predictive model. The goal is to make the model efficient and accurate. 

- Build a predictive model based on the previous data modeling steps - you may combine the models in any way you think is appropriate.
- Evaluate the model for efficiency and accuracy - use timing software to evaluate the computational complexity of your model. Evaluate the model accuracy using different metrics like perplexity, accuracy at the first word, second word, and third word.

Questions to consider

- How does the model perform for different choices of the parameters and size of the model?

- How much does the model slow down for the performance you gain?

- Does perplexity correlate with the other measures of accuracy?

- Can you reduce the size of the model (number of parameters) without reducing performance?
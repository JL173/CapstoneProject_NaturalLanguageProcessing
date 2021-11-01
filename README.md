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



## Task Three - Modeling

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


## Task Four - Prediction Model

The goal of this exercise is to build and evaluate your first predictive model. You will use the n-gram and backoff models you built in previous tasks to build and evaluate your predictive model. The goal is to make the model efficient and accurate. 

- Build a predictive model based on the previous data modeling steps - you may combine the models in any way you think is appropriate.
- Evaluate the model for efficiency and accuracy - use timing software to evaluate the computational complexity of your model. Evaluate the model accuracy using different metrics like perplexity, accuracy at the first word, second word, and third word.

Questions to consider

- How does the model perform for different choices of the parameters and size of the model?

- How much does the model slow down for the performance you gain?

- Does perplexity correlate with the other measures of accuracy?

- Can you reduce the size of the model (number of parameters) without reducing performance?


## Task Five - Creative Exploration

So far you have used basic models to understand and predict words. In this next task, your goal is to use all the resources you have available to you (from the Data Science Specialization, resources on the web, or your own creativity) to improve the predictive accuracy while reducing computational runtime and model complexity (if you can). Be sure to hold out a test set to evaluate the new, more creative models you are building.

Tasks to accomplish

- Explore new models and data to improve your predictive model.

- Evaluate your new predictions on both accuracy and efficiency. 

Questions to consider

- What are some alternative data sets you could consider using? 

- What are ways in which the n-gram model may be inefficient?

- What are the most commonly missed n-grams? Can you think of a reason why they would be missed and fix that? 

- What are some other things that other people have tried to improve their model? 

- Can you estimate how uncertain you are about the words you are predicting? 


## Task Six - The Data Product

The goal of this exercise is to create a product to highlight the prediction algorithm that you have built and to provide an interface that can be accessed by others via a Shiny app...

Tasks to accomplish

 - Create a data product to show off your prediction algorithm You should create a Shiny app that accepts an n-gram and predicts the next word.

Questions to consider

- What are the most interesting ways you could show off your algorithm?

- Are there any data visualizations you think might be helpful (look at the Swiftkey data dashboard if you have it loaded on your phone)?

- How should you document the use of your data product (separately from how you created it) so that others can rapidly deploy your algorithm?

Tips, tricks, and hints

- Consider the size of the predictive model you have developed. You may have to sacrifice some accuracy to have a fast enough/small enough model to load into Shiny.


## Task Seven - Slide Deck

The goal of this exercise is to "pitch" your data product to your boss or an investor. The slide deck is constrained to be 5 slides or less and should

1. explain how your model works
2. describe its predictive performance quantitatively
3. show off the app and how it works. 

Tasks to accomplish

- Create a slide deck promoting your product. Write 5 slides using RStudio Presenter explaining your product and why it is awesome!

Questions to consider

- How can you briefly explain how your predictive model works?

- How can you succinctly quantitatively summarize the performance of your prediction algorithm?

- How can you show the user how the product works?

Tips, tricks, and hints

- The Rstudio presentation information is available [here](https://support.rstudio.com/hc/en-us/articles/200486468-Authoring-R-Presentations)



### Review Criteria for Final Submission

**Data Product** A Shiny app that takes as input a phrase (multiple words) in a text box input and outputs a prediction of the next word.

- Does the link lead to a Shiny app with a text input box that is running on shinyapps.io?

- Does the app load to the point where it can accept input?

- When you type a phrase in the input box do you get a prediction of a single word after pressing submit and/or a suitable delay for the model to compute the answer?

- Put five phrases drawn from Twitter or news articles in English leaving out the last word. Did it give a prediction for every one?

**Slide Deck** pitching your algorithm and app as if you were presenting to your boss or an investor.

- Does the link lead to a 5 slide deck on R Pubs?

- Does the slide deck contain a description of the algorithm used to make the prediction?

- Does the slide deck describe the app, give instructions, and describe how it functions?

- How would you describe the experience of using this app?

- Does the app present a novel approach and/or is particularly well done?

- Would you hire this person for your own data science startup company?
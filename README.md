# CapstoneProject_NaturalLanguageProcessing
 
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





# Load packages
if(!require(tm)){install.packages("tm"); library(tm)}
if(!require(qdap)){install.packages("qdap"); library(qdap)}
if(!require(SnowballC)){install.packages("SnowballC"); library(SnowballC)}

# Import text data
tweets <- read.csv("C:/Users/ahpvi/Documents/doutorado/cursos/text_mining/coffee.csv", stringsAsFactors = FALSE)

# View the structure of tweets
str(tweets)

# Isolate text from tweets: coffee_tweets
coffee_tweets <- tweets$text

#' There are two kinds of the corpus data type, the permanent corpus, 
#' PCorpus, and the volatile corpus, VCorpus. In essence, the difference 
#' between the two has to do with how the collection of documents is stored
#' in your computer.

#' o make a volatile corpus, R needs to interpret each element in our vector
#' of text, coffee_tweets, as a document. And the tm package provides what 
#' are called Source functions to do just that!

# Make a vector source: coffee_source
coffee_source <- VectorSource(coffee_tweets)

#' Now that we've converted our vector to a Source object, we pass it to 
#' another tm function, VCorpus(), to create our volatile corpus. 

#' The VCorpus object is a nested list, or list of lists. At each index of the VCorpus object, there is a PlainTextDocument object, which is a list containing actual text data (content), and some corresponding metadata (meta).

# To review a single document object (the 10th) you subset with double square brackets. 
coffee_corpus[[10]]

#' To review the actual text you index the list twice. To access the document's metadata, like timestamp, change [1] to [2]. Another way to review the plain text is with the content() function which doesn't need the second set of brackets.
coffee_corpus[[10]][1]
content(coffee_corpus[[10]])

# Make a volatile corpus: coffee_corpus
coffee_corpus <- VCorpus(coffee_source)

# Print out coffee_corpus
print(coffee_corpus)

# Print the 15th tweet in coffee_corpus
print(coffee_corpus[[15]])

# Print the contents of the 15th tweet in coffee_corpus
content(coffee_corpus[[15]])

# Now use content to review plain text
content(coffee_corpus[[10]])

#' If your text data is in a data frame you can use DataframeSource() for 
#' your analysis. The data frame passed to DataframeSource() must have a 
#' specific structure.


## Cleaning and processing text --------------------------------------

#' Common preprocessing functions include:
#' tolower(): Make all characters lowercase
#' removePunctuation(): Remove all punctuation marks
#' removeNumbers(): Remove numbers
#' stripWhitespace(): Remove excess whitespace

# Create the object: text
text <- "<b>She</b> woke up at       6 A.M. It\'s so early!  She was only 10% awake and began drinking coffee in front of her computer."

# Make lowercase
tolower(text)

# Remove punctuation
removePunctuation(text)

# Remove numbers
removeNumbers(text)

# Remove whitespace
stripWhitespace(text)

#' The qdap package offers other text cleaning functions. Each is useful in
#'  its own way and is particularly powerful when combined with the others.
#'  
#'  bracketX(): Remove all text within brackets (e.g. "It's (so) cool" becomes "It's cool")
#'  replace_number(): Replace numbers with their word equivalents (e.g. "2" becomes "two")
#'  replace_abbreviation(): Replace abbreviations with their full text equivalents (e.g. "Sr" becomes "Senior")
#'  replace_contraction(): Convert contractions back to their base words (e.g. "shouldn't" becomes "should not")
#'  replace_symbol() Replace common symbols with their word equivalents (e.g. "$" becomes "dollar")

# Remove text within brackets
bracketX(text)

# Replace numbers with words
replace_number(text)

# Replace abbreviations
replace_abbreviation(text)

# Replace contractions
replace_contraction(text)

# Replace symbols with words
replace_symbol(text)

#' Often there are words that are frequent but provide little information. 
#' These are called stop words, and you may want to remove them from your
#' analysis.

# List standard English stop words
stopwords("en")

# Print text without standard stop words
removeWords(text, stopwords("en"))

# Add "coffee" and "bean" to the list: new_stops
new_stops <- c("coffee", "bean", stopwords("en"))
               
# Remove stop words from text
removeWords(text, new_stops)

#' Still another useful preprocessing step involves word stemming and stem completion.

#' The tm package provides the stemDocument() function to get to a word's root. This function either takes in a character vector and returns a character vector, or takes in a PlainTextDocument and returns a PlainTextDocument.

# Create complicate
complicate <- c("complicated", "complication", "complicatedly")

# Perform word stemming: stem_doc
stem_doc <- stemDocument(complicate)

# Create the completion dictionary: comp_dict
comp_dict <- "complicate"

# Perform stem completion: complete_text 
complete_text <- stemCompletion(stem_doc, comp_dict)

# Print complete_text
complete_text

# Remove punctuation: rm_punc
text_data <- "In a complicated haste, Tom rushed to fix a new complication, too complicatedly."
rm_punc <- removePunctuation(text_data)

# Create character vector: n_char_vec
n_char_vec <- unlist(strsplit(rm_punc, split = " "))

# Perform word stemming: stem_doc
stem_doc <- stemDocument(n_char_vec)

# Print stem_doc
stem_doc

# Re-complete stemmed document: complete_doc
complete_doc <- stemCompletion(stem_doc, comp_dict)

# Print complete_doc
complete_doc

#' The tm package provides a function tm_map() to apply cleaning functions
#' to an entire corpus, making the cleaning steps easier.

#' clean_corpus() takes one argument, corpus, and applies a series of 
#' cleaning functions to it in order, then returns the updated corpus.

# Alter the function code to match the instructions
clean_corpus <- function(corpus){
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, content_transformer(tolower))
      corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "coffee", "mug"))
      corpus <- tm_map(corpus, stripWhitespace)
      return(corpus)
}

# Apply your customized function to the tweet_corp: clean_corp
clean_corp <- clean_corpus(tweet_corp)

# Print out a cleaned up tweet
clean_corp[[227]][1]

# Print out the same tweet in original form
tweets$text[227]


# http://sifaka.cs.uiuc.edu/ir/resources/mooctr.html
# https://www.tidytextmining.com/

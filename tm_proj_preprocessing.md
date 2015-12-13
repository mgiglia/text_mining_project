---
title: "Health Mangement System Note Preprocessing"
output: pdf_document
fontsize: 11pt
geometry: margin=1in
---

Matthew Giglia  
MSA 8225  
Web \& Text Mining  
Term Project  
Preprocessing  
  
***
  
### Health Mangement System Note Preprocessing
  

  
  
***  
#### Given Names for babies born in the U.S., 1880 to 2014  
  
In order to use the Health Management System (HMS) notes in this project, we need to make sure that all first names and last names are removed from the data.  For first names we'll be using a data set of all the given names for babies born in the United States from 1880 to 2014 as a 100 percent sample of the records from the Social Security Administration.  This data set is freely available for download at [www.data.gov](www.data.gov) at the following link.  
  
> [Baby Names from Social Security Card Applications-National Level Data](http://catalog.data.gov/dataset/baby-names-from-social-security-card-applications-national-level-data)  
>  
  
Downloading the zip file from the above link creates a folder called *names* that we've saved to our R working directory.  The folder contains a series of comma delimited files of the form "yobYYYY.txt", where YYYY is a year from 1880 to 2014.  Each comma delimited file contains the same three attributes:  
  
* **fname** - given baby name  
* **gndr** - gender code for the given baby name {F, M}  
* **cnt** - the frequency of of the baby name given on all national level Social Security Card applications for the year  
  
While the names themselves, and their respective genders and frequency counts might be interesting in their own right, in order to remove the first names from our HMS notes, we really only need a single vector containing just the unique first names.  To create this vector we'll need to build a function that loops in each of the data sets for the respective years and then stores the only the unique names from each previously run.  
  

```r
# Inputs: Let x be the folder containing the files from data.gov Let y be
# the lower bound on the years represented Let z be the upper bound on the
# years represented
babyNameLoader <- function(x = "./names/", y = 1880, z = 2014) {
    
    for (i in y:z) {
        
        filename <- paste(x, "/yob", i, ".txt", sep = "", collapse = NULL)
        
        temp <- read.csv(file = filename, header = FALSE, col.names = c("fname", 
            "gndr", "cnt"), stringsAsFactors = FALSE)
        
        if (i == y) {
            
            uniqueNames <- unique(temp$fname)
            
        } else {
            
            uniqueNames <- c(uniqueNames, temp$fname)
            uniqueNames <- unique(uniqueNames)
            
        }
        
    }
    
    uniqueNames
}

fnames <- babyNameLoader()

# Display the first six names in the list
head(fnames)
```

```
## [1] "Mary"      "Anna"      "Emma"      "Elizabeth" "Minnie"    "Margaret"
```
  
We now have a vector *fnames* that contains 93889 distinct first names that we may use to clean our data.  We next turn our attention to surnames.  
  
  
***  
#### Frequently Occurring Surnames from the Census 2000

The United States Census Bureau has published a list of frequently occurring surnames from the 2000 census in a single data set that can be retrieved using the following link:  
  
> [Frequently Occurring Surnames from the Census 2000](http://www.census.gov/topics/population/genealogy/data/2000_surnames.html?cssp=SERP)  
>  

Like the data sets from the Social Security applications containing the baby names, the surnames data set contains the name and a frequency count of the number of residents who completed the survey with that name.  Additionally it provides a rank, and proportion of survey respondents with that surname for given ethnicity.  The definition of a "frequently" occurring For our purposes we only need to retain the first attribute in the comma delimited file in order remove last names from our note text.  
  

```r
# Import the data from the CSV file downloaded from the zipped folder and
# saved in the working directory
app_c <- read.csv(file = "./surnames/app_c.csv", header = TRUE, stringsAsFactors = FALSE)

# View the imported data set's structure
str(app_c)
```

```
## 'data.frame':	151671 obs. of  11 variables:
##  $ name        : chr  "SMITH" "JOHNSON" "WILLIAMS" "BROWN" ...
##  $ rank        : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ count       : int  2376206 1857160 1534042 1380145 1362755 1127803 1072335 858289 804240 783051 ...
##  $ prop100k    : num  881 688 569 512 505 ...
##  $ cum_prop100k: num  881 1569 2138 2650 3155 ...
##  $ pctwhite    : chr  "73.35" "61.55" "48.52" "60.71" ...
##  $ pctblack    : chr  "22.22" "33.80" "46.72" "34.54" ...
##  $ pctapi      : chr  "0.40" "0.42" "0.37" "0.41" ...
##  $ pctaian     : chr  "0.85" "0.91" "0.78" "0.83" ...
##  $ pct2prace   : chr  "1.63" "1.82" "2.01" "1.86" ...
##  $ pcthispanic : chr  "1.56" "1.50" "1.60" "1.64" ...
```
  
After viewing the structure of our imported data set we can see that the surnames are stored in the *name* attribute and that there are 151671 observations meaning we'll have a set of 151671 surnames from which we may remove last names from our note text.  Since we don't need the other attributes for this analysis we'll store the last names in a vector called *surnames* and delete the imported data set so that it doesn't take away from our memory for future objects.  
  

```r
# Store the surnames in their own vector
surnames <- unique(app_c$name)

# Delete the app_c data set from memory since it's not longer required.
remove(app_c)

# View the first six observations stored in the surnames vector
head(surnames)
```

```
## [1] "SMITH"    "JOHNSON"  "WILLIAMS" "BROWN"    "JONES"    "MILLER"
```
  
   
***    
#### Text Mining Preprocessing Functions  
  
R has a few packages that may be employed for text mining purposes, however the best for removing words, numbers, stemming words and creating term-document matrices is the "tm" package.  The "tm" package is freely available from the [Comprehensive R Archive Network](https://www.cran.r-project.org) and more information can be found at the following link:  
  
>  [tm: Text Mining Package](https://www.cran.r-project.org/web/packages/tm/tm.pdf)
>  
  
Let's start with a small proof of concept before we import our clinical note data into R and begin the preprocessing techniques against them.  
    
Suppose we have three documents, each a slightly augmented nursery rhyme:  
  
* Jack and Jill went up the hill To fetch a 5 pails of water. Jack fell down and broke his crown, And Jill came tumbling after. Jack was taken by ambulance to Strong Memorial Hospital to see Dr. Jones.  Dr. Jones can be reached at 555-123-3456.   
* Little Jack Horner Sat in the corner, Eating a Christmas pie; He put in his thumb,
And pulled out a plum, And said 'What a good boy am I!  Later Jack complained over intense stomach pain.  His subscriber id number is 20021234567.  
* William H. Dumphy sat on a wall, William H. Dumphy had a great fall. All the king's horses (between 60 and 80 total horses) and all the king's men (including 35 medical practitioners) Couldn't put William H. together again.  


```r
# Store the three nursery ryhmes in a vector called nursery
nursery <- c("Jack and Jill went up the hill To fetch a 5 pails of water. Jack fell down and broke his crown, And Jill came tumbling after. Jack was taken by ambulance to Strong Memorial Hospital to see Dr. Jones.  Dr. Jones can be reached at 555-123-3456.", 
    "Little Jack Horner Sat in the corner, Eating a Christmas pie; He put in his thumb, And pulled out a plum, And said 'What a good boy am I!  Later Jack complained over intense stomach pain.  His subscriber id number is 20021234567.", 
    "William H. Dumphy sat on a wall, William H. Dumphy had a great fall. All the king's horses (between 60 and 80 total horses) and all the king's men (including 35 medical practitioners) Couldn't put William H. together again.")
```
  
The *nursery* vector will become our corpus (i.e., a collection of documents) with each element of the vector a different nursery rhyme.  The individual nursery rhymes are our individual documents.  Looking over the corpus we see that it contains documents with punctuation, numbers, first names, and last names.  We'll now use functions from the text mining package "tm" plus our previously created vectors containing first names (*fnames*) and last names (*surnames*) to clean the documents of any information that could be used to tie back to an individual (in this case, Jack, Jill, Jack Horner, and Humpty Dumpty).  
  
We start initially by loading the "tm" package in R, and then creating a "Corpus" object in R out of our *nursery* vector.  
  

```r
# Install the tm: text mining package install.packages('tm')

# Load the tm library
require(tm)
```

```
## Loading required package: tm
## Loading required package: NLP
```

```r
# Change the nursery vector to a data frame with the original vector as text
nursery <- data.frame(text = nursery, stringsAsFactors = FALSE)

# Now change the nursery data frame to a tm Corpus object.  VectorSource
# let's the Corpus function know that the documents are stored in a
# particular vector inside the data frame (use ?VectorSource for more info.)
nursery <- Corpus(VectorSource(nursery$text))
nursery
```

```
## <<VCorpus>>
## Metadata:  corpus specific: 0, document level (indexed): 0
## Content:  documents: 3
```
  
*nursery* is now a corpus R object with three documents.  In R you may actually assign various metadata information to the documents in the corpus such as the author, a datetimestamp, a description, a heading, an id, a language and an origin.  This isn't necessary for our proof of concept so we'll ignore that for now but it is worth nothing for future analyses.  
  
As a first preprocessing step we'll want to change all of the text to lower case, this will allow us to ensure matching when we want to remove the names from our data, and later will allow us to stem the terms and take frequencies on the stemmed terms.  
  

```r
# Make all of the terms lowercase in the documents.  tm_map allows for text
# mining transformations on a corpus.  We want to transform the content of
# the document (not the meta data) so we use content_transformer.  The
# function 'tolower' in the base package will make everything lowercase.
nursery <- tm_map(nursery, content_transformer(tolower))

# View the result to verify that the documents are now all lower case.
lapply(content(nursery), as.character)
```

```
## [[1]]
## [1] "jack and jill went up the hill to fetch a 5 pails of water. jack fell down and broke his crown, and jill came tumbling after. jack was taken by ambulance to strong memorial hospital to see dr. jones.  dr. jones can be reached at 555-123-3456."
## 
## [[2]]
## [1] "little jack horner sat in the corner, eating a christmas pie; he put in his thumb, and pulled out a plum, and said 'what a good boy am i!  later jack complained over intense stomach pain.  his subscriber id number is 20021234567."
## 
## [[3]]
## [1] "william h. dumphy sat on a wall, william h. dumphy had a great fall. all the king's horses (between 60 and 80 total horses) and all the king's men (including 35 medical practitioners) couldn't put william h. together again."
```
    
After viewing the documents in our corpus, we see that now all of our terms are lower case.  Next we'll remove punctuation by performing much the same process except this time we'll use the "tm" function *removePunctuation* in our *tm_map*.  
  

```r
# Remove punctuation
nursery <- tm_map(nursery, content_transformer(removePunctuation))

# View the results to verify that the documents no longer have punctuation
lapply(content(nursery), as.character)
```

```
## [[1]]
## [1] "jack and jill went up the hill to fetch a 5 pails of water jack fell down and broke his crown and jill came tumbling after jack was taken by ambulance to strong memorial hospital to see dr jones  dr jones can be reached at 5551233456"
## 
## [[2]]
## [1] "little jack horner sat in the corner eating a christmas pie he put in his thumb and pulled out a plum and said what a good boy am i  later jack complained over intense stomach pain  his subscriber id number is 20021234567"
## 
## [[3]]
## [1] "william h dumphy sat on a wall william h dumphy had a great fall all the kings horses between 60 and 80 total horses and all the kings men including 35 medical practitioners couldnt put william h together again"
```
  
And now finally we'll remove all of the numbers from the documents before moving on to the first names and the surnames.  
  

```r
# Remove numbers
nursery <- tm_map(nursery, content_transformer(removeNumbers))

# View the results to verify that the documents no longer have numbers
lapply(content(nursery), as.character)
```

```
## [[1]]
## [1] "jack and jill went up the hill to fetch a  pails of water jack fell down and broke his crown and jill came tumbling after jack was taken by ambulance to strong memorial hospital to see dr jones  dr jones can be reached at "
## 
## [[2]]
## [1] "little jack horner sat in the corner eating a christmas pie he put in his thumb and pulled out a plum and said what a good boy am i  later jack complained over intense stomach pain  his subscriber id number is "
## 
## [[3]]
## [1] "william h dumphy sat on a wall william h dumphy had a great fall all the kings horses between  and  total horses and all the kings men including  medical practitioners couldnt put william h together again"
```
  
Fantastic!  Removing numbers will ensure that no member identifiers or phone numbers will be in any of our documents.  Next we need to ensure that we don't have any first names or last names in the data.  We'll use the *fnames* and *surnames* vectors that we created previously and use the "tm" package function *removeWords* in our *tm_map*.  First we recall that our *fnames* vector was mixed case and our *surnames* vector was all capital letters, so we'll first need to make these lower case as well before we attempt to remove them from our corpus' documents.  
  

```r
# Apply tolower to fnames
fnames <- tolower(fnames)

# Apply tolower to surnames
surnames <- tolower(surnames)

# Remove names from the documents using removeWords The remove words
# function can only handle 1,000 words at a time, so we'll need to loop
# through all of the words that are in the fname or surnames vectors.

# Let x be a corpus Let y be a vector containing words to remove
removeManyWords <- function(x, y) {
    
    n <- ceiling(length(y)/1000)
    s <- 1
    e <- 1000
    
    for (i in 1:n) {
        
        x <- tm_map(x, content_transformer(removeWords), y[s:e])
        s <- s + 1000
        e <- e + 1000
        
    }
    
    x
    
}

# Remove first names using fnames
nursery <- removeManyWords(nursery, fnames)

# Remove last names using surnames
nursery <- removeManyWords(nursery, surnames)

# Remove white space for readability
nursery <- tm_map(nursery, content_transformer(stripWhitespace))

# View the results to verify that the documents no longer contain first or
# last names
lapply(content(nursery), as.character)
```

```
## [[1]]
## [1] " up a pails of broke his after ambulance memorial hospital dr dr reached at "
## 
## [[2]]
## [1] " eating a put his thumb pulled out a what a i complained intense stomach his subscriber id number is "
## 
## [[3]]
## [1] " h a h a horses between total horses including medical practitioners couldnt put h together again"
```
  
It's amazing what words are used as names!  The original nursery rhyme of "Jack and Jill" has been reduced to only a few words: "up", "a", "pails", "of", "broke", "his", "after."  This implies that words such as "and", "went", "hill", "to", "fetch", "water", "fell", "down", "crown", "came", and "tumbling" are either first names or last names!  Let's do a quick check to see which words fell into which name category just for fun.  
  

```r
check.if.name <- c("and", "went", "hill", "to", "fetch", "water", "fell", "down", 
    "crown", "came", "tumbling")

check.if.name %in% fnames
```

```
##  [1] FALSE FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE
```

```r
check.if.name %in% surnames
```

```
##  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
```
  
And there we have it, "hill" and "down" are first names, but all of those removed words happened to be last names with more than 100 people in the 2000 census.  Even though it appears as though most of the words were removed from removing such an exhaustive list of first and last names, we feel relatively confident that the clinical notes will retain most of their predictive power since the words we expect to be meaningful in a duo-mining model for inpatient admits or ER visits would be words that are clinical in nature and therefore are unlikely to be used as names.  
  
  
***  
#### Generalized Initial Preprocessing Function
  
We're now ready to apply what we've learned from the initial preprocessing for names to our clinical notes.  Additional preprocessing such as the removal of stop words, stemming, and then turning the corpus into a term-document matrix will also be performed on the clinical notes, but at that point we'll be able to look at the notes here in R since we'll be reasonable assured that all the first names and last names have been removed.  Since we've already seen how the proof of concept above worked with the nursery rhymes at the individual preprocessing steps we can generalize the entire process in the form of function.  Then once we have our clinical notes corpus loaded into R we'll simply use the function on the corpus to get us to the point where the following preprocessing steps have already been completed:  
  
* Turn all the terms to lower case  
* Remove punctuation  
* Remove numbers  
* Remove first names  
* Remove last names  
* Strip white space  
  
We'll build this function now:  
  

```r
# Let x be a Corpus object
initial.tm.Preprocess <- function(x) {
    
    fun.message <- c("initial.tm.Preprocess started at:", date())
    print(fun.message)
    
    # make lower case
    x <- tm_map(x, content_transformer(tolower))
    
    fun.message <- c("tolower completed at:", date())
    print(fun.message)
    
    # remove punctuation
    x <- tm_map(x, content_transformer(removePunctuation))
    
    fun.message <- c("removePunctuation completed at:", date())
    print(fun.message)
    
    
    # remove numbers
    x <- tm_map(x, content_transformer(removeNumbers))
    
    fun.message <- c("removeNumbers completed at:", date())
    print(fun.message)
    
    
    # remove first names
    x <- removeManyWords(x, fnames)
    
    fun.message <- c("removeManyWords first names completed at:", date())
    print(fun.message)
    
    
    # remove last names
    x <- removeManyWords(x, surnames)
    
    fun.message <- c("removeManyWords surnames completed at:", date())
    print(fun.message)
    
    
    # strip white space
    x <- tm_map(x, content_transformer(stripWhitespace))
    
    fun.message <- c("stripWhitespace completed at:", date())
    print(fun.message)
    
    
    # return x
    x
    
}
```
  
  
***  
#### Import of the Health Management System Notes and Complete the Preprocsessing  
  
We're now totally ready to import the Health Management System Notes into R.  The notes are currently stored in a pipe delimited text file in a folder of our working directory called *vNotes* which was created by SAS using a SAS program that is not shown here.  We'll load the notes as a data frame and then create a corpus from the *note_text* field.  Then we'll used our *generalized preprocessing function* from the last section to clean the notes of any punctuation, numbers, or names.  Once we've completed that we'll store the notes back into a data frame and export them to another pipe delimited text file for use in our advanced text mining software.  
  

```r
# import the unprocessed pipe file into R
vNotes.df <- read.table("./vNotes/corpus.txt", sep = "|", stringsAsFactors = FALSE, 
    header = TRUE)

# the note text is stored in the attribute *note_text*, use this to create a
# corpus object.  the other attributes in the data frame are the clinical
# notes meta data, such as the subject id, and the date and time of the
# note.
vNotes.corpus <- Corpus(VectorSource(vNotes.df$note_text))
vNotes.corpus
```

```
## <<VCorpus>>
## Metadata:  corpus specific: 0, document level (indexed): 0
## Content:  documents: 31715
```


  
We've successfully loaded the unprocessed pipe delimited file of Health Management System notes from SAS into R and created a corpus from the *note_text*.  31,715 unique documents were loaded into our corpus, each representing a different note filled out by a nurse in our Health Management System between October 1, 2014 and October 31, 2014.  Now we'll run the generalized preprocessing function on the corpus prior to reviewing it.  
  

```r
# use the generalized initial preprocessing function to remove punctuation,
# numbers, first names, last names, make lower case and strip white space
vNotes.corpus <- initial.tm.Preprocess(vNotes.corpus)
```

```
## [1] "initial.tm.Preprocess started at:" "Thu Nov 19 17:12:27 2015"         
## [1] "tolower completed at:"    "Thu Nov 19 17:12:28 2015"
## [1] "removePunctuation completed at:" "Thu Nov 19 17:12:30 2015"       
## [1] "removeNumbers completed at:" "Thu Nov 19 17:12:32 2015"   
## [1] "removeManyWords first names completed at:"
## [2] "Thu Nov 19 22:28:55 2015"                 
## [1] "removeManyWords surnames completed at:"
## [2] "Fri Nov 20 06:09:19 2015"              
## [1] "stripWhitespace completed at:" "Fri Nov 20 06:09:21 2015"
```

```r
# view the first three notes to verify that the function worked
lapply(content(vNotes.corpus[1:3]), as.character)
```

```
## [[1]]
## [1] " calll pcp spoke receptionistrequested phone "
## 
## [[2]]
## [1] "concurrent approved additional pt visits criteria utilized excellus exchange meets criteria approved authorization dates approval sent yes"
## 
## [[3]]
## [1] "initial member spoke member construction daytime hours available until pm requests next if member advise return writers initial mcm follow up within one if contact rcostello rn"
```

The three notes show that numbers, punctuation, names and white space have all been removed.  The clinical notes are now ready for analysis using our text mining software.  We'll now take the clean content from our corpus and attach it back to the data frame that contained the other note meta data, replacing the *note_text* attribute with this cleaned version.  Then we'll export that data frame back out to pipe delimited file for later use.  




# Explore the Request Tracker (RT) Database.
# Version 4.0 of the RT web application is used.

library(RMySQL)
# library(qdap)
library(RCurl)
library(RJSONIO)
library(stringr)
library(tm)
library(wordcloud)

db_key = '%db_key%'

# -----------------------------------------
# Define some functions

clean.text <- function(some_txt)
{
	some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
	some_txt = gsub("@\\w+", "", some_txt)
	some_txt = gsub("[[:punct:]]", "", some_txt)
	some_txt = gsub("[[:digit:]]", "", some_txt)
	some_txt = gsub("http\\w+", "", some_txt)
	some_txt = gsub("[ \t]{2,}", "", some_txt)
	some_txt = gsub("^\\s+|\\s+$", "", some_txt)
	some_txt = gsub("amp", "", some_txt)
	# define "tolower error handling" function
	try.tolower = function(x)
	{
		y = NA
		try_error = tryCatch(tolower(x), error=function(e) e)
		if (!inherits(try_error, "error"))
		y = tolower(x)
		return(y)
	}

	some_txt = sapply(some_txt, try.tolower)
	some_txt = some_txt[some_txt != ""]
	names(some_txt) = NULL
	return(some_txt)
}

lookup_sentiment <- function (text, key) {
 
	text <- URLencode(text); 
	# Save all the spaces, then get rid of the weird characters that break the API, then convert back the URL-encoded spaces.
	text <- str_replace_all(text, "%20", " ");
	text <- str_replace_all(text, "%\\d\\d", "");
	text <- str_replace_all(text, " ", "%20"); 
	if (str_length(text) > 360){
		text <- substr(text, 0, 359);
	}
	
	# Web Services call to obtain sentiment for word.
	data <- getURL(paste("http://api.datumbox.com/1.0/TwitterSentimentAnalysis.json?api_key=", key, "&text=", text, sep = ""))
 
	js <- fromJSON(data, asText=TRUE);
 
	# get mood probability
	sentiment = js$output$result
	return(list(sentiment=sentiment))
}

# -----------------------------------------
# Make a database connection and retrieve the data.

# The ~/.my.conf file defines the rt_nectar database group
# that specifies the details needed to access the rt_nectar database:
# [rt_nectar]
# database=rt_nectar
# user=mysql_root
# password=*******

db_connection <- dbConnect(MySQL(), group = "rt_nectar")

# Explore the database like so:
# dbListTables(db_connection)
#
# [1] "ACL"                     "Articles"                "Attachments"             "Attributes"             
# [5] "CachedGroupMembers"      "Classes"                 "CustomFieldValues"       "CustomFields"           
# [9] "GroupMembers"            "Groups"                  "Links"                   "ObjectClasses"          
# [13] "ObjectCustomFieldValues" "ObjectCustomFields"      "ObjectTopics"            "Principals"             
# [17] "Queues"                  "ScripActions"            "ScripConditions"         "Scrips"                 
# [21] "Templates"               "Tickets"                 "Topics"                  "Transactions"           
# [25] "Users"                   "sessions"               

# dbListFields(db_connection, "Tickets")
#
# [1] "id"              "EffectiveId"     "Queue"           "Type"            "IssueStatement"  "Resolution"     
# [7] "Owner"           "Subject"         "InitialPriority" "FinalPriority"   "Priority"        "TimeEstimated"  
# [13] "TimeWorked"      "Status"          "TimeLeft"        "Told"            "Starts"          "Started"        
# [19] "Due"             "Resolved"        "LastUpdatedBy"   "LastUpdated"     "Creator"         "Created"        
# [25] "Disabled"       
   
# dbGetQuery(db_connection, "select count(*) from Tickets")
#
#      count(*)
# 1     9274

subjects <- dbGetQuery(db_connection, "select Subject from Tickets")

dbDisconnect(db_connection)

# -----------------------------------------
# Preprocess the subject-line text

# clean text
subjects_clean = clean.text(subjects[[1]])
subject_count = length(subjects_clean)
# Data frame (text, sentiment)
subjects_df = data.frame(text=subjects_clean, sentiment=rep("", subject_count), stringsAsFactors=FALSE)

print("Lookup sentiments...")
# Apply function lookup_sentiment
sentiment = rep(0, subject_count)
for (subject_index in 1:subject_count)
{
	tmp = lookup_sentiment(subjects_clean[subject_index], db_key)
	subjects_df$sentiment[subject_index] = tmp$sentiment  
	print(paste(subject_index," of ", subject_count, ": " , tmp$sentiment))
}
  
# Delete rows with no sentiment
subjects_df <- subjects_df[subjects_df$sentiment != "",]

# Establish sentiment categories
sentiments = levels(factor(subjects_df$sentiment))

# Figure the relative percentage of positive, negative and neutral subject lines.
# Convert into labels with attached percentages for each subject-line.
labels <-  lapply(sentiments, function(sentiment) {
		percent <- length((subjects_df[subjects_df$sentiment == sentiment,])$text) / length(subjects_df$sentiment) * 100
		percent_str <- format(round(percent, 2), nsmall = 2)
		label <- paste(sentiment, percent_str, "%")
	}
)

# More scrubbing of subject-lines while assembling a vector of texts.
sentiment_count = length(sentiments)
sentiment_docs = rep("", sentiment_count)
for (sentiment_index in 1:sentiment_count)
{
	tmp = subjects_df[subjects_df$sentiment == sentiments[sentiment_index],]$text
	tmp = str_replace_all(tmp,"[^[:graph:]]", " ") 
	sentiment_docs[sentiment_index] = paste(tmp,collapse=" ")
}
# Remove stopwords from texts.
sentiment_docs = removeWords(sentiment_docs, stopwords("english"))

# Assemble the vector of texts into a corpus of documents
# and attach the sentiment labels to the term-document-matrix as column names.
corpus = Corpus(VectorSource(sentiment_docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = labels

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(sentiment_count, "Dark2"), scale = c(3,.5), random.order = FALSE, title.size = 1.5)

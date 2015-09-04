# Explore the Request Tracker (RT) Database.
# Version 4.0 of the RT web application is used.

# This script performs cluster analysis on the email contents

library(RMySQL)
# library(qdap)
library(stringr)
library(tm)
library(XML)

library(RTextTools)

# -----------------------------------------
# Define some functions

clean.text <- function(some_txt)
{
	some_txt = gsub("\\n", " ", some_txt)
	some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", some_txt)
	some_txt = gsub("@\\w+", " ", some_txt)
	some_txt = gsub("[[:punct:]]", " ", some_txt)
	some_txt = gsub("[[:digit:]]", " ", some_txt)
	some_txt = gsub("http\\w+", " ", some_txt)
	some_txt = gsub("[ \t]{2,}", " ", some_txt)
	some_txt = gsub("^\\s+|\\s+$", " ", some_txt)
	some_txt = gsub("amp", " ", some_txt)
	
	# Define "tolower error handling" function
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
   
# dbGetQuery(db_connection, "select distinct ContentType from Attachments")
#
#                                                               ContentType
# 1                                                               text/plain
# 2                                                                text/html
# 3                                                    multipart/alternative
# 4                                                          multipart/mixed
# 5                                                                image/png
# 6                                                        multipart/related
# 7                                                 application/octet-stream
# 8                                                         multipart/report
# 9                                                  message/delivery-status
# 10                                                          message/rfc822
# 11                                                              image/tiff
# 12                                                              image/jpeg
# 13                                                     text/rfc822-headers
# 14                                                         application/rtf
# 15                                                                text/csv
# 16                                                              text/x-log
# 17                                                               image/gif
# 18                                                         application/pdf
# 19                                                           text/calendar
# 20                                                   application/x-msexcel
# 21                                                        application/x-sh
# 22       application/vnd.openxmlformats-officedocument.spreadsheetml.sheet
# 23                                                    text/x-python-script
# 24                                                    application/pgp-keys
# 25                                                      application/msword
# 26                                                        multipart/signed
# 27                                             application/pkcs7-signature
# 28 application/vnd.openxmlformats-officedocument.wordprocessingml.document
# 29                                      application/x-iwork-keynote-sffkey
# 30                                                               image/jpg
# 31                                                      application/pkcs10
# 32                                               application/pgp-signature
# 33                                                               image/bmp
# 34                                                application/vnd.ms-excel
# 35                                                application/x-javascript
# 36                                                         application/ics
# 37                                                      application/x-yaml
# 38                                                                text/rtf
# 39                                                application/java-archive
# 40                                                      application/x-gzip
# 41                                                                    text
# 42                                                        application/gzip
# 43                                                     application/ms-tnef
# 44                                                           text/x-python
# 45                                                         application/zip
# 46                                            application/x-zip-compressed
# 

sql_template <- 
	"select t.id, t.created, t.subject, Convert(a.content using utf8) as content, a.ContentType
		from Tickets t
		join Transactions tr on tr.ObjectId = t.id
		join (
				select id, TransactionId, content, ContentType
				from Attachments a1
				where ((parent = 0 and content is not null) or (content is not null and content != ''))
					and (ContentType = 'text/plain' or ContentType = 'text/html' or ContentType = 'text')
					and creator != 1 # exclude system generated responses
					and (
						select count(*)
							from attachments a2
							where a1.transactionid = a2.transactionid and a1.id > a2.id
							and ((a2.parent = 0 and a2.content is not null) or (a2.content is not null and a2.content != ''))
					) = 0
		) a on a.TransactionId = tr.id
		# where t.EffectiveId = %effective_id%
		and t.type = 'ticket'
		order by t.created;
"
# Content types from this query are:
# 1  text/plain
# 2   text/html
# 3    text/csv
# 4   image/png
# 5        text

effective_id = 2
record_id <- as.character(effective_id)
record_id <- dbEscapeStrings(db_connection, record_id)
sql <- str_replace_all(sql_template,"%effective_id%", record_id)


subjects <- dbGetQuery(db_connection, sql)

dbDisconnect(db_connection)

# -----------------------------------------
# Preprocess the content text

message_index <- 4

# Column indicies
id_index <- 1
created_index <- 2
subject_index <- 3
content_index <- 4
content_type_index <- 5

# Preallocate space for results
message_count <- length(subjects[[id_index]])
messages <- rep("", message_count)

for (message_index in 1:message_count) {
	subject <- subjects[[subject_index]][message_index]
	content <- subjects[[content_index]][message_index]
	content_type <- subjects[[content_type_index]][message_index]
	if (content_type == "text/html") {
		html <- htmlParse(content, asText = TRUE)
		message_nodes <- xpathSApply(html, '//text()')
		message <- paste(sapply(message_nodes, xmlValue), collapse = "")
	} else {
		message <- content
	}
	# Combine subject-line with message-body.
	message <- paste(clean.text(subject), clean.text(message), sep=" ")
	# Remove non-UTF-8 characters.
	message = str_replace_all(message,"[^[:graph:]]", " ") 
	messages[message_index] <- message
}

messages = removeWords(messages, stopwords("english"))
# Assemble the vector of texts into a corpus of documents.
corpus = Corpus(VectorSource(messages))


# Create document-term matrix, passing data cleaning options
# Stem the words to avoid multiples of similar words
# Need to set wordLength to minimum of 1 because "r" a likely term
dtm <- create_matrix(messages, 
                     stemWords=TRUE, 
                     removeStopwords=FALSE, 
                     minWordLength=1,
                     removePunctuation= TRUE)


# -----------------------------------------

# Assemble the vector of texts into a corpus of documents
# and attach the sentiment labels to the term-document-matrix as column names.
corpus = Corpus(VectorSource(sentiment_docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = labels

# Comparison word cloud
# This fails with the error message that the colour range for the palette should be 8 max.
comparison.cloud(tdm, scale = c(3,.5), random.order = FALSE, title.size = 1.5)

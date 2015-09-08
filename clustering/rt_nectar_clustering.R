# Explore the Request Tracker (RT) Database.
# Version 4.0 of the RT web application is used.

# This script performs cluster analysis on the email contents

library(RMySQL)
# library(qdap)
library(stringr)
library(tm)
library(XML)
library(foreach)
library(doMC)
library(ggplot2)
library(bigmemory)
library(biganalytics)

library(RTextTools)

# -----------------------------------------
# Define some constants

project_storage_path <- "/Users/developer/git/R_data_analytics_textmining"
input_folder_path <- paste(project_storage_path, "big_matrices", sep = "/")
output_folder_path <- paste(project_storage_path, "big_matrices", sep = "/")

# -----------------------------------------
# Define some functions

# An empty function for Comments
Comment <- function(`@Comments`) {invisible()}

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

# Create document-term matrix, passing data cleaning options
# Stem the words to avoid multiples of similar words
# Need to set wordLength to minimum of 1 because "r" a likely term
document_term_matrix <- create_matrix(messages, 
                     stemWords=TRUE, 
                     removeStopwords=TRUE, 
                     minWordLength=1,
                     removePunctuation= TRUE)

# Check the results
# findFreqTerms(dtm, lowfreq=2000)

# Apply kmeans
# kmeans5 <- kmeans(document_term_matrix, 5)

# Merge cluster assignment back to keywords
#messages_with_cluster <- as.data.frame(cbind(messages, kmeans5$cluster))
#names(messages_with_cluster) <- c("message", "kmeans5")

# Make df for each cluster result, quickly "eyeball" results
#cluster1 <- subset(messages_with_cluster, subset=kmeans5 == 1)
#cluster2 <- subset(messages_with_cluster, subset=kmeans5 == 2)
#cluster3 <- subset(messages_with_cluster, subset=kmeans5 == 3)
#cluster4 <- subset(messages_with_cluster, subset=kmeans5 == 4)
#cluster5 <- subset(messages_with_cluster, subset=kmeans5 == 5)

# Using multi-core
# Run kmeans for all clusters up to 100
# Limited to 1 as otherwise 16Gig of compressed memory is not enough when using kmeans.
registerDoMC(2)

# Benchmark start time.
start_time <- Sys.time()

cluster_size_count <- 6

# Use big.matrix otherwise memory consumption ends up proportional
# to each core used.
big.dtm <- as.big.matrix(as.matrix(document_term_matrix), type = "double", separated = FALSE,
      backingfile = "big.dtm.matrix", backingpath = output_folder_path, descriptorfile = "big.dtm.desc",
      binarydescriptor=FALSE, shared=TRUE)

# Need to set working directory as there seems to be a bug in the attach.resource code
# where the directory path gets lost and instead is set to '.'.
setwd(output_folder_path) 
      
costs <- foreach(cluster_size_index = 1:cluster_size_count, .combine=rbind, .packages=c('doMC')) %dopar% {
    cat("\nCluster: ", cluster_size_index)
	# Use bigkmeans instead of kmeans,
	# otherwise memory consumption ends up around 20-30Gig and the computer dies.
	kmeans <- bigkmeans(x=big.dtm, centers=cluster_size_index, iter.max=100)
	cbind(cluster_size_index, sum(kmeans$withinss)) # For the inbuilt kmeans would be kmeans$tot.withinss
}

# Benchmark stop time and record duration.
# With 2 cores and 6 clusters max takes about 80min.
duration = difftime(Sys.time(), start_time, units = "secs")
cat("\nMulti-core kmeans duration/sec: ", duration, "\n")
# -----------------------------------------
# Exploring/benchmarking kmeans calls with different matrix implementations.

Comment( `

# Benchmark stop time and record duration.
duration = difftime(Sys.time(), start_time, units = "secs")
cat("\nMulti-core kmeans duration/sec: ", duration, "\n")

# Benchmark start time.
start_time <- Sys.time()
km <- kmeans(x=dtm, centers=1, iter.max=100)
# Benchmark stop time and record duration.
duration = difftime(Sys.time(), start_time, units = "secs")
cat("\nkmeans duration/sec: ", duration, "\n")

# Benchmark start time.
start_time <- Sys.time()
big <- bigkmeans(x=dtm, centers=1, iter.max=100)
# Benchmark stop time and record duration.
duration = difftime(Sys.time(), start_time, units = "secs")
cat("\bigkmeans duration/sec: ", duration, "\n")

# Benchmark start time.
start_time <- Sys.time()
really_big <- bigkmeans(x=big.dtm, centers=1, iter.max=100)
# Benchmark stop time and record duration.
duration = difftime(Sys.time(), start_time, units = "secs")
cat("\nbig.matrix bigkmeans duration/sec: ", duration, "\n")

`)
# -----------------------------------------
# Plotting output of kmeans as a function of cluster size.

cost_df <-data.frame(costs)
names(cost_df) <- c("cluster", "cost")

#Calculate lm's for emphasis
#lm(cost_df$cost[1:10] ~ cost_df$cluster[1:10])
#lm(cost_df$cost[10:19] ~ cost_df$cluster[10:19])
#lm(cost_df$cost[20:100] ~ cost_df$cluster[20:100])

#cost_df$fitted <- ifelse(cost_df$cluster <10, (19019.9 - 550.9*cost_df$cluster), 
#                         ifelse(cost_df$cluster <20, (15251.5 - 116.5*cost_df$cluster),
#                         (13246.1 - 35.9*cost_df$cluster)))

#Cost plot
ggplot(data=cost_df, aes(x=cluster, y=cost, group=1)) + 
	theme_bw(base_family="Garamond") + 
	geom_line(colour = "darkgreen") +
	theme(text = element_text(size=20)) +
	ggtitle("Reduction In Cost For Values of 'k'\n") +
	xlab("\nClusters") + 
	ylab("Within-Cluster Sum of Squares\n") +
	scale_x_continuous(breaks=seq(from=0, to=6, by= 1))
#geom_line(aes(y= fitted), linetype=2)

# -----------------------------------------
# Word cloud for message text.

# Assemble the vector of texts into a corpus of documents.

Comment( `
messages = removeWords(messages, stopwords("english"))
corpus = Corpus(VectorSource(messages))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)

# Comparison word cloud
# This fails with the error message that the colour range for the palette should be 8 max.
comparison.cloud(tdm, scale = c(3,.5), random.order = FALSE, title.size = 1.5)
`)


# Clear out the workspace.
#rm(list = ls())
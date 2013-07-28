###############################################################################
## Build off of the data we scraped using Python and finalize the datasets
## for analysis
###############################################################################

# set the directory
setwd("~/GitHub/SearchNetwork")

## load the packages
library(rmongodb)
library(XML)
library(plyr)
library(stringr)
library(reshape2)


###############################################################################
## Connect to our MongoDB -- on my network, not local to my machine
###############################################################################

# create the connection object
mongo <- mongo.create(host="192.168.1.69")

# show that we are connected
mongo.is.connected(mongo)

# list the databases
mongo.get.databases(mongo)
mongo.get.database.collections(mongo, "he_search_graph")



###############################################################################
## Walk over the raw pages,  parse, save back to mongo for later
###############################################################################

# lets define some constants to save on typing later
# DB = "he_search_graph"
# COLL = "rawpages"
# DBNS <- paste(DB, COLL, sep = ".")

## select 1 record -- lets see what we have
mongo.count(mongo, "he_search_graph.rawpages")
tmp <- mongo.find.one(mongo, "he_search_graph.rawpages")
class(tmp) # what do we have?
tmp <- mongo.bson.to.list(tmp) # convert to a list
class(tmp)
names(tmp) # what metadata do we have
tmp[length(tmp)] # show a value


## create a cursor that represents a SELECT * in SQL terms
cursor = mongo.find(mongo, DBNS)

## wipe out the records from the parsedpages
mongo.count(mongo, "he_search_graph.parsedpages")
# mongo.remove(mongo, "he_search_graph.parsedpages")
mongo.count(mongo, "he_search_graph.parsedpages")

## iterate with the cursor to get every collection
while (mongo.cursor.next(cursor)) {
 
 #iterate and grab the next record
 tmp = mongo.bson.to.list(mongo.cursor.value(cursor))
 
 # extract the school name
 college = tmp$college
 inst = gsub("-", " ", str_sub(college, 11, nchar(college)-1))
 
 # grab  page from the document and parse
 page = tmp$page
 doc = htmlParse(page)
 
 # get the unitid
 unitid <- xpathSApply(doc, '//*[@id="collegeYes"]/button/@data-url')
 names(unitid) <- NULL
 unitid <- str_extract(unitid, pattern="[0-9]{1,6}")
 
 # get the school's rating (out of 5 stars)
 XPATH <- '/html/body/div[1]/div[3]/div/div/div[1]/div/div[1]/div/a/span/meta[1]/@content'
 rating <- xpathSApply(doc, XPATH)
 names(rating) <- NULL
 rating <- as.numeric(rating)
 
 # get the list of the first 10 schools - loop through the xpath indices
 comps <- data.frame(stringsAsFactors=F)
 for (i in 1:10) {
  XPATH <- paste('//*[@id="siteContentWrapper"]/div/div/div[3]/div[3]/div[3]/div/div/ul[1]/li[' , i,
                 ']/a/@href', sep="")
  tmp <- xpathSApply(doc, XPATH)
  names(tmp) <- NULL
  tmp <- ifelse(is.null(tmp), NA, tmp)
  tmp2 <- data.frame(unitid=unitid, comp=tmp)
  comps <- rbind.fill(comps, tmp2)
 }
 
 # get the other schools listed
 for (i in 1:15) {
  XPATH <- paste('//*[@id="hiddenSimilarCollegeList"]/li[', i, ']/a/@href', sep="")
  tmp <- xpathSApply(doc, XPATH)
  names(tmp) <- NULL
  tmp <- ifelse(is.null(tmp), NA, tmp)
  tmp2 <- data.frame(unitid=unitid, comp=tmp)
  comps <- rbind.fill(comps, tmp2)
 }
 
 # assign the rank to the school based on the order listed on the site
 # this is the edge weight for our network
 rank <- data.frame(rank=1:nrow(comps), stringsAsFactors=F)
 comps <- cbind(rank, comps)
 comps$unitid = as.character(comps$unitid)
 comps$comp = as.character(comps$comp)
 
 # put the parsed data into a list
 tmplist = list(path=college, 
                inst=inst, 
                unitid=unitid, 
                rating=rating, 
                comps=comps)
 
 # convert the list into BSON so we can send it back to MongoDB
 # notice that we are sending a list that includes vectors and a df
 tmp.bson <- mongo.bson.from.list(tmplist)
 
 # write the data to the parsedpages collection
 mongo.insert(mongo, "he_search_graph.parsedpages", tmp.bson)
 
 # status
 cat("finished ", college, "\n")
}


###############################################################################
## Query all of the parsed pages, and put into dataframes
###############################################################################

## lets see what we got - probe one record
# tmp <- mongo.find.one(mongo, "he_search_graph.parsedpages")
# class(tmp) # what do we have?
# tmp <- mongo.bson.to.list(tmp) # convert to a list
# class(tmp)
# names(tmp) # what metadata do we have
# tmp[length(tmp)] # show a value
# tmp.df = tmp$comps
# class(tmp.df) # the rmongodb package returned our df as a list
# tmp.df = as.data.frame(tmp.df) # convert back to a df
# class(tmp.df) # verify
# str(tmp.df)

##  build the master data frames
edges = data.frame(stringsAsFactors=F)
schools = data.frame(stringsAsFactors=F)

## for each parsed page, populate the master datasets
cursor = mongo.find(mongo, "he_search_graph.parsedpages")

## iterate over the cursor
while (mongo.cursor.next(cursor)) {
 #iterate and grab the next record
 tmp = mongo.bson.to.list(mongo.cursor.value(cursor))
 # append the comps data to the edges dataframe
 e.tmp = tmp$comps
 e.tmp = as.data.frame(e.tmp)
 edges = rbind.fill(edges, e.tmp)
 # append the school info
 s.tmp = data.frame(unitid = tmp$unitid,
                    name = tmp$inst,
                    rating = tmp$rating,
                    path = tmp$path,
                    stringsAsFactors=FALSE)
 schools = rbind.fill(schools, s.tmp)
 # status
 cat("finished ", tmp$path, "\n")
}

## cleanup
rm(cursor, e.tmp, s.tmp, tmp, mongo)

## tidy up the edges dataset
colnames(edges) = c("rank", "from", "comp")
s.tmp = subset(schools, select=c("unitid", "path"))
colnames(s.tmp) = c("to", "path")
edges = merge(edges, s.tmp, by.x="comp", by.y="path", all.x=T)
edges$comp = NULL
rm(s.tmp)

## save the edges and school info as parsed datasets into mongodb
tmplist = list(edges = edges, 
               schools = schools)
tmp.bson <- mongo.bson.from.list(tmplist)
mongo.remove(mongo, "he_search_graph.datasets")
mongo.insert(mongo, "he_search_graph.datasets", tmp.bson)

## disconnect
mongo.disconnect(mongo)


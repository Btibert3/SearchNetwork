###############################################################################
## Build off of the data we scraped using Python and finalize the datasets
## for analysis
###############################################################################

# set the directory
setwd("~/Dropbox/GitHub/HigherEd/SearchNetwork")

## load the packages
library(rmongodb)
library(XML)
library(plyr)
library(stringr)


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


## disconnect
mongo.disconnect(mongo)



###############################################################################
## Crawl the IPEDS Surveys to get the files we want AND VERSIONS we want
## BE VERY CAREFUL FOR REVISION DATASETS - DONT ALWAYS USE ORIGINAL FILE
###############################################################################

# use 2006 - 2010, as 2010 is the most recent data avail in IPEDS 
BASE_URL <- 'http://nces.ed.gov/ipeds/datacenter/data/'

## Create the file names for the variables we want
hd = paste0('HD', 2006:2010)
ic = paste0('IC', 2006:2010)
sfa = c('SFA0607','SFA0708','SFA0809','SFA0910','SFA1011')

# specify the tmp dir
tmpDIR = "./data/tmp"

##==============================================================
## get the datasets
##==============================================================

## crawl the raw zip files - HD
for (survey in hd) {
 # build the url
 URL <- paste(BASE_URL, survey, ".zip", sep="")
 # build the destination -- needs to include path AND file
 dest <- paste("data/hd/", survey, ".zip", sep="")
 # fetch the file
 download.file(URL, destfile=dest, mode="wb", cacheOK=FALSE)
}

## crawl the raw zip files - IC
for (survey in ic) {
 # build the url
 URL <- paste(BASE_URL, survey, ".zip", sep="")
 # build the destination -- needs to include path AND file
 dest <- paste("data/ic/", survey, ".zip", sep="")
 # fetch the file
 download.file(URL, destfile=dest, mode="wb", cacheOK=FALSE)
}

## crawl the raw zip files - SFA
for (survey in sfa) {
 # build the url
 URL <- paste(BASE_URL, survey, ".zip", sep="")
 # build the destination -- needs to include path AND file
 dest <- paste("data/sfa/", survey, ".zip", sep="")
 # fetch the file
 download.file(URL, destfile=dest, mode="wb", cacheOK=FALSE)
}


##==============================================================
## helper functions 
##==============================================================

## clean out the tmp folder
cleanTmp = function(DIR="data/tmp") {
 FILES = list.files(DIR, full.names=TRUE)
 for (FILE in FILES) {
  file.remove(FILE)
 }
}

## pick the file, use the revised (rv) version if present
pickFile = function(DIR="data/tmp") {
 FILES = list.files(DIR, full.names=TRUE)
 if (length(FILES) == 1) {
  fpath = FILES
 }
 if (length(FILES) != 1) {
  fpath = FILES[str_detect(FILES, "_rv.csv")]
 }
 return(fpath)
}


##==============================================================
## parse the HD datasets
##==============================================================

# Files
FILES = list.files("data/hd/", pattern=".zip", full.names=TRUE)

## master hd dataframe
hd = data.frame(stringsAsFactors=F)

## go through the files
for (FILE in FILES ) {
 # status
 cat("processing ", FILE, "\n")
 # unzip the file to the tmp folder
 unzip(FILE, exdir=tmpDIR)
 # determine the path
 fpath = pickFile()
 tmp = read.csv(fpath, header=TRUE, stringsAsFactors=FALSE)
 colnames(tmp) = tolower(colnames(tmp))
 year = str_extract(fpath, "20[0-9]{2}")
 tmp$year = year
 hd = rbind.fill(hd, tmp)
 # clean tmp folder
 cleanTmp()
}


##==============================================================
## parse the IC datasets
##==============================================================

# Files
FILES = list.files("data/ic/", pattern=".zip", full.names=TRUE)

## master hd dataframe
ic = data.frame(stringsAsFactors=F)

## go through the files
for (FILE in FILES ) {
 # status
 cat("processing ", FILE, "\n")
 # unzip the file to the tmp folder
 unzip(FILE, exdir=tmpDIR)
 # determine the path
 fpath = pickFile()
 tmp = read.csv(fpath, header=TRUE, stringsAsFactors=FALSE)
 colnames(tmp) = tolower(colnames(tmp))
 year = str_extract(fpath, "20[0-9]{2}")
 tmp$year = year
 ic = rbind.fill(ic, tmp)
 # clean tmp folder
 cleanTmp()
}



##==============================================================
## parse the sfa datasets
##==============================================================

# Files
FILES = list.files("data/sfa/", pattern=".zip", full.names=TRUE)

## master hd dataframe
sfa = data.frame(stringsAsFactors=F)

## go through the files
for (FILE in FILES ) {
 # status
 cat("processing ", FILE, "\n")
 # unzip the file to the tmp folder
 unzip(FILE, exdir=tmpDIR)
 # determine the path
 fpath = pickFile()
 tmp = read.csv(fpath, header=TRUE, stringsAsFactors=FALSE)
 colnames(tmp) = tolower(colnames(tmp))
 year = str_extract(fpath, "[0-9]{4}")
 tmp$year = year
 sfa = rbind.fill(sfa, tmp)
 # clean tmp folder
 cleanTmp()
}

## change the years to match the other
sfa$year[sfa$year=='0607'] = '2006'
sfa$year[sfa$year=='0708'] = '2007'
sfa$year[sfa$year=='0809'] = '2008'
sfa$year[sfa$year=='0910'] = '2009'
sfa$year[sfa$year=='1011'] = '2010'


## save out the datasets
saveRDS(sfa, file="data/sfa.rds")
saveRDS(hd, file="data/hd.rds")
saveRDS(ic, file="data/ic.rds")


###############################################################################
## Put together the IPEDS data 
###############################################################################














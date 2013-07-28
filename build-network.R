###############################################################################
## Build the network data from a popular scollege search site
## Author: Brock Tibert
##         @BrockTibert
## Date: May 2013
###############################################################################

## packages -- might need libxml2-dev
require(XML)
require(RCurl)
require(stringr)
require(plyr)
require(igraph)


###############################################################################
## Get the College Roster from the website
###############################################################################
GetRoster <- function() {
  # returns a character vector of URL suffixes
  
  # load the roster of colleges
  URL <- "http://www.cappex.com/colleges/"
  doc <- htmlParse(URL)
  links <- unlist(doc['//@href'])
  names(links) <- NULL
  pattern <- "/colleges/.*/$"
  m <- as.vector(str_match(links, pattern))
  school.links <- m[!is.na(m)]
  school.links <- school.links[3:length(school.links)]
  
  # return the data
  return(school.links) 
}

## use the function to get the roster of colleges on the site
roster <- GetRoster()
saveRDS(roster, file="data/roster.rds")


###############################################################################
## Get each schools data and parse the data into R dataframes
## A really sloppy way of doing it, but its easy to follow in a hacky way
## Use Google Chrome developer tools to get XPATH String
###############################################################################
ParseSchool <- function(F) {
  URL <- paste("http://www.cappex.com", F, sep="")
  # reparse the doc
  doc <- htmlParse(URL)
  # Cappex school name
  inst <- gsub("-", " ", str_sub(F, 11, nchar(F)-1))
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
    XPATH <- paste('/html/body/div[1]/div[3]/div/div/div[3]/div[3]/div[4]/div/div/ul[1]/li[', i,
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
  rank <- data.frame(rank=1:nrow(comps))
  comps <- cbind(comps, rank)
  # print a message
  cat("finished: ", inst, "\n")
  # return the parsed data as a named list
  return(list(list(path=F, inst=inst, unitid=unitid, rating=rating, comps=comps)))
}

# scrape the data into a list of lists
# takes about a half hour to an hour to walk the site
data = sapply(roster, ParseSchool)
saveRDS(data, file="data/raw-data.rds")


###############################################################################
## put together a crosswalk between IPEDS UNITIDS and the school path
## should error check this -- hacky solution -- for loop is easy to read
###############################################################################
xwalk <- data.frame(stringsAsFactors=F)
for (S in 1:length(data)) { 
  L = data[[S]]
  unitid <- L$unitid
  rating <- L$rating
  path <- L$path
  tmp.df <- data.frame(unitid, path, rating, stringsAsFactors=F)
  xwalk <- rbind.fill(xwalk, tmp.df)
}
xwalk = unique(xwalk)
saveRDS(xwalk, file="data/xwalk.rds")


###############################################################################
## put together the data as an edgelist
## TODO:  speed up this process -- hacky but intuitive to read
###############################################################################
edges <- data.frame(stringsAsFactors=F)
for (S in 1:length(data)) { 
  L = data[[S]]
  tmp.df <- L$comps
  edges <- rbind(edges, tmp.df)
  cat("completed: ", S, "\n")
}
edges = unique(edges)
saveRDS(edges, file="data/edges.rds")



###############################################################################
## clean up the edgelist data for igraph
###############################################################################

## merge the xwalk and edges data
edgelist <- merge(edges, xwalk,
                 by.x = "comp",
                 by.y = "path",
                 all.x = TRUE)

## we merged extra data -- remove and only keep unitids
edgelist <- subset(edgelist, select=c(unitid.x, unitid.y))
names(edgelist) <- c("from", "to")

## ensure that all nodes are valid
edgelist <- subset(edgelist, !is.na(from) & !is.na(to))


###############################################################################
## build the graph in igraph - save out as various data formats
###############################################################################
g <- graph.data.frame(edgelist, directed=TRUE, vertices=xwalk)
saveRDS(g, file="data/network.rds")
write.graph(g, file="data/network.graphml", format="graphml")
write.graph(g, file="data/network.gml", format="gml")
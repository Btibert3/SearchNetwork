###############################################################################
## Combine the parsed datasets and analyze the data
###############################################################################

## load the libraries
library(rmongodb)
library(igraph)
library(plyr)
library(reshape2)

## connect to our mongodb instance on the network
mongo <- mongo.create(host="192.168.1.69")
mongo.get.databases(mongo)
mongo.get.database.collections(mongo, "he_search_graph")


###############################################################################
## Bring in the datasets to R
## We know there is only 1 record, so we just use find.one
## NOTE: conversion to dataframe includes arrarys, so we convert to vectors
###############################################################################

# bring down the data and convert to a list
tmp = mongo.find.one(mongo, "he_search_graph.datasets")
tmp = mongo.bson.to.list(tmp)

# append the comps data to the edges dataframe
edges = tmp$edges
edges = as.data.frame(edges)
edges = data.frame(lapply(edges, as.vector), stringsAsFactors=F)

# schools
schools =  tmp$schools
schools = as.data.frame(schools)
schools = data.frame(lapply(schools, as.vector), stringsAsFactors=F)

## cleanup
mongo.disconnect(mongo)
rm(tmp, mongo)


###############################################################################
## Load the huge Delta Cost Project dataset and reshape the data to
## give us the metrics we want, one row per school
## access the data here: http://nces.ed.gov/ipeds/deltacostproject/
###############################################################################

## load the data from my machine -- already unzipped
## use ?download.file and ?unzip if you dont already have it
ddir = '~/dropbox/datasets/highered/delta-cost/'
fname = paste0(ddir, "delta_public_00_10.csv")
delta = read.csv(fname, header=TRUE, stringsAsFactors=FALSE)
colnames(delta) = tolower(colnames(delta))
rm(ddir, fname)

## this is a huge dataset
dim(delta)

## filter the schools - keep those we grabbed earlier
## use 2010 to find the schools
## we will filter the network dataset to keep only these schools
delta10  = subset(delta, academicyear==2010)
delta10 = subset(delta10, unitid %in% schools$unitid)

## keep only the schools demographics
cols = c('unitid', 'instname', 'zip', 'state', 'oberegion',
         'sector','carnegiegrp_2000')
demos = subset(delta10, select = cols)
rm(delta10, cols)

## keep the data columns we want - grab more than we may need just in case
cols = c('unitid', 'academicyear', 'fte_count', 
         'grad_rate_150_p4yr', 'ftretention_rate', 'fall_cohort_pct_instate', 
         'applcn', 'admssn', 'enrlt', 'actpct', 'actcm25', 'actcm75', 
         'satpct', 'satmt25', 'satmt75', 'satvr25', 'satvr75')

## filter our data
delta.f = subset(delta, 
                 subset = unitid %in% schools$unitid,
                 select = cols)
rm(cols)


###############################################################################
## reshape the data to give us the attributes we want (by year)
###############################################################################

## calculate some common metrics
delta.f = mutate(delta.f,
                 admitpct = admssn/applcn,
                 yieldpct = enrlt/admssn)

## define the columns we want to pivot
cols = names(delta.f)[3:ncol(delta.f)]

## reshape the data, 1 row per school, every year/value var in the column
for (i in cols) {
  tmp = dcast(delta.f, unitid ~ academicyear, value.var=i)
  names(tmp)[2:ncol(tmp)] = paste0(i, "_", names(tmp[2:ncol(tmp)]))
  demos = merge(demos, tmp, by.x = "unitid", by.y = "unitid", all.x=T)
}
rm(i, cols, tmp)


###############################################################################
## build the dataset that we will use in modeling
###############################################################################


## calculate some of the key trend stats that we will use in the model
## using last years values and changes as the key predictors for what
## will happen THIS year
df = mutate(demos,
            appchange = (applcn_2010 - applcn_2009) / applcn_2009,
            appdelta_lag1 = (applcn_2009 - applcn_2008) / applcn_2008,
            instate = fall_cohort_pct_instate_2009 / 100,
            gradrate = grad_rate_150_p4yr_2009,
            retnrate = ftretention_rate_2009,
            admitrate = admitpct_2009,
            admit_delta = admitpct_2009 - admitpct_2008,
            yieldrate = yieldpct_2009,
            yield_delta = yieldpct_2009 - yieldpct_2008)


## keep only the columns we want for our modeling
## note, we may want to keep things like region, state, etc...
df = subset(df, 
            select = c('unitid', 'appchange', 'appdelta_lag1',
                       'instate', 'gradrate', 'retnrate', 
                       'admitrate', 'admit_delta', 'yieldrate',
                       'yield_delta'))


###############################################################################
## playing around - think about filtering out isolates, and ensure
## no missing unitids in the TO column for edges
###############################################################################

### playing around
tmp = complete.cases(df)
df2 = df[tmp,]
edges.f = subset(edges, from %in% df2$unitid)
edges.f = subset(edges.f, to %in% df2$unitid)
edges.f = subset(edges.f, select = c('from', 'to', 'rank'))
schools.f = subset(schools, unitid %in% df2$unitid)
schools.f = merge(schools.f, df2)
g = graph.data.frame(edges.f, directed=TRUE, vertices=schools.f)
l <- layout.fruchterman.reingold(g)
plot(g, layout=l, vertex.label = schools.f$unitid)
## only keep schools with 3 connections (filter on 3 for both in and out degree?)
##?igraph.plotting for help on the attributes for E(g) and V(g)
degin = degree(g, mode = "in")
degout = degree(g, mode = "out")
## has to be an easier way than this, right?
g2 = induced.subgraph(g, V(g)[degree(g, mode = "in") >= 5])
g3 = induced.subgraph(g2, V(g2)[degree(g2, mode = "out") >= 5])
filterd_schools = get.data.frame(g3, what="vertices")
## we lose the unitid, so use cbind(uniitd, schools.f to put in twice)
## this is redundant, but helps auto set unitid as graph label and filter 
## the data later

## do I just want to use app data -- maximizes schools and network
## doesnt account for anything else
# R = with(demos, 
#          which(applcn_2010 > 0 & applcn_2009 > 0 & applcn_2008 > 0))


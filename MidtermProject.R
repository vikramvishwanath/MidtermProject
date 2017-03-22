require(foreign)
require(plyr)
require(dplyr)
require(tidyr)
require(magrittr)
require(ggplot2)
require(ggthemes)
require(knitr)

# Part 1: Cleaning up accid.DBF
# This file tells us about accidents and related info
# Columns we can keep are:
# ACTIVITYNO, DEGREE, NATURE, BODYPART, EVENT, ENVIRON, HUMAN
# TASK, HAZSUB

# read file
accid <- read.dbf("accid.dbf")
acc <- read.dbf("acc.dbf") # code reference table

head(accid)

sum(accid$SITESTATE == 'MA') == dim(accid)[1] #TRUE

# all in MA so drop 2nd column
# columns to keep are 1, 6, 7, 8, 9, 10, 11, 12, 13, 14
accid <- accid[, c(1, 6:14)]
head(accid)

# there seem to be some duplicate values
# remove them
accid <- unique(accid)
head(accid)

# make sure there are no NA's; count each column
countNA_accid <- NULL
for(i in 1:10){
  no_NA <- sum(is.na(accid[, i]))
  countNA_accid <- c(countNA_accid, no_NA)
}
countNA_accid

# only the last column has NA's
# split into two tables one with HAZSUBs and one with non

accid_hazsub <- filter(accid, is.na(HAZSUB)==FALSE)
accid <- filter(accid, is.na(HAZSUB)==TRUE)

# count NA's in last column of accid; they all should be
sum(is.na(accid$HAZSUB))==dim(accid)[1]

# all NA's--> drop it
accid <- accid[, -10]

# arrange by TASK, then DEGREE
# TASK=1 is regular task

accid <- accid[order(accid[, 9], accid[, 2]), ]
accid_hazsub <- accid_hazsub[order(accid_hazsub[, 9],
                                   accid_hazsub[, 2]),]

head(accid)
head(accid_hazsub)

# accid has values equal to zero
# no zero values in README or code reference table#
# probably uninjured
# extract them to their own table and then delete from original

zero_rows <- which(accid$DEGREE==0)
zero_values <- accid[zero_rows, ]
accid <- accid[-zero_rows, ]

head(zero_values)
head(accid)

# replace hazard sub codes with names
# values are in hzs.dbf
haz <- read.dbf("hzs.DBF")

# haz has colname = 'CODE'
# rename HAZSUB col in accid_hazsub to 'CODE' to join

colnames(accid_hazsub)[10] <- 'CODE'

# inner_join accid HAZSUB with name with code
accid_hazsub <- left_join(accid_hazsub, haz, by='CODE')

#rename cols and remove CODE
accid_hazsub$CODE <- accid_hazsub$TEXT
accid_hazsub <- accid_hazsub[, -11]
colnames(accid_hazsub)[10] <- 'HAZSUB'
head(accid_hazsub)
# much better

# not all HAZSUB codes have HAZSUBS, move them to the bottom
toprows <- filter(accid_hazsub, is.na(HAZSUB)==FALSE)
bottomrows <- filter(accid_hazsub, is.na(HAZSUB)==TRUE)
accid_hazsub <- rbind(toprows, bottomrows)
  
head(accid_hazsub)

# save file
save(file='Accidents.SAV', list='accid')
save(file='AccidentsHazSub.SAV', list='accid_hazsub')
rm(list=ls())

#######################################################################
# Part 2: Cleaning up viol.DBF

viol <- read.dbf("viol.DBF")
# How dangerous a place is depends on its number of violations
# and their seriousness
# From looking at the README for viol.DBF, we can keep these columns:
# ACTIVITYNO, ISSUEDATE, ITEMNO, GRAVITY, VIOLTYPE, INSTANCES

# From README, if DELETE==X, then violation was deleted for some
# other reason; we can remove these columns from the original viol
# before working with it

viol <- filter(viol, is.na(DELETE)==TRUE)
# check NA's then delete if all NA's
sum(is.na(viol$DELETE)) == dim(viol)[1] # TRUE
viol <- viol[, -3]

# now only keep columns we care about
viol %<>% select(ACTIVITYNO, ISSUEDATE, ITEMNO, GRAVITY,
                 VIOLTYPE, INSTANCES)
head(viol)

# some in GRAVITY have NA's
# let's see what the rows are

a <- head(filter(viol, is.na(GRAVITY)==FALSE))
# what VIOLTYPES have GRAVITIES
levels(a$VIOLTYPE)
# GRAVITY can exist for all levels but doesn't
# rearrange them so that those rows are at the bottom

toprows <- filter(viol, is.na(GRAVITY)==FALSE)
bottomrows <- filter(viol, is.na(GRAVITY)==TRUE)
viol <- rbind(toprows, bottomrows)

head(viol)

# there don't seem to be any duplicates but just remove them 
# just to make sure

viol <- unique(viol)

head(viol)

# save file
save(file='Viol.SAV', list='viol')
rm(list=ls())

############################################################
# Part 3: Cleaning up osha.DBF

osha <- read.dbf("osha.DBF")
# from osha.DBF (hereafter referred as osha), we don't need
# PREVACTNO because ACTIVITYNO is already a unique identifier
# we should keep:
# ACTIVITYNO, ESTABNAME, SITEADD, SITESTATE, SITEZIP, SIC,
# ACCID_, VIOLS_, HAZSUB_
osha %<>% select(ACTIVITYNO, ESTABNAME, SITEADD, SITESTATE, 
                 SITEZIP, SIC, ACCID_, VIOLS_, HAZSUB_)
head(osha)

# check to see is all in MA
sum(osha$SITESTATE=='MA') == dim(osha)[1] # TRUE, drop it
osha <- osha[, -4]

head(osha)

# zipcode of 00000 does not exist, might not even need that column
osha <- osha[, -4]

head(osha)

# Last three cols have zeros, meaning that some don't appear in those
# tables. If a place has no accidents, or violations, or hazardous
# substances, then we can safely assume that place is entirely safe
# remove those rows; they don't appear in the tables anyways

rows <- which(osha$ACCID_==0 & osha$VIOLS_==0 & osha$HAZSUB_==0)
osha <- osha[-rows, ]

head(osha)

# SIC column
# this columns has codes that correspond to industry type; this might
# be of benefit to a data analyst. Replace the codes
# these codes are found in sic.DBF
sic <- read.dbf("sic.DBF")
head(sic)

# inner_join sic codes in osha with industry in sic.DBF
osha <- left_join(osha, sic, by='SIC')

# could have used inner join, but want all rows
# number of rows didnt change so we didnt get any NA's
# could have used inner_join but better this way

# replace code column with industry column, reorganize a little
osha$SIC <- osha$INDUSTRY
osha <- osha[, -8]
head(osha)

# rename columns
colnames(osha) <- c('ACTIVITYNO', 'ESTABNAME', 'ADDRESS', 
                    'INDUSTRY', 'ACCID','VIOL','HAZSUB')

head(osha)


# save files
save(file='Osha.SAV', list='osha')
rm(list=ls())

########################################################
# Part 4: Joining to make comprehensive tables

# load files
load("Accidents.SAV")
load("AccidentsHazSub.SAV")
load("Viol.SAV")
load("Osha.SAV")

# add sic and site names to accidents DBF and hazard subs accid DBF
# one thing osha and these accident files have in column is ACTIVITYNO
# extract first 4 cols from osha
# these contain ACTIVITYNO, ESTABNAME, ADDRESS, INDUSTRY
# left_join to accid and accid_hazsub

columns <- osha %>% select(ACTIVITYNO, ESTABNAME, ADDRESS, INDUSTRY)

accid <- left_join(accid, columns, by='ACTIVITYNO')
accid_hazsub <- left_join(accid_hazsub, columns, by='ACTIVITYNO')

# reorder columns to make it more meaningful
accid <- accid[, c(10:12, 1:9)]
accid_hazsub <- accid_hazsub[, c(11:13, 1:10)]

head(accid)
head(accid_hazsub)

# add sic and site names and things to viol.DBF
# then reorder

viol <- left_join(viol, columns, by='ACTIVITYNO')
viol <- viol[, c(7:9, 1:6)]

head(viol)

# rewrite files
save(file='Accidents.SAV', list='accid')
save(file='AccidentsHazSub.SAV', list='accid_hazsub')
save(file='Viol.SAV', list='viol')
save(file='Osha.SAV', list='osha')


rm(list=ls())

##############################################################
# Part 5: Replacing the codes with meaningful strings

load("Accidents.SAV")
load("AccidentsHazSub.SAV")
load("Viol.SAV")
load("Osha.SAV")
acc <- read.dbf("acc.DBF")

colnames(accid) <- c("ESTABNAME",'ADDRESS','INDUSTRY','ACTIVITYNO',
                     'DEGREE','NATURECODE','BODYPARTCODE','SOURCECODE',
                     'EVENTCODE','ENVIRONCODE','HUMANCODE','TASK')

# filter out each code category into separate table, then rename to common
# column name ".....CODE"

bodypart <- filter(acc, CATEGORY == 'PART-BODY')
colnames(bodypart) <- c("CATEGORY", "BODYPARTCODE", "BODYPART")

nature <- filter(acc, CATEGORY == 'NATUR-INJ')
colnames(nature) <- c("CATEGORY", "NATURECODE", "NATUREOFINJ")

source <- filter(acc, CATEGORY == 'SOURC-INJ')
colnames(source) <- c("CATEGORY", "SOURCECODE",'SOURCEOFINJ')

event <- filter(acc, CATEGORY == 'EVENT-TYP')
colnames(event) <- c("CATEGORY", 'EVENTCODE','EVENT')

environ <- filter(acc, CATEGORY == 'ENVIR-FAC')
colnames(environ) <- c("CATEGORY",'ENVIRONCODE','ENVIRONMENTOFINJ')

human <- filter(acc, CATEGORY == 'HUMAN-FAC')
colnames(human) <- c("CATEGORY",'HUMANCODE','HUMANFACTOR')

# join accid to each table by code
accid <- left_join(accid, bodypart, by='BODYPARTCODE')
accid <- left_join(accid, nature, by='NATURECODE')
accid <- left_join(accid, source, by='SOURCECODE')
accid <- left_join(accid, event, by='EVENTCODE')
accid <- left_join(accid, environ, by='ENVIRONCODE')
accid <- left_join(accid, human, by='HUMANCODE')

# remove code columns
accid %<>% select(ESTABNAME, ADDRESS, INDUSTRY, ACTIVITYNO, DEGREE, NATUREOFINJ, 
                  SOURCEOFINJ, BODYPART, EVENT, HUMANFACTOR, ENVIRONMENTOFINJ, TASK)

# replace DEGREE with names

accid$DEGREE <- gsub(pattern = 1, replacement = 'FATALITY', x = accid$DEGREE)
accid$DEGREE <- gsub(pattern = 2, replacement = 'HOSP', x = accid$DEGREE)
accid$DEGREE <- gsub(pattern = 3, replacement = 'NONHOSP', x = accid$DEGREE)


# repeat all of the above for accid_hazsub
colnames(accid_hazsub) <- c("ESTABNAME",'ADDRESS','INDUSTRY','ACTIVITYNO',
                     'DEGREE','NATURECODE','BODYPARTCODE','SOURCECODE',
                     'EVENTCODE','ENVIRONCODE','HUMANCODE','TASK','HAZSUB')

accid_hazsub <- left_join(accid_hazsub, bodypart, by='BODYPARTCODE')
accid_hazsub <- left_join(accid_hazsub, nature, by='NATURECODE')
accid_hazsub <- left_join(accid_hazsub, source, by='SOURCECODE')
accid_hazsub <- left_join(accid_hazsub, event, by='EVENTCODE')
accid_hazsub <- left_join(accid_hazsub, environ, by='ENVIRONCODE')
accid_hazsub <- left_join(accid_hazsub, human, by='HUMANCODE')

accid_hazsub %<>% select(ESTABNAME, ADDRESS, INDUSTRY, ACTIVITYNO, DEGREE, NATUREOFINJ, 
                  SOURCEOFINJ, BODYPART, EVENT, HUMANFACTOR, ENVIRONMENTOFINJ, TASK,
                  HAZSUB)

accid_hazsub$DEGREE <- gsub(pattern = 1, replacement = 'FATALITY', x = accid_hazsub$DEGREE)
accid_hazsub$DEGREE <- gsub(pattern = 2, replacement = 'HOSP', x = accid_hazsub$DEGREE)
accid_hazsub$DEGREE <- gsub(pattern = 3, replacement = 'NONHOSP', x = accid_hazsub$DEGREE)

save(file='Accidents.SAV', list='accid')
save(file='AccidentsHazSub.SAV', list='accid_hazsub')

# replace the codes in viol with their respective values
# from viol.TXT, 
# O = other
# R = repeat
# S = serious
# U = unclassified
# W = willful

# extract each category of violtype to separate tables
violO <- viol[which(viol$VIOLTYPE=='O'), ]
violR <- viol[which(viol$VIOLTYPE=='R'), ]
violS <- viol[which(viol$VIOLTYPE=='S'), ]
violU <- viol[which(viol$VIOLTYPE=='U'), ]
violW <- viol[which(viol$VIOLTYPE=='W'), ]

# replace single letter codes
violO$VIOLTYPE <- 'OTHER'
violR$VIOLTYPE <- 'REPEAT'
violS$VIOLTYPE <- 'SERIOUS'
violU$VIOLTYPE <- 'UNCLASSIFIED'
violW$VIOLTYPE <- 'WILLFUL'

# rowbind to get back modified viol
viol <- rbind(violS, violR, violW, violO, violU)

# define value type for all tables

viol[] <- lapply(viol, factor)
viol$INSTANCES %<>% as.integer()

accid[] <- lapply(accid, factor)
accid_hazsub[] <- lapply(accid_hazsub, factor)

osha[] <- lapply(osha, factor)
osha$ACCID %<>% as.integer()
osha$VIOL %<>% as.integer()
osha$HAZSUB %<>% as.integer()

save(file='Accidents.SAV', list='accid')
save(file='AccidentsHazSub.SAV', list='accid_hazsub')
save(file='Viol.SAV', list='viol')
save(file='Osha.SAV', list='osha')

rm(list=ls())

####################################################################
# SUMMARY

# Summary of what we have done so far
# We have simply taken bits and pieces from multiple DBFs
# and combined them into comprehensive tables with many values.
# None of these tables have NA's because those values were extracted
# to their own tables. The only assumption we have made was when
# cleaning up the osha file: we eliminated any rows where a certain
# ACTIVITYNO had zero accidents, zero violations, zero hazardous
# substances. All other assumptions were justified. If a data analyst
# was working with this data, he/she has four tables that are cleaned
# up and sorted by columns. He/she can proceed multiple ways in
# preparing the data. All of the codes hve been replaced by meaningful
# words. Though this makes the table difficult to work with, a data analyst
# can simply assign a numerical value to each factor and get back a table 
# with numbers.

####################################################################
# Part 6: Exploratory Analysis

load("Accidents.SAV")
load("AccidentsHazSub.SAV")
load("Viol.SAV")
load("Osha.SAV")

# Total degree counts
# for each factored column, count th frequenciesof the levels
# i.e. make a histogram

degree <- count(accid, DEGREE)
degree$DEGREE <- c('FATLITY','HOSPITALIZED','NOT HOSPITALIZED') 
degree$DEGREE %<>% factor()

nature <- count(accid, NATUREOFINJ)
source <- count(accid, SOURCEOFINJ)
bodypart <- count(accid, BODYPART)
event <- count(accid, EVENT)
human <- count(accid, HUMANFACTOR)
environ <- count(accid, ENVIRONMENTOFINJ)
industry <- count(accid, INDUSTRY)
hazsubs <- count(accid_hazsub, HAZSUB)

# plot

ggplot(degree) + aes(x=reorder(DEGREE, -n), y=n, fill=DEGREE) + geom_col() +xlab('') +
  ylab("INCIDENTS") + scale_fill_discrete(guide=FALSE) +
  ggtitle("OUTCOME OF INJURY") + theme_tufte() 
  
ggplot(nature) + aes(x=reorder(NATUREOFINJ, n), y=n) + geom_col() + coord_flip() +
  xlab("INJURY") +
  ylab("INCIDENTS") + ggtitle("NATURE OF INJURY") + theme_tufte() 

# There are over 30 sources on injury, too cluttered. Only plot top 15

source_top <-  source %>% arrange(desc(n))
source_top <- source_top[1:15,]
ggplot(source_top) + aes(x=reorder(SOURCEOFINJ, n), y=n) + geom_col() + 
  coord_flip() + xlab("SOURCE OF INJURY") +
  ylab("INCIDENTS") +
  ggtitle("TOP 15 SOURCES OF INJURY") + theme_tufte()

# There are 30 bodyparts, too cluttered, only plot top 15

bodypart_top <- bodypart %>% arrange(desc(n))
bodypart_top <- bodypart_top[1:15,]
ggplot(bodypart_top) + aes(x=reorder(BODYPART, n), y=n) + geom_col() +
  coord_flip() + xlab("BODYPART(S)") +
  ylab("INCIDENTS") + ggtitle('TOP 15 BODYPARTS AFFECTED') + theme_tufte()

ggplot(event) + aes(x=reorder(EVENT, n), y=n) + geom_col() + xlab("EVENT") +
  ylab("INCIDENTS") + coord_flip() + ggtitle("EVENTS CAUSING INJURY") + theme_tufte()

ggplot(human) + aes(x=reorder(HUMANFACTOR, n), y=n) + geom_col() + 
  coord_flip() + xlab("HUMAN ERROR ") +
  ylab("INCIDENTS") + ggtitle("HUMAN ERRORS") + theme_tufte()

ggplot(environ) + aes(x=reorder(ENVIRONMENTOFINJ, n), y=n) + geom_col() +
  coord_flip() + xlab("ENVIRONMENT") +
  ylab("INCIDENTS") + ggtitle("ENVIRONMENT OF INJURY") + theme_tufte()

# too many industries -> top 15
industry_top <-  industry %>% arrange(desc(n))
industry_top <- industry_top[1:15,]
ggplot(industry_top) + aes(x=reorder(INDUSTRY, n), y=n) + geom_col() +
  coord_flip() + xlab("INDUSTRY") +
  ylab("INCIDENTS") + ggtitle("TOP 15 INDUSTRIES \nWITH MOST ACCIDENTS") + theme_tufte()


# too many haz subs, --> top 15
hazsubs_top <- hazsubs %>% arrange(desc(n)) %>% filter(is.na(HAZSUB) == FALSE)
hazsubs_top <- hazsubs_top[1:15,]
ggplot(hazsubs_top) + aes(x=reorder(HAZSUB, n), y=n) + geom_col() +
  coord_flip() + xlab("HAZARDOUS SUBSTANCE") +
  ylab("INCIDENTS") + ggtitle("TOP 15 MOST COMMON \nHAZARDOUS SUBSTANCES") + theme_tufte()



# count violation type
types <- count(viol, VIOLTYPE)

ggplot(types) + aes(x=reorder(VIOLTYPE, n), y=n) + geom_col() +
  coord_flip() + xlab("TYPE") +
  ylab("VIOLATIONS") + ggtitle("TYPES OF VIOLATIONS") + theme_tufte()


# top 5 companies with most deaths
# filter accid for fatalities, then count, take top 5
places <- filter(accid, accid$DEGREE=='FATALITY') %>% count(ESTABNAME) %>% arrange(desc(n))
places <- places[1:5,]

# make table, kable() works in RMD file.
kable(places)

# top 5 companies with most deaths minus human error
# non human error reasons are
# 1) EQUIP. INAPPROPR FOR OPERATION
# 2) INSUF/LACK/PROTCV WRK CLTHG/EQUIP
# 3) INSUFF/LACK/ENGINEERNG CONTROLS
# 4) INSUFF/LACK/HOUSEKEEPING PROGRAM  
# 5) INSUFF/LACK/RESPIRATORY PROCTECT  
# 6) INSUFF/LACK/WRITN WRK PRAC PROG.
# 7) DEFECTIVE EQUIPMENT IN USE
# 8) MALFUNC IN SECURING/WARNING OP
# 9) MATER-HANDLG PROCED. INAPPROPR
# 10) SAFETY DEVICES REMOVED/INOPER.

errors <- c(1, 3, 4:8, 10, 12, 17)
errors <- as.vector(levels(accid$HUMANFACTOR))[errors]
fatalNH <- filter(accid, DEGREE == 'FATALITY' & HUMANFACTOR %in% errors)
fatalNH %<>% count(ESTABNAME) %>% as.data.frame()
fatalNH <- fatalNH[order(fatalNH[ ,2], decreasing=TRUE), ]
fatalNH <- fatalNH[1:5,]
kable(fatalNH)

# end of code
################################################



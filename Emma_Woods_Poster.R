# IST719
# Author: Emma Woods
# Purpose: Brexit Blues Project 

library(RColorBrewer)
library(reshape2)
library(dplyr)
library(ggplot2)
library(vioplot)
library(alluvial)

# for UK map
library(tidyverse)
library(rgeos)
library(rgdal)
library(maptools)

# text mining for wordcloud
library(tm)
library(SnowballC)
library(wordcloud)
library(readr)
library(e1071)
library(mlbench)

# set fully qualified path to data file
fname <- file.choose()
uk_census <- read.csv(file = fname
                         , header = TRUE
                         , stringsAsFactors = FALSE
)
str(uk_census)
summary(uk_census)

# open data file 2
fname2 <- file.choose()
uk_referendum <- read.csv(file = fname2
                      , header = TRUE
                      , stringsAsFactors = FALSE
)
str(uk_referendum)
summary(uk_referendum)

# open data file 3
fname3 <- file.choose()
uk_wellbeing <- read.csv(file = fname3
                          , header = TRUE
                          , stringsAsFactors = FALSE
)
str(uk_wellbeing)

# Issues to be fixed 
######################################
# need to merge dataframes 
######################################
# first change column name in census data from "Code" to "Area.Code"
# Update column name 3
# First put existing column names into a new variable
cnames <- colnames(uk_census)

# Now change the name of column 2 and store in the variable
cnames[2] <- "Area.Code"

# Now use 'colnames' to update the column names in the dataframe
colnames(uk_census) <- cnames

# Confirm update
colnames(uk_census)

# merge data frames by ID
brexit <- merge(uk_referendum,uk_census,by="Area.Code")

# run function that allows you to 'fix' data by displaying in an editable table
fix(brexit)    
colnames(brexit)

# further cleaning
# remove column "ID"
#brexit_clean <- brexit[, -2]

# look at overall Brexit voting percentages
percent_leave <- mean(brexit$Percent.Leave)
percent_leave

percent_remain <- mean(brexit$Percent.Remain)
percent_remain

uk_votes <- data.frame(percent_remain, percent_leave)
uk_votes
str(uk_votes)

# melt
uk_votes_long <- melt(uk_votes,  
                        measure.vars = c("percent_leave", "percent_remain"),
                        variable.name = "Votes", value.name = "Percentage")

str(uk_votes_long)
colnames(uk_votes_long)

# need to convert 'Percentage' from Factor to numeric
uk_votes_long$Percentage <- as.character(uk_votes_long$Percentage)
uk_votes_long$Percentage <- as.numeric(uk_votes_long$Percentage)
View(uk_votes_long$Percentage)

# need to convert 'Votes' from Factor to string
uk_votes_long$Votes <- as.character(uk_votes_long$Votes)
View(uk_votes_long$Votes)

bp <- ggplot(uk_votes_long, aes(x="", y=Percentage, fill=Votes))+
  geom_bar(width = 1, stat = "identity")
bp

pie <- bp + coord_polar("y", start=0)
pie

# Use custom color palettes
pie + scale_fill_manual(values=c("#CA0D11", "#111E5E"))

## back to cleaning....
 
# remove duplicate "Area.y" column
brexit_clean <- brexit[, -22]
fix(brexit_clean)

# now look at structure and summary
str(brexit_clean)
summary(brexit_clean)
colnames(brexit_clean)

# combine age columns
brexit_clean$ageto18 <- rowSums(brexit_clean[,23:26])
brexit_clean$age20to39 <- rowSums(brexit_clean[,27:30])   
brexit_clean$age40to59 <- rowSums(brexit_clean[,31:34]) 
brexit_clean$age60to79 <- rowSums(brexit_clean[,35:38]) 
brexit_clean$age80plus <- rowSums(brexit_clean[,39:41]) 
brexit_clean <- brexit_clean[, -c(23:41)]
str(brexit_clean)
colnames(brexit_clean)

#########################################################
# explore population distribution
options(scipen = 999)
plot(brexit_clean$All.Residents)
plot(sort(brexit_clean$All.Residents))

boxplot(brexit_clean$All.Residents)
hist(brexit_clean$All.Residents)

# density plot to show the shape of the population data
d <- density(brexit_clean$All.Residents)
plot(d)

# look at distribution of the total votes cast column
d2 <- density(brexit_clean$Votes.Cast)
plot(d2)
# add color to the density plot
polygon(d2, col = "orange")

boxplot(brexit_clean$Votes.Cast)
hist(brexit_clean$Votes.Cast)

# there seems to be an outlier in each column
# check the data
brexit_clean[which.max(brexit_clean$All.Residents),]
brexit_clean[which.max(brexit_clean$Votes.Cast),]

# Ah! Row 321 is the entire region of Northern Ireland
# as this is skewing the data, let's put that in its own df
NIreland_DF <- brexit_clean[brexit_clean$Region == "Northern Ireland", ]
View(NIreland_DF)

great_britain <- brexit_clean[-321,]

great_britain[which.max(great_britain$All.Residents),]
great_britain[which.max(great_britain$Votes.Cast),]

# let's re-run the plots to see the distribution now
# first turn off scientific notation
options(scipen = 999)

#Votes.Cast
par(mfrow = c(2,2))
boxplot(great_britain$Votes.Cast)
hist(great_britain$Votes.Cast)
d <- density(great_britain$Votes.Cast)
plot(d)
polygon(d, col = "red")

vioplot(great_britain$Votes.Cast)

# Population density
vioplot(great_britain$All.Residents, main = "Population distribution of Great Britain"
        , sub = "Single-dimension plot", ylab = "Population Density", col = "red", border = "blue")

vioplot(great_britain$Votes.Cast, main = "Distribution of EU Referendum Votes \nin Great Britain"
        , sub = "Single-dimension plot", ylab = "Total Number of Votes Cast", col = "blue", border = "red")

# look at values in Region column
unique((great_britain$Region))
unique((brexit_clean$Region))

# filter data set by rows containing Scotland in Region column only
scotland_DF <- great_britain[great_britain$Region == "Scotland", ]
View(scotland_DF)

wales_DF <- great_britain[great_britain$Region == "Wales", ]
View(wales_DF)

england_DF <- great_britain[great_britain$Region != "Wales", ]
england_DF <- england_DF[england_DF$Region != "Scotland", ]
View(england_DF)

# view Northern Ireland subset
View(NIreland_DF)

# compare distributions to see how they voted (Leave/Remain)
# using number of votes cast
# 'mar' setting sets margins around plots to improve visuals
par(mfrow = c(2,2), mar = c(2,3,1,2))

boxplot(scotland_DF$Remain)
boxplot(wales_DF$Remain)
s <- density(scotland_DF$Remain)
plot(s)
polygon(s, col = "blue")
w <- density(wales_DF$Remain)
plot(w)
polygon(w, col = "yellow")

# using percentage of votes cast
par(mfrow = c(2,2), mar = c(2,3,1,2))

# Scotland
boxplot(scotland_DF$Percent.Remain)
boxplot(scotland_DF$Percent.Leave)
r <- density(scotland_DF$Percent.Remain)
plot(r)
polygon(r, col = "blue")
l <- density(scotland_DF$Percent.Leave)
plot(l)
polygon(l, col = "yellow")

# Wales
par(mfrow = c(2,2), mar = c(2,3,1,2))

boxplot(wales_DF$Percent.Remain)
boxplot(wales_DF$Percent.Leave)
r <- density(wales_DF$Percent.Remain)
plot(r)
polygon(r, col = "blue")
l <- density(wales_DF$Percent.Leave)
plot(l)
polygon(l, col = "yellow")

# Distribution of voting preference in Great Britain
par(mfrow = c(1,2))

# overall distribution for GB (England, Scotland, Wales)
boxplot(great_britain$Percent.Remain, main = "Great Britain\nvotes to Remain"
        , sub = " Single-dimension plot", ylab = "Percentage of votes cast", col = "yellow")
boxplot(great_britain$Percent.Leave, main = "Great Britain\nvotes to Leave"
        , sub = " Single-dimension plot", ylab = "Percentage of votes cast", col = "red")

# Distribution for UK
par(mfrow = c(1,2))
uk_density <- density(brexit_clean$Percent.Remain)
plot(uk_density, main = "UK\nDistribution of Votes Cast", xlab = "% of vote")
polygon(uk_density, col = "blue")

uk_density2 <- density(england_DF$Percent.Leave)
lines(uk_density2,  main = "UK\nDistribution of Votes Cast", xlab = "% of vote")
polygon(uk_density2, col = "red")
legend("right", legend = c("% Remain", "% Leave"))


# Distribution for England only
par(mfrow = c(1,2))
e_density <- density(england_DF$Percent.Remain)
plot(e_density, main = "England: votes to Remain\nDistribution of Votes Cast", sub = "Single-dimension plot", xlab = "% of vote")
polygon(e_density, col = "yellow")

e_density2 <- density(england_DF$Percent.Leave)
lines(e_density2,  main = "England: votes to Leave\nDistribution of Votes Cast", sub = "Single-dimension plot", xlab = "% of vote")
polygon(e_density2, col = "dark green")

# Distribution of voting preference in Scotland and Wales
par(mfrow = c(2,2))
hist(scotland_DF$Percent.Remain, main = "Scotland: votes to Remain\nOverall % of Votes Cast"
     , xlab ="Single-dimension plot"
     , border = "yellow", col = "dark green"
     )
hist(scotland_DF$Percent.Leave, main = "Scotland: votes to Leave\nOverall % of Votes Cast"
     , xlab = "Single-dimension plot", border = "dark green", col = "yellow"
     )
hist(wales_DF$Percent.Remain, main = "Wales: votes to Remain\nOverall % of Votes Cast"
     , xlab = "Single-dimension plot", border = "yellow", col = "dark green"
    )
hist(wales_DF$Percent.Leave, main = "Wales: votes to Leave\nOverall % of Votes Cast"
     , xlab = "Single-dimension plot", border = "dark green", col = "yellow"
     )

#################################
# MULTI-DIMENSIONAL PLOTS
#################################

# use aggregation to create new df
# find the sum of votes cast by region
df.grouped <- aggregate(great_britain$Votes.Cast, list(great_britain$Region), sum)
df.grouped

uk.grouped <- aggregate(brexit_clean$Votes.Cast, list(brexit_clean$Region), sum)
uk.grouped

# find the average percent voting turnout by region
df.grouped2 <- aggregate(brexit_clean$Percent.Turnout, list(brexit_clean$Region), mean)
df.grouped2

uk.grouped2 <- aggregate(brexit_clean$Percent.Turnout, list(brexit_clean$Region), mean)
uk.grouped2

# update column names to be more meaningful
colnames(df.grouped) <- c("Region", "Votes")
colnames(df.grouped2) <- c("Region", "Votes")

colnames(uk.grouped) <- c("Region", "Votes")
colnames(uk.grouped2) <- c("Region", "Votes")

# now plot the aggregated df
# use column names as labels
# set plot space and margins

# create a custom palette
num.colors <- 11
# create function to generate custom palette with specified colors
FUN <- colorRampPalette(c("yellow", "red"))
# call the function on the number specified in num.colors
my.cols <- FUN(num.colors)

# multi-dimensional plot
par(mar=c(4,12,3,2))
#par(mfrow = c(1,), mar = c(3,4,2,1))
barplot(df.grouped$Votes, names.arg = df.grouped$Region, las=2
        , main = "Number of votes cast by Region\nMulti-dimension plot"
        , col = my.cols, border = "black"
        , horiz = T
        , font.axis=1 
        , col.axis="orange" 
        , cex.axis=0.5
)

# create a custom palette
num.colors <- 11
# create function to generate custom palette with specified colors
FUN <- colorRampPalette(c("blue", "red"))
# call the function on the number specified in num.colors
my.cols <- FUN(num.colors)

# multi-dimensional plot 2
par(mar=c(4,12,3,2))
#par(mfrow = c(1,), mar = c(3,4,2,1))
df <- uk.grouped2[order(uk.grouped2$Votes, decreasing = FALSE),]
barplot(df$Votes, names.arg = df$Region, las=2
        , main = "Voter Turnout by Region"
        , col = my.cols, border = "black"
        , xlab = "% Turnout"
        , horiz = T
        , font.axis=1 
        , col.axis="orange" 
        , cex.axis=0.5
        , xlim=c(0,80)
)

# what is population dist by region

df.grouped3 <- aggregate(great_britain$Votes.Cast, list(great_britain$Region), sum)
df.grouped3

############################################
# CHOROPLETH MAP: UK POPULATION DISTRIBUTION
# Based on UK Census data
# code source: https://rforjournalists.com/2019/10/20/how-to-make-a-uk-local-authority-choropleth-map-in-r/
############################################

# load shapefile
shape.dat.dir <- "/Users/emma/OneDrive - Syracuse University/Term5/IST719/Project/Project/"

shp <- readOGR(paste0(shape.dat.dir, "Local_Authority_Districts__December_2011__Boundaries-shp")
               , "Local_Authority_Districts__December_2011__Boundaries")

# convert shapefile to dataframe
shp <- fortify(shp, region = 'lad11cd')

# now merge with census data
shp <- merge(shp, uk_census, by.x = 'id', by.y = 'Code', all.x = TRUE)
shp <- arrange(shp, order)

# plot the map

p <- ggplot(data = shp, aes(x = long, y = lat, 
                            group = group, fill = All.Residents)) + 
  geom_polygon() + coord_equal() + theme_void() +
  ggtitle('UK Population Distribution')

p

##############################
#
# How did each region vote?
#
##############################

# create new variable
# compare Leave/Remain columns to see how each area voted
# dplyr
brexit_vote <- mutate(brexit_clean, remain.leave = ifelse(Percent.Leave > Percent.Remain, "Leave", "Remain"))

colnames(brexit_vote)

## ALLUVIAL PLOT

# use aggregate
alluv.df <- aggregate(brexit_vote$Votes.Cast
                      , list(brexit_vote$Region, brexit_vote$remain.leave)
                      , sum)
colnames(alluv.df) <- c("region", "vote", "votes.cast")

# edit region names to fit plot
alluv.df$region[alluv.df$region == "Yorkshire and The Humber"] <-"Yorkshire"
alluv.df$region[alluv.df$region == "Northern Ireland"] <-"N. Ireland"
View(alluv.df)

# create alluvial plot
alluvial(alluv.df[, 1:2], freq = alluv.df$votes.cast, border = "darkblue")

my.cols <- rep("blue", nrow(alluv.df))
my.cols[alluv.df$vote == "Leave"] <- "red"

alluvial(alluv.df[, 1:2], freq = alluv.df$votes.cast, col = my.cols, border = "black")


#########################################################
## OBSERVATIONS
## Most support within one region for Remain: Scotland
## Most support within one region for Leave: Wales
## Differences when comparing pop dist?
## How have their wellbeing scores suffered as a result?
#########################################################

# age groups for UK
# use original census DF
# before merging look at population distribution for UK
UK_DF <- uk_census[uk_census$Area == "United Kingdom", ]
View(UK_DF)

# update age columns
UK_DF$ageto18 <- rowSums(UK_DF[,5:8])
UK_DF$age20to39 <- rowSums(UK_DF[,9:12])   
UK_DF$age40to59 <- rowSums(UK_DF[,13:16]) 
UK_DF$age60to79 <- rowSums(UK_DF[,17:20]) 
UK_DF$age80plus <- rowSums(UK_DF[,21:23]) 
UK_DF <- UK_DF[, -c(5:23)]
str(UK_DF)
colnames(UK_DF)

# age groups within Scotland and Wales
# regroup with new 'Brexit vote' column
scotland_DF <- brexit_vote[brexit_vote$Region == "Scotland", ]
View(scotland_DF)
colnames(scotland_DF)

# note: there are NAs in the over 80's column for Scotland
scotlandDF_long <- scotland_DF[c(4, 22:26, 28)]
View(scotlandDF_long)
colnames(scotlandDF_long)

# melt
scotlandDF_long <- melt(scotlandDF_long, id.vars = c("Area.x", "All.Residents", "remain.leave"), 
                        measure.vars = c("ageto18", "age20to39", "age40to59", "age60to79"),
                        variable.name = "AgeGroup", value.name = "Count")

str(scotlandDF_long)

scotland_grouped <- aggregate(scotlandDF_long$Count, list(scotlandDF_long$AgeGroup, scotlandDF_long$remain.leave), sum)
scotland_grouped

s_dot <- ggplot(scotland_grouped, aes(x=Group.1, y=x))
s_dot + geom_linerange(
    aes(x=Group.1, ymin = 0, ymax = x, group=Group.2), 
    color = "lightgray", size = 1.5,
    position = position_dodge(0.3)
)+ 
    geom_point(
        aes(color = Group.2),
        position = position_dodge(0.3), size = 3
    )+
    scale_color_manual(values = c("#0073C2FF", "#EFC000FF"))+
    labs(title="Population Breakdown in Scotland", 
         subtitle="Age Distribution across Regions",
         x = "Age Group", 
         y = "Count") +
    guides(col=guide_legend("Referendum Votes")) +
    theme_minimal()

#####################
## Compare with Wales
#####################
wales_DF <- brexit_vote[brexit_vote$Region == "Wales", ]
View(wales_DF)
colnames(wales_DF)

walesDF_long <- wales_DF[c(4, 22:28)]
View(walesDF_long)
colnames(walesDF_long)

# melt
walesDF_long <- melt(walesDF_long, id.vars = c("Area.x", "All.Residents", "remain.leave"), 
                        measure.vars = c("ageto18", "age20to39", "age40to59", "age60to79", "age80plus"),
                        variable.name = "AgeGroup", value.name = "Count")

str(walesDF_long)

wales_grouped <- aggregate(walesDF_long$Count, list(walesDF_long$AgeGroup, walesDF_long$remain.leave), sum)
wales_grouped

w_dot <- ggplot(wales_grouped, aes(x=Group.1, y=x))
w_dot + geom_linerange(
        aes(x=Group.1, ymin = 0, ymax = x, group=Group.2), 
        color = "lightgray", size = 1.5,
        position = position_dodge(0.3)
        )+ 
    geom_point(
        aes(color = Group.2),
        position = position_dodge(0.3), size = 3
        )+
    scale_color_manual(values = c("#0073C2FF", "#EFC000FF"))+
    labs(title="Population Breakdown in Wales", 
         subtitle="Age Distribution across Regions",
         x = "Age Group", 
         y = "Count") +
    guides(col=guide_legend("Referendum Votes")) +
    theme_minimal()

#####################
## Now England
#####################
england_DF <- subset(brexit_vote, subset = Region %in% c("East", "East Midlands","London", "North East", "North West"
                                                                  , "South East", "South West", "West Midlands", 
                                                                  "Yorkshire and The Humber"))
colnames(england_DF)

englandDF_long <- england_DF[c(4, 22:28)]
View(englandDF_long)
colnames(englandDF_long)

# melt
englandDF_long <- melt(englandDF_long, id.vars = c("Area.x", "All.Residents", "remain.leave"), 
                     measure.vars = c("ageto18", "age20to39", "age40to59", "age60to79", "age80plus"),
                     variable.name = "AgeGroup", value.name = "Count")

str(englandDF_long)

england_grouped <- aggregate(englandDF_long$Count, list(englandDF_long$AgeGroup, englandDF_long$remain.leave), sum)
england_grouped

e_dot <- ggplot(england_grouped, aes(x=Group.1, y=x))
e_dot + geom_linerange(
  aes(x=Group.1, ymin = 0, ymax = x, group=Group.2), 
  color = "lightgray", size = 1.5,
  position = position_dodge(0.3)
)+ 
  geom_point(
    aes(color = Group.2),
    position = position_dodge(0.3), size = 3
  )+
  scale_color_manual(values = c("#0073C2FF", "#EFC000FF"))+
  labs(title="Population Breakdown in Wales", 
       subtitle="Age Distribution across Regions",
       x = "Age Group", 
       y = "Count") +
  guides(col=guide_legend("Referendum Votes")) +
  theme_minimal()


#####################
## Northern Ireland
#####################
# NIreland_DF

NIreland_DF <- brexit_vote[brexit_vote$Region == "Northern Ireland", ]
View(NIreland_DF)
colnames(NIreland_DF)

NIrelandDF_long <- NIreland_DF[c(4, 22:28)]
View(NIrelandDF_long)
colnames(NIrelandDF_long)

# melt
NIrelandDF_long <- melt(NIrelandDF_long, id.vars = c("Area.x", "All.Residents", "remain.leave"), 
                     measure.vars = c("ageto18", "age20to39", "age40to59", "age60to79", "age80plus"),
                     variable.name = "AgeGroup", value.name = "Count")

str(NIrelandDF_long)

NI_grouped <- aggregate(NIrelandDF_long$Count, list(NIrelandDF_long$AgeGroup, NIrelandDF_long$remain.leave), sum)
NI_grouped

ni_dot <- ggplot(NI_grouped, aes(x=Group.1, y=x))
ni_dot + geom_linerange(
  aes(x=Group.1, ymin = 0, ymax = x, group=Group.2), 
  color = "lightgray", size = 1.5,
  position = position_dodge(0.3)
)+ 
  geom_point(
    aes(color = Group.2),
    position = position_dodge(0.3), size = 3
  )+
  scale_color_manual(values = c("#0073C2FF", "#EFC000FF"))+
  labs(title="Population Breakdown in Northern Ireland", 
       subtitle="Age Distribution across Regions",
       x = "Age Group", 
       y = "Count") +
  guides(col=guide_legend("Referendum Votes")) +
  theme_minimal()

# just remainers
remain_DF <- brexit_vote[brexit_vote$remain.leave == "Remain", ]
View(remain_DF)
colnames(remain_DF)

# change NAs to zeros
remain_DF[is.na(remain_DF)] <- 0

remain_long <- remain_DF[c(4, 22:28)]
View(remain_long)
colnames(remain_long)


# melt
remain_long <- melt(remain_long, id.vars = c("Area.x", "All.Residents", "remain.leave"), 
                        measure.vars = c("ageto18", "age20to39", "age40to59", "age60to79", "age80plus"),
                        variable.name = "AgeGroup", value.name = "Count")

str(remain_long)

remain_grouped <- aggregate(remain_long$Count, list(remain_long$AgeGroup), sum)
remain_grouped

remain_dot <- ggplot(remain_grouped, aes(x=Group.1, y=x))
remain_dot + geom_linerange(
  aes(x=Group.1, ymin = 0, ymax = x), 
  color = "lightgray", size = 1.5,
  position = position_dodge(0.3)
)+ 
  geom_point(
    aes(color = Group.1),
    position = position_dodge(0.3), size = 3
  )+
  labs(title="Population Breakdown for Remainers", 
       subtitle="Age Distribution across Regions",
       x = "Age Group", 
       y = "Count") +
  guides(col=guide_legend("Referendum Votes")) +
  theme_minimal()

# just leavers
leave_DF <- brexit_vote[brexit_vote$remain.leave == "Leave", ]
View(leave_DF)
colnames(leave_DF)

# change NAs to zeros
leave_DF[is.na(leave_DF)] <- 0

leave_long <- leave_DF[c(4, 22:28)]
View(leave_long)
colnames(leave_long)


# melt
leave_long <- melt(leave_long, id.vars = c("Area.x", "All.Residents", "remain.leave"), 
                    measure.vars = c("ageto18", "age20to39", "age40to59", "age60to79", "age80plus"),
                    variable.name = "AgeGroup", value.name = "Count")

str(leave_long)

leave_grouped <- aggregate(leave_long$Count, list(leave_long$AgeGroup), sum)
leave_grouped

leave_dot <- ggplot(leave_grouped, aes(x=Group.1, y=x))
leave_dot + geom_linerange(
  aes(x=Group.1, ymin = 0, ymax = x), 
  color = "lightgray", size = 1.5,
  position = position_dodge(0.3)
)+ 
  geom_point(
    aes(color = Group.1),
    position = position_dodge(0.3), size = 3
  )+
  labs(title="Population Breakdown for Leavers", 
       subtitle="Age Distribution across Regions",
       x = "Age Group", 
       y = "Count") +
  guides(col=guide_legend("Referendum Votes")) +
  theme_minimal()

# together
VotePop_long <- brexit_vote[c(4, 22:28)]
View(VotePop_long)
colnames(VotePop_long)

VotePop_long[is.na(VotePop_long)] <- 0

# melt
VotePop_long <- melt(VotePop_long, id.vars = c("Area.x", "All.Residents", "remain.leave"), 
                        measure.vars = c("ageto18", "age20to39", "age40to59", "age60to79", "age80plus"),
                        variable.name = "AgeGroup", value.name = "Count")

str(VotePop_long)

VotePop_grouped <- aggregate(VotePop_long$Count, list(VotePop_long$AgeGroup, VotePop_long$remain.leave), sum)
VotePop_grouped

VotePop_dot <- ggplot(VotePop_grouped, aes(x=Group.1, y=x))
VotePop_dot + geom_linerange(
  aes(x=Group.1, ymin = 0, ymax = x, group=Group.2), 
  color = "lightgray", size = 1.5,
  position = position_dodge(0.3)
)+ 
  geom_point(
    aes(color = Group.2),
    position = position_dodge(0.3), size = 3
  )+
  scale_color_manual(values = c("#0073C2FF", "#EFC000FF"))+
  labs(title="Age Distribution of voters", 
       x = "Age Group", 
       y = "Count") +
  guides(col=guide_legend("Referendum Votes")) +
  theme_minimal()



#######################################
#
## WELLBEING DATA
#
#######################################
# need to merge wellbeing data with clean brexit dataframe 
summary(uk_wellbeing)
View(uk_wellbeing)

# first update column names to make them more meaningful
colnames(uk_wellbeing)

# First put existing column names into a new variable
cnames <- colnames(uk_wellbeing)

# Now change the name of column 2 and store in the variable
cnames[5] <- "2011"
cnames[6] <- "2012"
cnames[7] <- "2013"
cnames[8] <- "2014"
cnames[9] <- "2015"
cnames[10] <- "2016"
cnames[11] <- "2017"
cnames[12] <- "2018"

# Now use 'colnames' to update the column names in the dataframe
colnames(uk_wellbeing) <- cnames

# Confirm update
colnames(uk_wellbeing)
str(uk_wellbeing)

# look at values in Estimate column
unique((uk_wellbeing$Estimate))

# filter data set by rows containing average scores in estimate column only
average_score <- uk_wellbeing[uk_wellbeing$Estimate == "Average (mean)", ]
View(average_score)
str(average_score)

# make version of just UK
UK_average <- average_score[average_score$Geography == "United Kingdom", ]
View(UK_average)

########################################
# subset by country
########################################

# make version of just Scotland
Scotland_avg <- average_score[average_score$Geography == "Scotland", ]
View(Scotland_avg) 

# just Wales
Wales_avg <- average_score[average_score$Geography == "Wales", ]
View(Wales_avg) 

# just England
England_avg <- average_score[average_score$Geography == "England", ]
View(England_avg) 

# just Northern Ireland
NI_avg <- filter(average_score, grepl('N', Region.code))
View(NI_avg) 

########################################
# Reformat (melt)
########################################

# UK
UK_average_long <- UK_average[c(1, 3,5:12)]
View(UK_average_long)

# melt
UK_average_long <- melt(UK_average_long, id.vars = c("Geography", "Measures.of.Well.being"), 
                           measure.vars = c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"),
                           variable.name = "Year", value.name = "Average")

str(UK_average_long)

# need to convert 'Year' from Factor to numeric
UK_average_long$Year <- as.character(UK_average_long$Year)
UK_average_long$Year <- as.numeric(UK_average_long$Year)
View(UK_average_long)

# Scotland
# make long version
Scotland_avg_long <- Scotland_avg[c(1,3,5:12)]

# melt
Scotland_avg_long <- melt(Scotland_avg_long, id.vars = c("Geography", "Measures.of.Well.being"), 
                        measure.vars = c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"),
                        variable.name = "Year", value.name = "Average")
View(Scotland_avg_long)

# need to convert 'Year' from Factor to numeric
Scotland_avg_long$Year <- as.character(Scotland_avg_long$Year)
Scotland_avg_long$Year <- as.numeric(Scotland_avg_long$Year)
View(Scotland_avg_long)

# Wales
# make long version
Wales_avg_long <- Wales_avg[c(1,3,5:12)]

# melt
Wales_avg_long <- melt(Wales_avg_long, id.vars = c("Geography", "Measures.of.Well.being"), 
                          measure.vars = c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"),
                          variable.name = "Year", value.name = "Average")
View(Wales_avg_long)

# need to convert 'Year' from Factor to numeric
Wales_avg_long$Year <- as.character(Wales_avg_long$Year)
Wales_avg_long$Year <- as.numeric(Wales_avg_long$Year)
View(Wales_avg_long)

# England
# make long version
England_avg_long <- England_avg[c(1,3,5:12)]

# melt
England_avg_long <- melt(England_avg_long, id.vars = c("Geography", "Measures.of.Well.being"), 
                          measure.vars = c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"),
                          variable.name = "Year", value.name = "Average")
View(England_avg_long)
str(England_avg_long)

# need to convert 'Year' from Factor to numeric
England_avg_long$Year <- as.character(England_avg_long$Year)
England_avg_long$Year <- as.numeric(England_avg_long$Year)
View(England_avg_long)

# Northern Ireland
# make long version
NI_avg_long <- NI_avg[c(1,3,5:12)]

# melt
NI_avg_long <- melt(NI_avg_long, id.vars = c("Geography", "Measures.of.Well.being"), 
                         measure.vars = c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"),
                         variable.name = "Year", value.name = "Average")
View(NI_avg_long)
str(NI_avg_long)

# need to convert 'Year' from Factor to numeric
NI_avg_long$Year <- as.character(NI_avg_long$Year)
NI_avg_long$Year <- as.numeric(NI_avg_long$Year)
View(NI_avg_long)

###########################################
# time series plot
###########################################

library(RColorBrewer)
display.brewer.pal(n=4, name = "Reds")

# use facet grid to plot each score over time
ggplot(UK_average_long) + aes(x = Year, y = Average, color = Measures.of.Well.being) + geom_point(shape=8) + 
    facet_grid(Measures.of.Well.being ~ ., scales = "free") +
    scale_color_brewer(palette =  "RdBu") +
    theme_light()

ggplot(Scotland_avg_long) + aes(x = Year, y = Average, color = Measures.of.Well.being) + geom_point(shape=8) + 
    facet_grid(Measures.of.Well.being ~ ., scales = "free") +
    scale_color_brewer(palette =  "RdBu") +
    theme_light()

ggplot(Wales_avg_long) + aes(x = Year, y = Average, color = Measures.of.Well.being) + geom_point(shape=8) + 
    facet_grid(Measures.of.Well.being ~ ., scales = "free") +
    scale_color_brewer(palette =  "RdBu") +
    theme_light()

ggplot(England_avg_long) + aes(x = Year, y = Average, color = Measures.of.Well.being) + geom_point(shape=8) + 
    facet_grid(Measures.of.Well.being ~ ., scales = "free") +
    scale_color_brewer(palette =  "RdBu") +
    theme_light()

ggplot(log(NI_avg_long) + aes(x = Year, y = Average, color = Measures.of.Well.being) + geom_point(shape=8) + 
    facet_grid(Measures.of.Well.being ~ .) +
    scale_color_brewer(palette =  "RdBu") +
    theme_light())


# time series chart
# as facet grid
ggplot(England_avg_long) + aes(x = Year, y = Average, color = Measures.of.Well.being) + geom_line() + 
           facet_grid(Measures.of.Well.being ~ ., scales = "free") +
           scale_color_brewer(palette =  "RdBu") +
           theme_light()

library(viridis)

# multiple lines on one plot
no_avg <- England_avg_long %>%
    filter(Measures.of.Well.being %in% c("Happiness", "Life Satisfaction", "Worthwhile")) 
 
# 1st version   
no_avg %>%
ggplot(aes(x = Year, y = Average, color = Measures.of.Well.being)) + geom_line() + 
    scale_color_viridis(discrete = TRUE) +
    ggtitle("Wellbeing measures in England 2012 - 2019") +
    theme_minimal()

# final version
# England
no_avg %>%
    ggplot(aes(x = Year, y = Average, color = Measures.of.Well.being)) + geom_line(size = 2) + 
    scale_color_brewer(palette = "YlOrRd") +
    ggtitle("Wellbeing measures in England 2012 - 2019") +
    guides(col=guide_legend("Wellbeing Measures")) +
    theme_minimal()

# Scotland

no_avg <- Scotland_avg_long %>%
    filter(Measures.of.Well.being %in% c("Happiness", "Life Satisfaction", "Worthwhile")) 

no_avg %>%
    ggplot(aes(x = Year, y = Average, color = Measures.of.Well.being)) + geom_line(size = 2) + 
    scale_color_brewer(palette = "YlOrRd") +
    ggtitle("Wellbeing measures in Scotland 2012 - 2019") +
    guides(col=guide_legend("Wellbeing Measures")) +
    theme_minimal()

# Wales

no_avg <- Wales_avg_long %>%
  filter(Measures.of.Well.being %in% c("Happiness", "Life Satisfaction", "Worthwhile")) 

no_avg %>%
  ggplot(aes(x = Year, y = Average, color = Measures.of.Well.being)) + geom_line(size = 2) + 
  scale_color_brewer(palette = "YlOrRd") +
  ggtitle("Wellbeing measures in Wales 2012 - 2019") +
  guides(col=guide_legend("Wellbeing Measures")) +
  theme_minimal()

# Northern Ireland

no_avg <- NI_avg_long %>%
  filter(Measures.of.Well.being %in% c("Happiness", "Life Satisfaction", "Worthwhile")) 

no_avg %>%
  ggplot(aes(x = Year, y = Average, color = Measures.of.Well.being)) + geom_line(size = 2) + 
  scale_color_brewer(palette = "YlOrRd") +
  ggtitle("Wellbeing measures in Northern Ireland 2012 - 2019") +
  guides(col=guide_legend("Wellbeing Measures")) +
  theme_minimal()

## ANXIETY SCORES
# make version of just anxiety score
anxiety_scores <- average_score[average_score$Measures.of.Well.being == "Anxiety", ]
View(anxiety_scores) 

# make long version
anxiety_long <- anxiety_scores[c(1,3,5:12)]

# melt
anxiety_long <- melt(anxiety_long, id.vars = c("Geography", "Measures.of.Well.being"), 
                         measure.vars = c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"),
                         variable.name = "Year", value.name = "Average")
View(anxiety_long)
str(anxiety_long)

# need to convert 'Year' from Factor to numeric
anxiety_long$Year <- as.character(anxiety_long$Year)
anxiety_long$Year <- as.numeric(anxiety_long$Year)
View(anxiety_long)

# Anxiety scores time series
# multiple lines on one plot
anxiety_avg <- anxiety_long %>%
  filter(Geography %in% c("England", "Scotland", "Wales", "Northern Ireland")) 

anxiety_avg %>%
  ggplot(aes(x = Year, y = Average, color = Geography)) + geom_line(size = 2) +
  scale_color_brewer(palette = "YlOrRd") +
  ggtitle("Anxiety Scores in UK 2012 - 2019") +
  guides(col=guide_legend("Anxiety Score")) +
  theme_minimal()

##################need to remove#################################

###########################################
# filter each average score per well being
###########################################

# happiness average scores
happiness_average <- average_score[average_score$Measures.of.Well.being == "Happiness", ]
View(happiness_average)

# make a long version
happiness_avg_long <- happiness_average[c(1,5:12)]

happiness_avg_long <- melt(happiness_avg_long, id.vars = c("Geography"), 
                          measure.vars = c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"),
                          variable.name = "Year", value.name = "Average")

# time series plot
ggplot(satisfaction_long, aes(x = Year, y = Score)) + 
    geom_line(aes(color = Geography), size = 1)
theme_minimal()


########################################
# subset by other estimates (not average)
########################################

# create subset for each wellbeing score
happiness_score <- uk_wellbeing[uk_wellbeing$Measures.of.Well.being == "Happiness", ]
View(happiness_score)

anxiety_score <- uk_wellbeing[uk_wellbeing$Measures.of.Well.being == "Anxiety", ]
View(anxiety_score)

satisfaction_score <- uk_wellbeing[uk_wellbeing$Measures.of.Well.being == "Life Satisfaction", ]
View(satisfaction_score)

worthwhile_score <- uk_wellbeing[uk_wellbeing$Measures.of.Well.being == "Worthwhile", ]
View(worthwhile_score)

## need to drop the average scores from each one

happiness_score2 <- happiness_score[happiness_score$Estimate != "Average (mean)", ]
View(happiness_score2)

anxiety_score2 <- anxiety_score[anxiety_score$Estimate != "Average (mean)", ]
View(anxiety_score2)

satisfaction_score2 <- satisfaction_score[satisfaction_score$Estimate != "Average (mean)", ]
View(satisfaction_score2)

worthwhile_score2 <- worthwhile_score[worthwhile_score$Estimate != "Average (mean)", ]
View(worthwhile_score2)

# convert to long format
happiness_score <- happiness_score2[happiness_score2$Measures.of.Well.being == "Happiness", ]
View(happiness_score)

# happiness scores
happiness_score_long <- happiness_score2[c(1,4:12)]
gather(happiness_score_long, qt, score, 2011:2018)

happiness_score_long <- melt(happiness_score_long, id.vars = c("Geography", "Estimate"))

# satisfaction scores
satisfaction_score2 <- satisfaction_score2[c(1,4:12)]

satisfaction_long <- melt(happiness_score_long, id.vars = c("Geography", "Estimate"), 
    measure.vars = c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"),
    variable.name = "Year", value.name = "Score")

# time series plot
ggplot(satisfaction_long, aes(x = Year, y = Score)) + 
    geom_line(aes(color = Geography), size = 1)
    theme_minimal()
    
##################end of remove#################################

######################################
# Brexit headlines wordcloud 
######################################
    
# load file
fname4 <- file.choose()
headlines <- read.csv(file = fname4
                          , header = TRUE
                          , stringsAsFactors = FALSE
)
str(headlines)
    
# create corpus
corpus = Corpus(VectorSource(headlines$title))
# look at corpus
corpus[[1]][1]
    
# convert to lowercase
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, tolower)
    
# remove punctuation
corpus = tm_map(corpus, removePunctuation)
    
# remove stopwords
corpus = tm_map(corpus, removeWords, c("brexit", "johnson,", "eu,", "britain,", stopwords("english")))
    
# eliminate white spaces
corpus = tm_map(corpus, stripWhitespace)
corpus[[1]][1]
    
# create DTM
DTM <- TermDocumentMatrix(corpus)
mat <- as.matrix(DTM)
f <- sort(rowSums(mat), decreasing = TRUE)
dat <- data.frame(word = names(f), freq=f)
head(dat, 5)
    
gsub(",", "", dat$word) 
    
# create wordcloud
    
# make palette function
myPal <- colorRampPalette(c("red", "white"))
    
par(mar=c(0,0,0,0), bg = "#00519B")
    
set.seed(100)
wordcloud(words = dat$word
              , freq = dat$freq
              , min.freq = 3
              , max.words = 250
              , random.order = FALSE
              , colors = myPal(length(dat$word)))
    
    







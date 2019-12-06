##################################
# Code for Segmentation
#################################################################


# ###########################################################
# Analysing the Grocery Store data
# ###########################################################

# load from the file location

# load by browsing
seg.raw <- read.csv(file.choose())

# check column names 
names(seg.raw)

head(seg.raw) # info on 300 customers and their subscription decisions

# Male Dummy
seg.raw$Male[seg.raw$gender=='Female'] <- 0
seg.raw$Male[seg.raw$gender=='Male'] <- 1

# ###########################################################

# a simple function to report means by group
# if the data is solely numeric you can simply use the aggregate() function to do this
seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))  
}

# Illustration of how seg.summ() works

seg.summ(seg.raw, seg.raw$Segment)

str(seg.raw)

#### K-MEANS CLUSTERING
# convert factor variables to numeric (kmeans requires). OK b/c all are binary.
seg.df.num <- seg.raw[c(
                       'PointsTotal.x',
                       'Age',
                       'OnlineTicketPurchaser_value',
                       'TuesdayAttendee_value',
                       'ConcessionPurchaser_value',
                       'AttendsWithChild_value',
                       'WeekendMatineeViewer_value',
                       'WeekdayMatineeViewer_value',
                       'BlackEarnCount',
                       'BlackEarnPointTotal',
                       'BlackBurnCount',
                       'BlackBurnPointTotal',
                       'BlackActivityDays',
                       'AccountOpenD2D',
                       'PacPadD2D',
                       'VISACineplex',
                       'VISA',
                       'CineplexIssue',
                       'CineplexRedeem',
                       'PointsScene',
                       'CARAIssue',
                       'CARARedeem'
                       )]
summary(seg.df.num)


# Determine number of clusters
# My goal is to minimize the within cluster variance
# Lines 51-53 calculates within cluster sum of squares for each number of clusters
wss <- (nrow(seg.df.num)-1)*sum(apply(seg.df.num,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(seg.df.num, 
                                     centers=i)$withinss)

# The elbow graph to gauge the number of clusters
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

set.seed(96743) # set seed to make sure the random assignment starts at the same point
seg.k4 <- kmeans(seg.df.num, centers=4)

# inspect the 4-segment solution by looking at the means per segment per numeric
seg.summ(seg.df.num, seg.k4$cluster)
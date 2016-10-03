if (!require(data.table)) {
  install.packages("data.table", repos="http://cran.us.r-project.org")
}
if (!require(ggplot2)) {
  install.packages("ggplot2", repos="http://cran.us.r-project.org")
}
if (!require(gtools)) {
  install.packages("gtools", repos="http://cran.us.r-project.org")
}
if (!require(gridExtra)) {
  install.packages("gridExtra", repos="http://cran.us.r-project.org")
}
if (!require(reshape2)) {
  install.packages("reshape2", repos="http://cran.us.r-project.org")
}
if (!require(plyr)) {
  install.packages("plyr", repos="http://cran.us.r-project.org")
}
if (!require(chron)) {
  install.packages("plyr", repos="http://cran.us.r-project.org")
}

library(data.table)
library(reshape2)
library(plyr)
library(grid)
library(stringr)
library(gridExtra)
library(ggplot2) 
library(gtools)
library(chron)


# Create a nice printf function
printf <- function(...) cat(sprintf(...))

workingDir = '/Users/michaeltauberg/Documents/Hilary'
setwd(workingDir)

emails = read.csv('Emails.csv')

#hilarySent = emails[emails$MetadataFrom == 'H',]
h1 = emails[emails$ExtractedFrom == 'H <hrod17@clintonemail.com>',]
h2 = emails[emails$ExtractedFrom == 'H <HDR22@clintonemail.com>',]
h3 = emails[emails$MetadataFrom == 'H',]
hilarySent = unique(rbind(h1,h2,h3)) # get all email aliases


# Get the days of the week her mail was sent
daysOfWeek = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

hSentDays = data.frame()
for (day in daysOfWeek) {
  daySent = hilarySent[grepl(day, hilarySent$ExtractedDateSent), ]
  daySent$day = rep(day, nrow(daySent))
  hSentDays = rbind(hSentDays,daySent )
}

hSentDays$ExtractedDateSent = gsub(',', ' ', hSentDays$ExtractedDateSent) # remove commas for spaces which we can split on
hSentDays$ExtractedDateSent = gsub('\\.', ' ', hSentDays$ExtractedDateSent) # remove periods for spaces which we can split on
hSentDays$ExtractedDateSent = gsub('-', ' ', hSentDays$ExtractedDateSent) # remove dashes
hSentDays$ExtractedDateSent = gsub('AM.*', 'AM', hSentDays$ExtractedDateSent) # remove anything after AM/PM
hSentDays$ExtractedDateSent = gsub('PM.*', 'PM', hSentDays$ExtractedDateSent)

hSentDays$ExtractedDateSent = paste(' ', hSentDays$ExtractedDateSent, sep="")  # add a leading space in case some entries were missing it

# We have to drop entries where there is no colon in ExtractedDateSent. Need the colon for parsing
hSentDays = hSentDays[grepl(':',hSentDays$ExtractedDateSent), ]

# get composite parts
dateElems = strsplit(hSentDays$ExtractedDateSent, ' +')
times = c()
for (i in 1:length(dateElems)) {
  ampm = dateElems[[i]][length(dateElems[[i]])]  # last element
  time = dateElems[[i]][length(dateElems[[i]])-1] # second last element
  
  # if the time value is too short, it's in the previous chunk, so add it back
  #if (nchar(time) < 5) {
  #if (!grepl(':', time))
    #time = paste(dateElems[[i]][length(dateElems[[i]])-2],dateElems[[i]][length(dateElems[[i]])-1], sep=":")  
  #}
  
  # if dates were not seperated correctly, sub out the year values - data cleaning
  time = gsub('2009', '', time)
  time = gsub('2010', '', time)
  time = gsub('2011', '', time)
  time = gsub('2012', '', time)
  
  # add missing semicolon when there is no space
  #if (!grepl(':', time)) {
  #  time = paste(substr(time, 1, 2), substr(time,3,4), sep=":")
  #}
  timeElems = strsplit(time, ':')
  hour = as.numeric(timeElems[[1]][1])
  min = timeElems[[1]][2]
  seconds = "00" # set seconds to dummy value
  
  if (grepl("PM", ampm)) {
    if (hour != 12) {  
      hour = hour + 12 # convert to 24hr format
    }
  }
  else { # AM
    if (hour == 12) {  #12AM = hour 0 in 24 hour scale
      hour = '0'
    }
  }
  hour = as.character(hour)
  time = paste(paste(hour,min,sep=":"), seconds, sep=":")
  times = c(times, time)
}

hours = c()
timeElems = strsplit(times, ':')
for (elem in timeElems) {
  hours = c(hours, as.numeric(elem[1]))
} # get first one


hSentDays = cbind(hSentDays, times, hours)
defineDayPeriod = function (x) {
  if ((x<6) || (x >22)) {return("non_work_hrs")}
  else {return("working_hrs")}
}
hSentDays$dayPeriod = sapply(hSentDays$hours, defineDayPeriod) 
hSentDays$hours = factor(hSentDays$hours)
hSentDays$dayPeriod = factor(hSentDays$dayPeriod)
p = ggplot(hSentDays, aes(x=hours, fill=dayPeriod)) + geom_histogram(binwidth=1) # days of email sent # days of email sent
p = p + theme(axis.text.x=element_text(angle=75, hjust=1))
p = p + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Hour Sent") + ylab("Num Emails") + ggtitle("Number of Emails Sent by Hour")
p = p + theme(plot.title = element_text(lineheight=.8, face="bold"))
ggsave(filename = "./HilaryHoursSent.png", plot=p, width=6) 

# play with AM/PM


hSentDays$day = factor(hSentDays$day, 
                       levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


p = ggplot(hSentDays, aes(x=day)) + geom_histogram(binwidth=1, fill="#FF9999") # days of email sent
p = p + theme(axis.text.x=element_text(angle=75, hjust=1))
p = p + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
p = p + theme(plot.title = element_text(lineheight=.8, face="bold"))
p = p + xlab("Weekday") + ylab("Count") + ggtitle("Number of Mail Sent by Weekday")
p = p + guides(fill=FALSE) 
ggsave(filename = "./HilaryWeekdaySent.png", plot=p, width=6) 

# using percentages instead of count
p = ggplot(hSentDays, aes(x=day,fill=day)) + geom_bar(aes(y = (..count..)/sum(..count..)*100), binwidth = 1)  
p = p + theme(axis.text.x=element_text(angle=75, hjust=1))
p = p + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
p = p + theme(plot.title = element_text(lineheight=.8, face="bold"))
p = p + xlab("Weekday") + ylab("% Sent") + ggtitle("Percentage of Mail Sent by Weekday")
p = p + guides(fill=FALSE) 
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
# To use for fills, add
scale_fill_manual(values=cbPalette)
p = p + scale_fill_hue(c=85, l=80)
ggsave(filename = "./HilaryWeekdaySentPercentage.png", plot=p, width=6) 

# change count to percent


# who did she send emails to 
# get the top 20 people
p = ggplot(hSentDays, aes(x=ExtractedTo)) + geom_histogram() # days of email sent
# get the bottom 20 people

# how many times does she say thanks, thx or thanks you? what percentage. "sorry". "appreciate"

hilarySent = emails[emails$ExtractedFrom == 'H <hrod17@clintonemail.com>',]
hilaryReplied = hilarySent[grep("Re:", hilarySent$ExtractedSubject), ]

# use python. Grep for Original Message. Get orig sent date and our date
  # or find first "Sent:" and second "Sent:"

# e-mail reponse rate? -> find all to:H. Then see how many have corresponding emails with same subject
# search "    christmas present"

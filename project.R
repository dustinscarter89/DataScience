#Project
library(ggplot2)





ecdat <- read.csv("Ecdat2.csv")

#Create a boxplot of the average number of packs per capita by state. 
# Inputs data into ggplot for display and displays data as boxplot
box1 <- ggplot(ecdat , aes(x = state, y = packpc)) + geom_boxplot(aes(group = state)) + scale_y_continuous(breaks=seq(0,200,5))
#Which states have the highest number of packs?
print(arrange(ecdat, desc(packpc)))

#Which have the lowest?
print(arrange(ecdat, packpc))

#Find the median over all the states of the number of packs per capita for each year. 
print(select(ecdat, year, packpc))
middle <- ecdat %>% group_by(year) %>% summarize(med = median(packpc))
print(middle)

#Plot this median value for the years from 1985 to 1995.

time_plot <- ggplot(middle, aes(x = year, y = med)) + geom_col(aes(group = year)) + scale_y_continuous(breaks = seq(0,120,3))

#What can you say about cigarette usage in these years?
  #Cigarette usage/capita over the time interval 1985 - 1995 has gone down

#Create a scatter plot of price per pack vs number of packs per capita for all states and years.
#Do a linear regression for these two variables. 

price_vs_packspc <- ggplot(ecdat, aes(x = avgprs, y = packpc)) + geom_point() + geom_smooth(method=lm, se=FALSE)
lin.reg <- lm(packpc ~ avgprs, ecdat)
print(lin.reg)
#Are the price and the per capita packs positively correlated, negatively correlated, or uncorrelated? 
#Explain why your answer would be expected.

  #price and per capita packs are negatively correlated, weakly. 
  #I would expect as the price goes up for the per capita packs to go down.
info <- summary(lin.reg)
print(info)
#________________________________________________________________________________________

#Change your scatter plot to show the points for each year in a different color.

price_vs_packspc2 <- ggplot(ecdat, aes(avgprs, packpc,colour = factor(year)))  + geom_point() + geom_smooth(method=lm, se=FALSE)

#Does the relationship between the two variable change over time?
  # yes it does

#How much variability does the line explain?
# 34%

#The plot in Part 3 does not adjust for inflation. 
#You can adjust the price of a pack of cigarettes for inflation by dividing the avgprs variable by the cpi variable. 
#Create an adjusted price for each row, then repeat Part 3 using this adjusted price.

price_vs_packspc3 <- ggplot(ecdat, aes(avgprs/cpi, packpc,colour = factor(year)))  + geom_point() + geom_smooth(method=lm, se=FALSE)

#Create a data frame with just the rows from 1985. 

rows1985 <- filter(ecdat, year =="1985")
print(rows1985)
#Create a second data frame with just the rows from 1995.
rows1995 <- filter(ecdat, year =="1995")
print(rows1995)

#Then, from each of these data frames, get a vector of the number of packs per capita. 

v1985 <- c(select(rows1985,packpc))
v1995 <- c(select(rows1995,packpc))




print(v1985)
print(v1995)






#Use a paired t-test to see if the number of packs per capita in 1995 was significantly different than the number of packs per capita in 1985.
#In the proess of doing this project, have any questions come to mind that this data set could answer? If so, pick one and do the analysis to find the answer to your question.

t.test(v1985$packpc,v1995$packpc)
# there is a significance difference between the pack numbers of 1985 and 1995

#how has taxes affected the number of pacs

pacpc_vs_tax <- ggplot(ecdat, aes(x = tax, y = packpc)) + geom_point() + geom_smooth(method=lm, se=FALSE)

lin4.reg <- lm(packpc ~ tax, ecdat)
print(summary(lin4.reg))

# taxes are negatively correlated to number of packs , meaning as taxes go up the number of packs goes down.
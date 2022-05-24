online_retail <-read.csv("online retail.csv",header=T,colClasses = "factor")
names(online_retail)
head(online_retail)
tail(online_retail)
summary(online_retail)
strans(online_retail)
dim(online_retail) 
#install and load package arules
install.packages("arules")
library(arules)
#install and load arulesViz
install.packages("arulesViz")
library(arulesViz)
#install and load plyr
install.packages("plyr")
library(plyr)
library(dplyr)
#we need to do is group data in the market basket dataframe by Member_number.
#We need this grouping and apply a function on it and store the output in another dataframe.
#This can be done by ddply.
transansactionData <- ddply(online_retail,c("CustomerID"),
                         function(df1)paste(df1$Description,
                                            collapse = ","))
#save the file as the transansaction data.
write.csv(transansactionData,"C:/USERS/LANRE/DESKTOP/transansactions.csv", quote = FALSE, row.names = FALSE)
#read the file.
trans <- read.transactions('C:/USERS/LANRE/DESKTOP/transansactions.csv', format = 'basket', sep=',')

trans

summary(trans)

install.packages("RColorBrewer")

library(RColorBrewer)

#create an item Frequency Bar Plot to view the distransibution of objects with absolute frequency.

itemFrequencyPlot(trans,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

#create an item Frequency Bar Plot to view the distransibution of objects with relative frequency.

itemFrequencyPlot(trans,topN=20,type="relative",col=brewer.pal(8,'Pastel2'),main="Relative Item Frequency Plot")

#generating rules
rules <- apriori(trans,
                 parameter =list(minlen=1,maxlen=2, conf = 0.001))

inspect(rules)

rules2 <- apriori(trans,
                 parameter = list(minlen=1, maxlen=2,conf = 0.00001),
                 appearance= list(rhs="BAKING SET 9 PIECE RETROSPOT",default="lhs"))

summary(rules2)
inspect(rules2)

plot(rules)

plot(rules,method = "grouped")

plot(rules@quality)


install.packages("shiny")
library(shiny)

rules_ex <-apriori(trans,
                   parameter =list(minlen=1,maxlen=2,conf=0.004) )
ruleExplorer(trans)

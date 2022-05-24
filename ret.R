#read the dataset

retail <- read.csv("Online Retail.csv",header=T,colClasses = "factor")

#inspect the element

names(retail)
head(retail)
tail(retail)
summary(retail)
str(retail)

dim(retail)



install.packages("Matrix")
library("Matrix")
install.packages("arulesViz")
library(arulesViz)
install.packages("arules")
library("arules")
set.seed = 220 # Setting seed

associa_rules = apriori(data = retail, 
                        parameter = list(support = 0.004, 
                                         confidence = 0.002))

# Plot
plot(retail, topN = 10)

# Visualising the results
inspect(sort(associa_rules, by = 'lift')[1:10])
plot(associa_rules, method = "graph", 
     measure = "confidence", shading = "lift")
rules <- apriori(retail)
summary(rules)

inspect(rules) 

rules <- apriori(retail,
                 parameter =list(minlen=2,maxlen=3, conf = 0.95))
summary(rules)
inspect(rules)

summary(retail)

barplot(CustomerID)

rules <- apriori(retail,
                 parameter = list(minlen=2, maxlen=3,conf = 0.95),
                 appearance= list(rhs=c("cosmetics=Yes"),default="lhs")) 

summary(rules)
inspect(rules)

rules <- apriori(retail,
                 parameter = list(minlen=2, maxlen=3,conf = 0.70),
                 appearance= list(rhs=c("cosmetics=Yes"),default="lhs"))

summary(rules)
inspect(rules)

install.packages("arulesViz") 
library(arulesViz)

plot(rules)

plot(rules, method="grouped")

plot(rules@quality) 

rules3 <- apriori(retail,
                  parameter = list(minlen=2,maxlen=4, conf = 0.60),
                  appearance =list(rhs=c("banana=Yes","apples=Yes","avocado=Yes")
                                   ,default="lhs") )

plotly_arules(rules3)

plotly_arules(rules3, measure = c("support", "lift"), shading = "confidence")

rules2 <- apriori(retail,
                  parameter = list(minlen=2, maxlen=3,conf = 0.7),
                  appearance =list(rhs=c("cosmetics=Yes"),
                                   lhs=c("apples=Yes",
                                         "banana=Yes",
                                         "coke=Yes",
                                         "turkey=Yes",
                                         "bourbon=Yes",
                                         "ice_cream=Yes",
                                         "baguette=Yes",
                                         "soda=Yes",
                                         "choclate=Yes",
                                         "cracker=Yes",
                                         "avocado=Yes",
                                         "sardines=Yes"),
                                   default="none"))

inspect(rules2)

rules_ex <-apriori(retail,
                   parameter =list(minlen=2,maxlen=4,conf=0.75))

ruleExplorer(rules_ex)


ruleExplorer(retail)
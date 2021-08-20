# Project 1 - Market Basket Analysis
# Problem Statement:
# Consider yourself to be the newly appointed manager of a retail-store 'ALL-MART'. Your first
# task as manager of the store is to increase cross-selling.

library(arules)
install.packages("arulesViz")
library(arulesViz)
library(dplyr)

market_basket <- read.transactions(
  
  file = 'C:\\Users\\Asus_M\\Desktop\\Market Basket Analysis\\market_basket.csv',
  sep = ',',
  quote ="",
  format = 'basket',
  rm.duplicates = T,
  skip = 1
)
# summary(market_basket)
# 
# A) Understand the Transactions
# a. Find the total number of transactions

# From the summary : the total number of transactions is equivalent to the number 
# of rows in our case the number of rows = 18440 = Total Number of transactions

# b. Find the total number of items in the inventory

# From the summary : the total number of items is equivalent to the number 
# of columns in our case the number of columns = 22346 = Total Number of items.

# c. Find the total number of items purchased

# To find the number of items that were purchased :

# Total number of items purchased = Total number of items * total number of transactions * density

total_number_items_purchased = 18440 * 22346 * 0.0009915565

total_number_items_purchased #[1] 408581

# d. Find out the 10 most frequently bought items & make a plot

library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)
itemFrequencyPlot(x=market_basket,
                   topN =10,
                   type='absolute',
                   horiz=T,
                   col=brewer.pal(10,"Dark2")
                   )

# B) Building 1st set of association rules
# a. Build apriori algorithm with support value-> 0.005 & Confidence value-> 0.8

rule1 <- market_basket %>% 
  apriori(parameter = list(supp=0.005,conf=0.8)) 

summary(rule1)

# b. Sort the rules w.r.t confidence & inspect the top 5 rules & the bottom 5 rules
rule1 <- market_basket %>% 
  apriori(parameter = list(supp=0.005,conf=0.8)) %>% 
  sort(by='confidence')

rule1 %>% head(5) %>%  inspect

rule1 %>% tail(5) %>%  inspect

# c. Sort the rules w.r.t lift & Inspect the top 5 rules

rule1 <- market_basket %>% 
  apriori(parameter = list(supp=0.005,conf=0.8)) %>% 
  sort(by='lift')

rule1 %>% head(5) %>%  inspect

rule1 %>% tail(5) %>%  inspect

# d. Plot the rules using different methods

# First Method :

plot(rule1,engine ="htmlwidget")

plot(rule1,method = "two-key",engine ="htmlwidget")

plot(rule1,method = "graph",engine ="htmlwidget")


# C) Building 2nd set of association rules
# a. Build apriori algorithm with support value-> 0.009 & Confidence value-> 0.3

rule2 <- market_basket %>% 
  apriori(parameter = list(supp=0.009,conf=0.3)) 

summary(rule2)

# b. Sort the rules w.r.t confidence & inspect the top 5 rules & the bottom 5 rules

rule2 <- market_basket %>% 
  apriori(parameter = list(supp=0.009,conf=0.3)) %>% 
  sort(by='confidence')

rule2 %>% head(5) %>%  inspect

rule2 %>% tail(5) %>%  inspect

# c. Plot the rules using different methods

plot(rule2,engine ="htmlwidget")

plot(rule2,method = "two-key",engine ="htmlwidget")

plot(rule2,method = "graph",engine ="htmlwidget")

# D) Building 3rd set of association rules
# a. Build apriori algorithm with support value-> 0.02 & Confidence value-> 0.5

rule3 <- market_basket %>% 
  apriori(parameter = list(supp=0.002,conf=0.5)) 

summary(rule3)

# b. Sort the rules w.r.t support & inspect the top 5 rules & the bottom 5 rules

rule3 <- market_basket %>% 
  apriori(parameter = list(supp=0.002,conf=0.5)) %>% 
  sort(by='support')

rule3 %>% head(5) %>%  inspect

rule3 %>% tail(5) %>%  inspect

# c. Plot the rules using different methods

plot(rule3,engine ="htmlwidget")

plot(rule3,method = "two-key",engine ="htmlwidget")

plot(rule3,method = "graph",engine ="htmlwidget")


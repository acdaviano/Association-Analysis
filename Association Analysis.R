#After loading the datasrt into R we look at the structure to get familiar with it. 
str(bxBookRatings)
str(bxBooks)
str(bxUsers)
#while looking at how it looks we then want to visualie the data and get some 
#better insight into the datset in order to make inferences. 
#I then loading the required libraries that would be needed to visualize the data.
require(dplyr)
require(ggplot2)
require(arules)
# I then wanted to visualize the top 20 books with ratings.We will first create a 
# new formula to look at the n amount of books through group and summary.
top20<-bxBookRatings %>% group_by(ISBN) %>% summarise(n=n()) %>%
  top_n(n=20) %>% arrange(n)

#Then I looked at the actual top 20 books.
top20<-merge(top20,bxBooks[,c("ISBN","Book.Title")])
ggplot(top20,aes(x=reorder(Book.Title,n),n))+ geom_bar(stat='identity')+ 
  theme(axis.text.x=element_text(angle=90, hjust=1))+ coord_flip() + 
  labs(x = "Book Title",y="Number of Ratings")

#Once I looked at the data, I then took the top 20 like the assignment states.
top20<-bxBookRatings %>% group_by(ISBN) %>% summarise(n=n()) %>%
  top_n(n=21) %>% arrange(n)

#Visual of the top 10 books and their ratings.
top20<-merge(top20,bxBooks[,c("ISBN","Book.Title")])
ggplot(top20,aes(x=reorder(Book.Title,n),n))+ geom_bar(stat='identity')+ 
  theme(axis.text.x=element_text(angle=90, hjust=1))+ coord_flip() + 
  labs(x = "Book Title",y="Number of Ratings")

#I then transformed the data to a transaction set and looked for the confidence/support
#thresholds to find the rules of the datset as we are trying to do. I alsotook a sample in 
#order to run the process faster. 
samplebooks<-bxBookRatings[sample(nrow(bxBookRatings), 900000), ]
samplebooks$User.ID<-as.factor(samplebooks$User.ID)
samplebooks$ISBN<-as.factor(samplebooks$ISBN)
samplebooks$Book.Rating<-as.factor(samplebooks$Book.Rating)
ratings<-as(split(samplebooks[,2],samplebooks[,1]),"transactions")
rm(ratings)
list(ratings)
#I then created the rules set with a supoort and confience specified =.
rules <- apriori(samplebooks, parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
summary(rules)
inspect(rules)
# This showed us that we needed to lower the supoort/confidence thresholds due to no rules
#being processed. So I lowered both the condifence and the support.
rules <- apriori(samplebooks, parameter = list(supp = 0.005, conf = 0.7, target = "rules"))
summary(rules)
inspect(rules)
# just as the hint stated i lowered the support and the confidence to 0.005
#for confidence we placed it at 0.7. However, they both may have needed to be lower 
#to get any rules to be processed because only 1 was produced with these parameters.
rules <- apriori(samplebooks, parameter = list(supp = 0.0005, conf = 0.5, target = "rules"))
summary(rules)
inspect(rules)
#I then sorted the rules by the lift in order to give some more perspective to the output.
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted[1:20])
#once we have the sorted set we can plot the rules to gain more insight.
library(arulesViz)
plot(rules.sorted[1:10]) 
plot(rules.sorted[1:5], method="graph", control=list(type="items"))
plot(rules[1:2], method="paracoord", control=list(reorder=TRUE))
plot(rules[1:5],method = "matrix3D")
#the above shows the visual plots of the rules.
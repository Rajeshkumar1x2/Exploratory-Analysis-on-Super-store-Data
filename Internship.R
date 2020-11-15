View(SampleSuperstore)
attach(SampleSuperstore)
SampleSuperstore<- subset(SampleSuperstore, select= -c(Country))
SampleSuperstore
# Data Visualization
# Examining  the Orders based upon the region:
byregion<- table(SampleSuperstore$Region)
barplot(byregion, main="Total orders by region",
        xlab= "Region", col = "red")
# We can see from the barplot that the western region has the highest order count(>3000), followed by Eastern, central and Southern Regions.

# Frequency of the Quantity oedered
hist(SampleSuperstore$Quantity, main = "Frequency distribution of quantity ordered",
     xlab = "Quantity ordered", ylab = "Frequency", col = "lightblue")
# The maximumm ordered quantity is 1 (>3000) followed by 2, the frequency of the orders is reducing as the quantity orderd is rising, they tend to have an inverse relationship.

#Sales by category
install.packages("dplyr")
library("dplyr")
library(magrittr)
sc<- SampleSuperstore%>%group_by(Category) %>% summarize(Sales=sum(Sales))
pcta<- round(sc$Sales/sum(sc$Sales)*100)
lblsa<-paste(sc$Category,pcta)
lblsa<-paste(lblsa, "%", sep=" ")
colors= c("yellow", "red", "black")
pie(sc$Sales, labels =lblsa, main = "Percentage of sales by category")
Inference1<- c("There are three categories - Furniture, Office supplies and technology, of which the category technology has been the major component of the sales, contributing 36% to the sales, followed by Furniture(32%) and Office supplies(31%)")
Inference1

# Profits by category
pc<- SampleSuperstore%>%group_by(Category) %>% summarize(Profit=sum(Profit))
pctb<- round(pc$Profit/sum(pc$Profit)*100)
lblsb<-paste(pc$Category,pctb)
lblsb<-paste(lblsb, "%", sep=" ")
colors= c("yellow", "red", "black")
pie(pc$Profit, labels =lblsb, main = "Percentage of profits by category")
Inference2<- c("The furniture category being the second major contributor to the sales, fetches the store only with 6% of the total profits,it is hence profitable for the store to invest more upon the develoipment of the office supplies or make changes in the prices which is far more profittable compared to furniture")
Inference2

# Sales by subcategory
library("dplyr")
ssc<- SampleSuperstore%>%group_by(Sub.Category) %>% summarize(Sales=sum(Sales))
pctc<- round(ssc$Sales/sum(ssc$Sales)*100)
lblsc<-paste(ssc$Sub.Category,pctc)
lblsc<-paste(lblsc, "%", sep=" ")
colors= c("yellow", "red", "black")
pie(ssc$Sales, labels =lblsc, main = "Percentage of sales by Subcategory")
Inference3<- c("from the above chart we get to know that phones and Chairs are the highest components of sales(14%), while envelopes and fastners form the least component contributing close to zero percent ")
Inference3

#checking correlation
install.packages("corrplot")s
mydata<- SampleSuperstore[, c(9,10,11,12)]
View(mydata)
library(corrplot)
mydata.cor= cor(mydata)
mydata.cor
corrplot(mydata.cor)
Inference4<- c(" from the correlation plot we infer that, the Sales and profit are somewhat related, profit and quantity are also weakly related, profit and discount are also negatively related")
Inference4

#Profits by region
D1<-aggregate.data.frame(SampleSuperstore$Profit,by=list(SampleSuperstore$Region),sum)
names(D1)<-c("Region","Total_Profits")                         
D2<-D1[order(D1$Total_Profits,decreasing = FALSE),]
barplot(D2$Total_Profits,names.arg =D2$Region,col= "pink", horiz= TRUE,border =NA,xlim=c(0,200000),las=1,
        cex.names=0.71)
options(scipen =1000)
title("Total Profits Area wise")
Inferenece5<- c("from the above inferences we get to know that the super store is earning huge profits from the western region")
Inferenece5

D3<-aggregate.data.frame(SampleSuperstore$Sales,by=list(SampleSuperstore$State),sum)
names(D3)<-c("State","Total_Sales")                         
D4<-D3[order(D3$Total_Sales,decreasing = FALSE),]
barplot(D4$Total_Sales,names.arg =D4$State,col= "pink", horiz= TRUE,border =NA,xlim=c(0,500000),las=1,
        cex.names=0.71)
options(scipen =1000)
title("Total Sales State wise")

D5<-aggregate.data.frame(SampleSuperstore$Profit,by=list(SampleSuperstore$State),sum)
names(D5)<-c("State","Total_Profits")                         
D6<-D5[order(D5$Total_Profits,decreasing = FALSE),]
barplot(D6$Total_Profits,names.arg =D6$State,col= "pink", horiz= TRUE,border =NA,xlim=c(0,200000),las=1,
        cex.names=0.71)
options(scipen =1000)
title("Total Profits State wise")
Inference6<- c("States such as California, washington and virginia are the highest profitable states to the super store, however due to several reasons the store has failed to improve its salaes in states of washington, Virginia,etc. Instead sales are high in regions were there are losses in places such as Pensilvaniya, Texas etc. which has to be looked upon")
Inference6

library("dplyr")
sbs<- SampleSuperstore%>%group_by(Segment) %>% summarize(Sales=sum(Sales))
pctd<- round(sbs$Sales/sum(sbs$Sales)*100)
lblsd<-paste(sbs$Segment,pctd)
lblsd<-paste(lblsd, "%", sep=" ")
colors= c("yellow", "red", "black")
pie(sbs$Sales, labels =lblsd, main = "Percentage of sales by Segment")

library("dplyr")
pbs<- SampleSuperstore%>%group_by(Segment) %>% summarize(Profit=sum(Profit))
pcte<- round(pbs$Profit/sum(pbs$Profit)*100)
lblse<-paste(pbs$Segment,pcte)
lblse<-paste(lblse, "%", sep=" ")
colors= c("yellow", "red", "black")
pie(pbs$Profit, labels =lblse, main = "Percentage of Profits by Segment")

#Regression
Regression<- lm(SampleSuperstore$Profit~ SampleSuperstore$Sales+ SampleSuperstore$Quantity + SampleSuperstore$Discount)
summary(Regression)
Inference7<- c("we observe from the regression results that Sales, Quantity sold and Discount are statistically significant in determining the levels of profit the store earns, hence it is very important that the store looks at the estimations and corrections that are stated sofar and run the store accordingly since small changes in Sales, Qunatity and Discount play a major role in fluctuating the profits that the firm earns.")

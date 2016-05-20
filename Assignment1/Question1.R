#Loading Libraries
library(HSAUR2) 
library(plyr)
data("Forbes2000",package="HSAUR2")

#1.1
companies.US<-subset(Forbes2000,country=="United States")
companies.UK<-subset(Forbes2000,country=="United Kingdom")
companies.FR<-subset(Forbes2000,country=="France")
companies.GR<-subset(Forbes2000,country=="Germany")
#Calculating median for the four countries
median.US<-median(companies.US$profits,na.rm=TRUE)
median.UK<-median(companies.UK$profits,na.rm=TRUE)
median.FR<-median(companies.FR$profits,na.rm=TRUE)
median.GR<-median(companies.GR$profits,na.rm=TRUE)
#Displaying the median for each countries
cat("Median profit for companies in US(in billions) = ", median.US)
cat("Median profit for companies in UK(in billions) = ", median.UK)
cat("Median profit for companies in FR(in billions) = ", median.FR)
cat("Median profit for companies in GR(in billions) = ", median.GR)


#1.2
#Getting German companies with negative profit
negative.GR<- companies.GR[companies.GR$profits < 0, c("name")]
negative.GR

# 1.3
#Counting category with hightest number of companies
companies.BR <- subset(Forbes2000,country=="Bermuda")
category.freq <- count(companies.BR, vars=c("category"))
category.max <- category.freq[category.freq$freq == max(category.freq$freq),]
cat("Most of the Bermuda island companies belong to the category, ",as.character(category.max$category))

# 1.4 
companies.profit<-Forbes2000[order(-Forbes2000$profits),]
profit.50<-companies.profit[1:50,]
plot(log(sales) ~ log(assets), data=profit.50)
with(high50ProfitCompanies, text(log(sales) ~ log(assets), labels = abbreviate(high50ProfitCompanies$country, minlength = 2,method = "both.sides"), pos = 4))

# 1.5 
# Calculating average values of sales for each company
average.sales <- aggregate(sales ~ country, Forbes2000, mean)
average.sales

five.billion <- Forbes2000[Forbes2000$profits>5,]
five.billion <- na.omit(five.billion)

company.count <- count(five.billion, vars=c("country"))
company.count

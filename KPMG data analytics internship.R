library(readr)
library(tidyr)
library(readxl)
library(tibble)
library(naniar)
library(dplyr)
library(epitools)
library(openxlsx)
library(lubridate)
library(XLConnect)
library(tidyverse)
library(car)

# transactions ------------------------------------------------------------
transactions <- read_xlsx("C:/Users/Asus/Desktop/KPMG Virtual Internship/KPMG_VI_New_raw_data_update_final.xlsx", sheet=2, skip=1)

transactions$transaction_date <- as.Date(transactions$transaction_date)
transactions$product_first_sold_date <- as.Date(transactions$product_first_sold_date)

transaction_date <- ymd(transactions$transaction_date)
which(is.na(transaction_date)) 

product_first_solddate <- ymd(transactions$product_first_sold_date)
which(is.na(product_first_solddate))

transactions$`Margin Type` <- transactions$`Margin Type` %>% factor(levels=c("Very Low", "Low", "Medium","High","Very High"),
                                                                    labels=c("Very Low", "Low", "Medium","High","Very High"))

dim(transactions) #20000 by 13 variables 
str(transactions)
glimpse(transactions)

colSums(is.na(transactions))
sum(is.na(transactions))
which(is.na(transactions))
which(is.na(transactions$online_order))

gg_miss_case(transactions)
gg_miss_var(transactions)

miss_scan_count(transactions, search = "NA")

transactions <- transactions[complete.cases(transactions),]
dim(transactions)

20000 - 19445

t(sapply(transactions, range))
str(transactions$transaction_date)

transaction_cust <- left_join(transactions, cust_demo, by="customer_id")
transaction_cust <- na.omit(transaction_cust)
dim(transaction_cust)

margintype_purchase <- transaction_cust %>% group_by(`Margin Type`) %>% 
  summarize(sum=sum(past_3_years_bike_related_purchases))

barplot(margintype_purchase$sum~ margintype_purchase$`Margin Type`, xlab="Product Margin Type",
         ylab="Bike related purchases ($)", main="Total purchases per product margin type", ylim=c(0, 300000))


# new customer list -------------------------------------------------------
new_cust <- read_xlsx("C:/Users/Asus/Desktop/KPMG Virtual Internship/KPMG_VI_New_raw_data_update_final.xlsx", sheet=3, skip=1)
str(new_cust)

new_cust <- new_cust[,-c(19:23)]

df <- data.frame(Date = new_cust$DOB)
as.Date(df$Date, "%m/%d/%Y")

new_cust_dob <- mdy(new_cust$DOB)
which(is.na(new_cust_dob)) 

new_cust <- new_cust[-(984:1001),]

#62 229 327 361 363 377 437 442 577 601 667 754 778 838 886 907 987

#new1 <- new_cust[c(60, 227, 325, 359, 361, 375, 435, 440, 575, 599, 665, 752, 776, 836, 884, 905, 985),]

#new_cust <- new_cust[-c(60, 227, 325, 359, 361, 375, 435, 440, 575, 599, 665, 752, 776, 836, 884, 905, 985),]

colSums(is.na(new_cust))
sum(is.na(new_cust))
which(is.na(new_cust))

miss_scan_count(new_cust, search="n/a")

new_cust <- new_cust %>% replace_with_na_at(.vars="job_industry_category",
                                            condition = ~.x == "n/a")

new_cust <- new_cust[complete.cases(new_cust),]
dim(new_cust)

hist(as.numeric(new_cust$property_valuation))

# customer demographic ----------------------------------------------------
cust_demo <- read_xlsx("C:/Users/Asus/Desktop/KPMG Virtual Internship/KPMG_VI_New_raw_data_update_final.xlsx", sheet=4, skip=1)

cust_demo_dob <- mdy(cust_demo$DOB)
df2 <- which(is.na(cust_demo_dob)) 
new3 <- cust_demo[df2,]

df1 <- data.frame(Date = cust_demo$DOB)
new2 <- data.frame(as.Date(df1$Date, "%m-%d-%Y"))

cust_demo$DOB <- new2
cust_demo$DOB <- na.omit(cust_demo$DOB)
cust_demo <- na.omit(cust_demo)


#cust_demo <- cust_demo %>% replace_with_na_at(.vars= "job_industry_category", condition = ~.x =="n/a")

cust_demo <- cust_demo[,-11]
str(cust_demo)
colSums(is.na(cust_demo))
sum(is.na(cust_demo))
which(is.na(cust_demo))

dim(cust_demo)

cust_demo <- cust_demo %>% mutate(age= (as.Date("2017-12-31") - ymd(new2$as.Date.df1.Date....m..d..Y..)) / 365.25 ) 
?round

cust_demo %>% filter(gender == "M")

# Chi square test of association ------------------------------------------
cust_purchasesbyindustry <- cust_demo %>% group_by(job_industry_category) %>%  
  summarize(sum=sum(past_3_years_bike_related_purchases))  

data.frame(cust_purchasesbyindustry)
cust_purchasesbyindustry %>% prop.table()

# table_cust_purchase_industry <- cust_purchasesbyindustry %>% spread(key=job_industry_category, value = sum) %>% prop.table()

industry <- c("Argiculture", "Entertainment", "Financial Services", "Health", "IT", "Manufacturing", "Property","Retail", "Telecommunications")
obs <- c(4875, 5317, 30727, 25431, 6024, 32377, 11181, 13866,2371)
table1 <- data.frame(category=rep(industry,obs))

table(table1$category) %>% prop.table()

100/9
?seq

chi1 <- chisq.test(table(table1$category), p=c(1/9,1/9,1/9,1/9,1/9,1/9,1/9,1/9,1/9))
chi1

cust_purchasesbyage$sum %>% qqPlot(dist="norm", main="QQ Plot of Purchases")
cust_purchasesbyage$age %>% qqPlot(dist="norm", main="QQ plot of Age")

cust_purchasesbyage <- cust_demo %>% select(age, past_3_years_bike_related_purchases) 
  
cust_purchasesbyage$age <- round(cust_purchasesbyage$age,0) 

cust_purchasesbyage <- cust_purchasesbyage  %>% group_by(age) %>% 
  summarize(sum=sum(past_3_years_bike_related_purchases)) %>% arrange(desc(age))

cust_purchasesbyage$age <- as.numeric(cust_purchasesbyage$age)

barplot(cust_purchasesbyage$sum~cust_purchasesbyage$age, main="Purchases by age", xlab="Age",
        ylab="Bicycle related purchases for last 3 years", ylim =c(0,8000))

scatterplot(cust_purchasesbyage$sum ~ cust_purchasesbyage$age, log="xy", main="Purchases by Age",
            ylab="Bicycle related purchases for last 3 years", xlab="Age")

barplot(cust_purchasesbyindustry$sum ~ cust_purchasesbyindustry$job_industry_category,
        xlab="Industry", ylab="Purchases in dollars ($)", main="Total purchases by industry")

?scatterplot

gender_car <- table(cust_demo$gender, cust_demo$owns_car)
barplot(gender_car, beside=TRUE, main = "Male and Female ownership of cars", 
        legend=row.names(gender_car), args.legend=c(x="bottom",horiz=FALSE))

purchase_owncar <- table(cust_demo$past_3_years_bike_related_purchases, cust_demo$owns_car)


cap <- function(x){
  quantiles <- quantile( x, c(.05, 0.25, 0.75, .95 ) )
  x[ x < quantiles[2] - 1.5*IQR(x) ] <- quantiles[1]
  x[ x > quantiles[3] + 1.5*IQR(x) ] <- quantiles[4]
  x
}

cust <- cust_demo %>% select(past_3_years_bike_related_purchases)
cust_sub <- as.data.frame(sapply(cust_purchasesbyage, FUN= cap))
summary(cust)


mean(cust_purchasesbyage$sum)

t.test(cust_purchasesbyage, mu=2541)

cor(cust_purchasesbyage)

table(cust_demo$past_3_years_bike_related_purchases, cust_demo$owns_car) %>% prop.table()
 

# customer address --------------------------------------------------------
cust_addr <- read_xlsx("C:/Users/Asus/Desktop/KPMG Virtual Internship/KPMG_VI_New_raw_data_update_final.xlsx", sheet=5, skip=1)
str(cust_addr)

colSums(is.na(cust_addr))
sum(is.na(cust_addr))
which(is.na(cust_addr))

cust_addr$state <- cust_addr$state %>% factor(levels=c("New South Wales", "NSW", "VIC", "Victoria", "QLD"),
                                              labels=c("New South Wales", "New South Wales", "Victoria", "Victoria", "Queensland"))


cust_addr <- cust_addr[complete.cases(cust_addr),]
dim(cust_addr)

write.xlsx(transactions, file="filename.xlsx", sheetName="sheet1", row.names=FALSE)
write.xlsx(new_cust, file="filename.xlsx", sheetName="sheet1", row.names=FALSE)
write.xlsx(cust_demo, file="filename2.xlsx", sheetName="sheet1", row.names=FALSE, colNames = TRUE)
write.xlsx(cust_addr, file="filename1.xlsx", sheetName="sheet1", row.names=FALSE)

write.xlsx(new1, file="new1.xlsx", sheetName="sheet1", row.names=FALSE)
write.xlsx(new3, file="new2.xlsx", sheetName="sheet1", row.names=FALSE)

cust <- left_join(cust_addr, cust_demo)
colSums(is.na(cust))
which(is.na(cust))


# transformation ----------------------------------------------------------
hist(cust_demo$past_3_years_bike_related_purchases)


log_cust_purchases <- log(cust_demo$past_3_years_bike_related_purchases)
hist(log_cust_purchases)

new_cust$past_3_years_bike_related_purchases <- as.numeric(new_cust$past_3_years_bike_related_purchases)
log_newcust_purchase <- log(new_cust$past_3_years_bike_related_purchases)
hist(log_newcust_purchase)

sqrt_cust_purchases <- sqrt(cust_demo$past_3_years_bike_related_purchases)
hist(sqrt_cust_purchases)

sqrt_newcust_purchase <- sqrt(new_cust$past_3_years_bike_related_purchases)
hist(sqrt_newcust_purchase)

cust_age <- (today() - ymd(new2$as.Date.df1.Date....m..d..Y..)) / 365.25

hist(as.numeric(cust_age), main="Histogram of customer age", xlab="Age")

ggplot(cust_demo, aes(x= past_3_years_bike_related_purchases)) + geom_line(stat="count")

table <- cust_demo %>% select(past_3_years_bike_related_purchases) %>% table()
table %>% prop.table()

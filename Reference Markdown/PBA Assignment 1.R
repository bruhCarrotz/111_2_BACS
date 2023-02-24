# Find out the path to the directory containing the data and load it onto R
online_retail <- read.csv("G:/My Drive/111_2_BACS/Reference Markdown/online_retail.csv", header=TRUE)
head(online_retail)
str(online_retail) 

# Convert InvoiceDate to date class and subset the data
online_retail$NewDate <- strptime(as.character(online_retail$InvoiceDate), "%m/%d/%y")
online_retail$NewDate[261324]
class(online_retail$NewDate)

online_retail$NewInvoiceDate <- format(online_retail$NewDate, "%Y-%m-%d")
online_retail$NewInvoiceDate[261324]

subset_df <- online_retail[online_retail$NewInvoiceDate >= "2011-07-01" & online_retail$NewInvoiceDate <= "2011-08-31", ]
subset_length <- length(subset_df$InvoiceNo)

# Check to see 3,664 unique transactions
length(unique(subset_df$InvoiceNo))

# Compute the mean of Quantity and UnitPrice
quantity_average = 0
unit_average = 0
for (i in 1: subset_length) {
  quantity_average <- quantity_average + subset_df$Quantity[i]
  unit_average <- unit_average + subset_df$UnitPrice[i]
}

quantity_average = quantity_average/subset_length
unit_average = unit_average/subset_length

print(quantity_average)
print(unit_average)


# Determine types of each column
for (i in 1: length(subset_df)) {
  print(class(subset_df[,i]))
}

# Compute the number of unique values in each column
for (i in 1: length(subset_df)) {
  print(length(unique(subset_df[,i])))
}

# Subset the data from U.K., Netherlands, and Australia
country_df <- subset_df[subset_df$Country == 'United Kingdom' | subset_df$Country == 'Netherlands' | subset_df$Country == 'Australia' , ]
length(unique(country_df$Country))

# Report the average and standard deviation of UnitPrice
x = mean(country_df$UnitPrice)
format(round(x, 3), nsmall = 3)

y = sd(country_df$UnitPrice)
format(round(y, 3), nsmall= 3)

# Report the number of unique transactions in these countries
length(unique(country_df$InvoiceNo))

# Report how many customers making transaction in these countries
length(unique(country_df$CustomerID))-1

# How many customers made a refund? (exclude NA)
subset_df$Refund <- substr(subset_df$InvoiceNo, 1, 1)
cust_refund <- subset(subset_df, subset_df$Refund == "C")
length(unique(cust_refund$CustomerID))-1

# Total sales amount for NA
subset_df$Sales <- subset_df$Quantity*subset_df$UnitPrice
custNA <- subset(subset_df, is.na(subset_df$CustomerID) == TRUE)
sum(custNA$Sales)

# Number of transaction made by NA
unique(custNA$InvoiceNo) |> length()

# Crete a variable containing the monthly aggregate spending for each customer
july_trans <- subset_df[subset_df$NewInvoiceDate >= "2011-07-01" & subset_df$NewInvoiceDate <= "2011-07-31", ]
agg_cust_july <- tapply(X = july_trans$Sales, INDEX = july_trans$CustomerID, FUN = sum)
aug_trans <- subset_df[subset_df$NewInvoiceDate >= "2011-08-01" & subset_df$NewInvoiceDate <= "2011-08-31", ]
agg_cust_aug <- tapply(X = aug_trans$Sales, INDEX = aug_trans$CustomerID, FUN = sum)

# Report the IDs and monthly purchase amount of 5 customers who spent the most in July 2011
agg_cust_july <- sort(-agg_cust_july)
head(agg_cust_july*(-1),5)

agg_cust_aug <- sort(-agg_cust_aug)
head(agg_cust_aug*(-1),5)
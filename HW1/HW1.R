

#Reading data from .txt file
customers <- read.table("G:/My Drive/111_2_BACS/HW1/customers.txt", header = TRUE)
ages <- customers$age
ages #print ages

#Q1
ages[5]

#Q2
sorted_ages <- sort(ages)
sorted_ages <- unique(sorted_ages)
sorted_ages[5]

#Q3
sorted_ages[1:5]

#Q4
decrease <- unique(sort(ages, decreasing=TRUE))
decrease[1:5]

#Q5
average <- mean(ages)
average

#Q6
std_dev <- sd(ages)
std_dev

#Q7
age_diff <- ages - average
age_diff

#Q8
mean(age_diff)

#Q9
hist(ages) #Q9(a)
plot(density(ages)) #Q9(b)
boxplot(ages, horizontal = TRUE) #Q9(c)
stripchart(ages, method="stack", add=TRUE)






####### HW1: Question No. 8 ####### 

#8a
library(ISLR)
college <- read.csv("C:/Users/Prachi Vijay/AppData/College.csv")
college <- na.omit( college )

#8b
rownames <- college[, 1]

View(college)

college <- college[, -1]
View(college)

#8c(i)
summary(college)

#8c(ii)
college$Private <- as.factor(college$Private)
pairs(college[ ,1:10])

#8c(iii)

boxplot(Outstate~Private, data = college,
        xlab = "Private University", ylab = "Out of State Tuition in USD",
        main = "Outstate vs Private Tuition Boxplot")


#8c(iv)
Elite <- rep("No", nrow(college))
Elite[college$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college$Elite <- Elite
summary(college$Elite)
plot(college$Elite, college$Outstate, xlab = "Elite University", ylab ="Out of State", main = "Outstate vs Elite Plot")

#8c(v)
par(mfrow = c(2,2))
hist(college$Apps, col = 2, xlab = "Applicants", ylab = "Count")
hist(college$Accept, col = 3, xlab = "Accepted Students", ylab = "Count")
hist(college$Grad.Rate, col = 4, xlab = "Graduation Rate", ylab = "Count")
hist(college$perc.alumni, col = 6, xlab = "Percentage Alumni", ylab = "Count")

#8c(vi)
summary(college$PhD)
acceptance_rate <- college$Accept/college$Apps
summary(college$Accept/college$Apps)
summary(college$Grad.Rate)
plot(college$Elite, college$PhD, xlab = "Elite University", ylab ="Percent of faculty with Ph.D.’s", main = "Elite vs % Ph.D Faculty Plot")
plot(college$Elite, college$Grad.Rate, xlab = "Elite University", ylab ="Graduation Rate", main = "Elite vs Grad Rate Plot")
plot(college$Room.Board, college$Books, 
     xlab = "Room and Board Costs", 
     ylab = "Estimated Book Costs",
     main = "Relationship Between Room/Board and Book Costs")
plot(college$S.F.Ratio, college$Grad.Rate, 
     xlab = "Student/Faculty Ratio", 
     ylab = "Graduation Rate",
     main = "Relationship Between Student/Faculty Ratio and Graduation Rate")
plot(college$Expend, college$Outstate, 
     xlab = "Instructional Expenditure per Student", 
     ylab = "Out-of-State Tuition",
     main = "Relationship Between Expenditure and Out-of-State Tuition")
counts <- table(cut(college$Grad.Rate, breaks = c(0, 50, 70, 100)), college$F.Undergrad, college$P.Undergrad)
mean_expend <- tapply(college$Expend, cut(college$Grad.Rate, breaks = c(0, 50, 70, 100)), mean)
barplot(mean_expend, 
        xlab = "Graduation Rate", 
        ylab = "Mean Instructional Expenditure per Student",
        main = "Comparison of Expenditure by Graduation Rate")
##Summery: 
#Average acceptance rate of the colleges is around 74%
#Average graduation rate of the given colleges is around 65%
#Elite universities have higher amount of Ph.D. professors as compared to other universities
#Elite universities have slightly higher Graduation rate as compared to other universities but the spread of Graduation rate is almost similar
#The cost of Books is almost constant whereas the Room/Board cost varies
#The Instructional Expenditure increases linearly with Out-Of-State Tuition
#Mean Instructional Expenditure per student increases with the Graduation rate


####### HW1: Question No. 9 ####### 
auto <- read.csv("Auto.csv", na.strings = "?")
auto <- na.omit(auto)

#9a: 
str(auto)
# Ans(9a): All variables except "name" are quantitative.

#9b
range(auto$cylinders)
range(auto$displacement)
range(auto$horsepower)
range(auto$weight)
range(auto$acceleration)
range(auto$year)
range(auto$origin)


#9c
sapply(auto[ ,1:8], mean)
sapply(auto[ ,1:8], sd)

#9d
subset <- auto[-c(10:85), -c(9)]
sapply(subset, range)
sapply(subset, mean)
sapply(subset, sd)

#9e
auto$name <- as.factor(auto$name)
pairs(auto[ ,1:8])
plot(auto$mpg ~ auto$cylinders, xlab = "Cylinders", ylab = "Mileage")
plot(auto$weight ~ auto$cylinders, xlab = "Cylinders", ylab = "Weight")
boxplot(auto$acceleration ~ auto$cylinders, xlab = "Cylinders", ylab = "Acceleration")
barplot(table(auto$origin, auto$cylinders),
        beside = TRUE,             # Show bars beside each other
        col = c("red", "green", "blue"),   # Assign colors to 'cylinders' categories
        legend.text = TRUE,        # Show legend text
        xlab = "Origin",           # X-axis label
        ylab = "Count",            # Y-axis label
        main = "Relationship between Origin and Cylinders")
plot(auto$weight, auto$mpg,
     xlab = "Weight",
     ylab = "Gas Mileage")
plot(auto$mpg, auto$year)

#Summary 9e:
#Mileage decreases with the increse in number of cylinders
#Weight increases as the number of cylinders increases
#Acceleration is the highest for the average number of cylinders. It starts declining after 5 cylinders
#Origin 1 has the highest number of cylinders as compared to Origin 2 and 3
#Origin 1,2 and 3 mostly prefer 4 cylinders. 

#9f
correlation_matrix <- cor(auto[, c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "year")])
ggplot(data = melt(correlation_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "Correlation Heatmap Between Parameters",
       x = "Parameters",
       y = "Parameters")
cor(auto$mpg, auto$horsepower)
cor(auto$mpg, auto$displacement)
cor(auto$mpg, auto$weight)
cor(auto$mpg, auto$acceleration)
plot(auto$mpg, auto$origin,
     xlab = "Gas Mileage",
     ylab = "Origin")
#The heatmap shows the correlation between parameters. 
#From the correlation between variables, it can be inferred that the gas Mileage is inversely proportional to Horsepower, Displacement, Weight.
#Gas Mileage is increases with the increase in acceleration.
#The Gas Mileage tend to increase with upcoming years. 
#Gas Mileage for Origin 1 is lower as compared to Origin 2 and 3. 
#Hence factors including Year, Acceleration, Displacement, Weight, Horsepower, Country of Origin are useful in predicting Gas Mileage. 


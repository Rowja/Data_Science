mydata <- read.csv("C:/Heart.csv", header = TRUE, sep = ",")
mydata

names(mydata)

mydata$Sex <- factor(mydata$Sex,
                     levels = c("M","F"),
                     labels = c(1,0))
mydata

missing_values <- colSums(is.na(mydata))
missing_values
mydata <- na.omit(mydata)
mydata
summary(mydata)
Age_summary <- table(mydata$Age)
print(Age_summary)
sex_summary <- table(mydata$sex)
print(sex_summary)
str(mydata)
new_row <- data.frame(  Age = 25,  Sex = 1,  ChestPainType = "NAP",  RestingBP = 120,  Cholesterol = 200,  FastingBS = 0,  RestingECG = "Normal",  MaxHR = 150,  ExerciseAngina = "N",  Oldpeak = 0.5,  ST_Slope = "Flat",  HeartDisease = 0)
mydata <- rbind(mydata, new_row)
mydata
mydata

median_age <- median(mydata$Age, na.rm = TRUE)
mydata$Age[mydata$Age > 100 | is.na(mydata$Age)] <- median_age
mydata

mean_age <- mean(mydata$Age, na.rm = TRUE)
median_age <- median(mydata$Age, na.rm = TRUE)
Mode <- function(x) {  
  ux <- unique(x)  
  ux[which.max(tabulate(match(x, ux)))]
}
mode_age <- Mode(mydata$Age)
cat("Mean Age:", mean_age, "\n")
cat("Median Age:", median_age, "\n")
cat("Mode Age:", mode_age, "\n")

numerical_columns <- sapply(mydata, is.numeric)
par(mfrow = c(3, 3))
for (col in names(mydata)[numerical_columns]) {  
  hist(mydata[[col]], main = paste("Histogram of", col), xlab = col, col = "lightblue", border = "black")
}

categorical_columns <- sapply(mydata, is.factor)
par(mfrow = c(3, 3))
for (col in names(mydata)[categorical_columns]) {  
  category_counts <- table(mydata[[col]])  
  barplot(category_counts, main = paste("Bar Graph of", col), xlab = col, ylab = "Frequency", col = rainbow(length(category_counts)))
}


numerical_columns <- sapply(mydata, is.numeric)
par(mfrow = c(3, 3))
for (col in names(mydata)[numerical_columns]) {  
  boxplot(mydata[[col]], main = paste("Boxplot of", col), xlab = col, col = "lightblue", border = "black")
}
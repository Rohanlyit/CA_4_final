# Reading the dataset
dat <- read.csv("C:/Users/dell/ca3_R/Health.csv", header = TRUE, stringsAsFactors = FALSE)
str(dat)
View(dat)

# Clearing all the dirty data
dat[dat == "0"] <- NA
dat[dat == "**"] <- NA
dat[dat == "~"] <- NA
dat[dat == "*"] <- NA
# Removing the null values
dat <- na.omit(dat)
View(dat)
str(dat)
# Dropping the category column
df2 <- subset(dat, select = -c(Category))
View(df2)
str(df2)
# Changing the datatypes of the columns
df2$TotalDischargesMale <- as.numeric(gsub(",","",df2$TotalDischargesMale))
df2$TotalDischargesFemale <- as.numeric(gsub(",","",df2$TotalDischargesFemale))
df2$Year <- as.numeric(df2$Year)
df2$TotalDischargesMale.Lessthan15yearsage. <- as.numeric(gsub(",","",df2$TotalDischargesMale.Lessthan15yearsage.))
df2$TotalDischargesMale.15to44yearsage. <- as.numeric(gsub(",","",df2$TotalDischargesMale.15to44yearsage.))
df2$TotalDischargesMale.45to64yearsage. <- as.numeric(gsub(",","",df2$TotalDischargesMale.45to64yearsage.))
df2$TotalDischargesMale.65yearsandover. <- as.numeric(gsub(",","",df2$TotalDischargesMale.65yearsandover.))
df2$TotalDischargesFemale.Lessthan15yearsage. <- as.numeric(gsub(",","",df2$TotalDischargesFemale.Lessthan15yearsage.))
df2$TotalDischargesFemale.15to44yearsage. <- as.numeric(gsub(",","",df2$TotalDischargesFemale.15to44yearsage.))
df2$TotalDischargesFemale.45to64yearsage. <- as.numeric(gsub(",","",df2$TotalDischargesFemale.45to64yearsage.))
df2$TotalDischargesFemale.65yearsandaboveage. <- as.numeric(gsub(",","",df2$TotalDischargesFemale.65yearsandaboveage.))
df2$TotalDischarges <- as.numeric(gsub(",","",df2$TotalDischarges))
str(df2)

# Making a subset for time-series
df_time <- subset(df2, select = c(TotalDischargesMale, TotalDischargesFemale, Year))
View(df_time)
str(df_time)


# Renaming the columns 
colnames(df2)[1] <- "Diseases"
colnames(df2)[2] <- "Discharge_Male_Less_than_15_years_age"
colnames(df2)[3] <- "Discharge_Male_Fifteen_to_45_years_age"
colnames(df2)[4] <- "Discharge_Male_FourtyFive_to_64_years_age"
colnames(df2)[5] <- "Discharge_Male_SixtyFive_years_above_age"
colnames(df2)[7] <- "Discharge_Female_Less_than_15_years_age"
colnames(df2)[8] <- "Discharge_Female_Fifteen_to_45_years_age"
colnames(df2)[9] <- "Discharge_Female_FourtyFive_to_64_years_age"
colnames(df2)[10] <- "Discharge_Female_SixtyFive_years_above_age"
View(df2)
df2[,13] = NULL
View(df2)
df2[,6] = NULL
View(df2)
df2[,10] = NULL
View(df2)
df2[,15] = NULL
View(df2)
df2[,10] = NULL
View(df2)
df2[,10] = NULL
View(df2)
df2[,10] = NULL
View(df2)
df2 = df2[-1,]
head(df2)
View(df2)
df2 = df2[-1,]
View(df2)

# Applying the multiple linear model to find the relation between total discharges and remaining other columns
# The bivariate correlations are provided by the cor() function,
# and scatter plots are generated from the scatterplotMatrix() 
# Fitting the model
model <- lm(TotalDischarges ~ Discharge_Male_Less_than_15_years_age + Discharge_Male_Fifteen_to_45_years_age + Discharge_Male_FourtyFive_to_64_years_age + Discharge_Male_SixtyFive_years_above_age + Discharge_Female_Less_than_15_years_age + Discharge_Female_Fifteen_to_45_years_age + Discharge_Female_FourtyFive_to_64_years_age + Discharge_Female_SixtyFive_years_above_age, data = df2)
summary(model)
summary(model)$coefficient
sigma(model)/mean(df2$TotalDischarges)

new_model <- as.data.frame(df2[, c("TotalDischarges", "Discharge_Male_Less_than_15_years_age", "Discharge_Male_Fifteen_to_45_years_age", "Discharge_Male_FourtyFive_to_64_years_age", "Discharge_Male_SixtyFive_years_above_age", "Discharge_Female_Less_than_15_years_age", "Discharge_Female_Fifteen_to_45_years_age", "Discharge_Female_FourtyFive_to_64_years_age", "Discharge_Female_SixtyFive_years_above_age")])
cor(new_model)

library(car)
help(scatterplotMatrix)
scatterplotMatrix(new_model, spread = FALSE, smoother.args = list(lty = 2),
                  main = "Scatter Plot Matrix")


#library(car)
#install.packages("effects")
#library(effects)
par(mfrow = c(2, 2))
plot(model)
#install.packages("gvlma")

# Global Validation linear model is applied by assuming it to be well satisfied
library(gvlma)
gvmodel <- gvlma(model)
summary(gvmodel)
qqPlot(model, labels = row.names(new_model), id.method = "identify",
       simulate = TRUE, main = "Q-Q Plot")

# Creating train and test data for the discharge dataset
set.seed(200)
no_of_records <- sample(1:nrow(new_model), 0.8 * nrow(new_model))
training_data <- new_model[no_of_records,]
training_data
testing_data <- new_model[-no_of_records,]
testing_data
# Building model on training data
lr_model <- lm(Discharge_Male_Less_than_15_years_age ~ Discharge_Female_Less_than_15_years_age, data = training_data)
summary(lr_model)
#  Predicting distance from testing data
discharge_predicted <- predict(lr_model, testing_data)
actuals_preds <- data.frame(cbind(actuals = testing_data$Discharge_Male_Less_than_15_years_age, 
                                  predicted = discharge_predicted))
head(actuals_preds)
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy
# Min - max accuracy
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy
# MAPE
mape <- mean(abs((actuals_preds$predicted - actuals_preds$actuals)) / actuals_preds$actuals)
mape
#install.packages("reshape")
# Exploratory Analysis
library(reshape)
df_time1 <- melt(df_time, id = c("Year"))
head(df_time1)
tail(df_time1)
library(ggplot2)
ggplot(data = df_time1, aes(x = Year)) + geom_line(aes(y = value, colour = variable)) +
  scale_colour_manual(values = c("blue", "red"))
excess_frac <- (df_time$TotalDischargesMale - df_time$TotalDischargesFemale)/df_time$TotalDischargesFemale
excess_ts <- ts(excess_frac, frequency = 1, start = df_time$Year[1])
library(ggplot2)
#install.packages("ggfortify")
library(ggfortify)
autoplot(excess_ts)
#install.packages("forecast")
#install.packages("astsa")
#install.packages("lmtest")
#install.packages("fUnitRoots")
#install.packages("FitARMA")
#install.packages("strucchange")
#install.packages("Rmisc")
#install.packages("fBasics")
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(forecast))
suppressPackageStartupMessages(library(astsa))
suppressPackageStartupMessages(library(lmtest))
suppressPackageStartupMessages(library(fUnitRoots))
suppressPackageStartupMessages(library(FitARMA))
suppressPackageStartupMessages(library(strucchange))
suppressPackageStartupMessages(library(reshape))
suppressPackageStartupMessages(library(Rmisc))
suppressPackageStartupMessages(library(fBasics))

# Running t.test to verify if there is a true difference 
# in the mean of two groups 
t.test(value ~ variable, data = df_time1)
basicStats(df_time[-1])

# Observing auto-correlation at lag = 10
# Suggests the presence of yearly component
par(mfrow=c(1,2))
acf(excess_ts)
pacf(excess_ts)
summary(lm(excess_ts ~ 1))
# Verifying if there are any structural breaks
break_point <- breakpoints(excess_ts ~ 1)
plot(break_point)
summary(break_point)
plot(excess_ts)
lines(fitted(break_point, breaks = 1), col = 4)
lines(confint(break_point, breaks = 1))
# Discharge ratio between male and female
fitted(break_point)[1]
fitted(break_point)[length(excess_ts)]

library(FitARMA)
# Fitting the ARIMA model
# Model_1 perfectly fits the data after diagonisis 
model_1 <- auto.arima(excess_ts, stepwise = FALSE, trace = TRUE)
checkresiduals(model_1)
# Purpose of LjungBoxTest is to verify if any auto-correlation beyond the confidence region
fun <- function(n) {Box.test(rnorm(n),type="Ljung-Box",lag=log(n))$p.value}
LjungBoxTest(residuals(model_1), k = 2, lag.max = 10)
sarima(excess_ts, p = 1, d = 1, q = 1)
h_fut <- 10
# Plotting the forecast for the discharges
plot(forecast(model_1, h = h_fut, xreg = rep(1, h_fut)))

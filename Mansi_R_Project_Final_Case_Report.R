#Reading csv file
dat1<- read.csv(file= "C:/Users/Rajas/Downloads/CaseStudyDataset.csv", header=T, sep=",")
dat1

head(dat1)
tail(dat1)

# Scatter plot graph
plot(dat1$Years, dat1$Weeks_SP, xlab="Years", ylab="Weeks SP")

#Mean of Weeks and Years
mean_weeks <-mean(dat1$Weeks_SP)
mean_weeks
mean_years <-mean(dat1$Years)
mean_years

#Standard Deviation of Weeks and Years
std_dev_weeks<- sd(dat1$Weeks_SP)
std_dev_weeks

std_dev_years<- sd(dat1$Years)
std_dev_years

fit<- lm(Weeks_SP ~ Years, data=dat1)
summary(fit)

# Calculating critical value and interval (Lower_Limit & Upper Limit)
n <- dim(dat1)[1]
CV <- qt(1-0.05/2, n-2)
L<- mean_weeks- CV*(std_dev_weeks/sqrt(n))
round(L,2)
U<- mean_weeks+ CV*(std_dev_weeks/sqrt(n))
round(U,2)

#Confidence Interval and Prediction Interval
newdata1<- data.frame(Years=10)

pred.clim1 <- predict(fit, newdata1, interval="confidence")
pred.clim1

pred.plim1 <- predict(fit, newdata1, interval="prediction")
pred.plim1



##########################################################################################
##
##Interval Estimation and hypothesis test of population difference when sigma1 and sigma2 is known
##
##########################################################################################
n1 <- 120
n2 <- 80
xbar1 <- 275
xbar2 <- 258
sigma1 <- 12.5
sigma2 <- 9.25

diff <- xbar1 -xbar2
diff

alpha <- 0.05
CV <- qnorm(1-alpha/2)
CV

SE <- sqrt(sigma1^2/n1+sigma2^2/n2)
SE

L <- diff - CV*SE
L

U <- diff + CV*SE
U

####Hypothesis Test on mu1 - mu2 #######
Dzero <- 0

z_obs <- (diff - Dzero)/SE
z_obs 

pvalue1 <- 1-pnorm(z_obs) #P(Z > Zobs)
pvalue1

or 

pvalue2 <- pnorm(z_obs) #P(Z< Zobs)
pvalue2

pvalue3<- 2*(1-pnorm(z_obs)) #or pnorm(z_obs)

alpha<-0.01
z_alpha<-qnorm(1-0.01)

###########################################################################################
##
##Interval Estimation and hypothesis test of population difference when sigma1 and sigma2 is unknown
## Replace z with t and use sample std deviations
##the standard error (SE) of x-bar-1 - x-bar-2 when sigma1 and sigma2 are unknown.
################################################################
n1 <- 24
n2 <- 28
xbar1 <- 29.8
xbar2 <- 27.3
s1 <- 2.56
s2 <- 1.81

diff1<-xbar1-xbar2

se <- sqrt(s1^2/n1 + s2^2/n2)     ## standard error
se

#calculate the degrees of freedom (df) as well 

Numertator <- (s1^2/n1 + s2^2/n2)^2
Denominator <- (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1)
df <- floor(Numertator/Denominator)   ## degrees of freedom
df

alpha <- 0.1
CV <- qt(1-alpha/2, df)    
CV

L <- diff1 - CV*SE
L

U <- diff1 + CV*SE
U



#Simple Regression
#Regression two or more independent variables is multiple regression
#Simple Linear Regression: y= B0+B1x+E..where B0 is y-intercept 
#and B1 is slope of regression line
#Least Square Criterion - min summation (yi- est y)^2

x <- c(1, 3, 2, 1, 3)
y <- c(14, 24, 18, 17, 27)

## produce scatter plot

plot(x, y, xlab="# of TV ads", ylab = "# of Cars sold", 
     main= " Scatter plot of number of TV ads and number of card sold",
     col="blue")

fit <- lm(y~x)

summary(fit)

fit$coef


###########################################
## code for Example: Effect of Advertisement on Sales


x<- c(12,8,10,13,7,8,10,6,9,11)*100
y<- c(101,92,110,120,90,82,93,75,91,105)*1000
x
y
plot(x,y)

fit1<-lm(y~x)
summary(fit1)

fit1$coef

newdata1<-data.frame(x=850)
predict(fit1, newdata1, se.fit=TRUE, interval="confidence") #average sales
predict(fit1, newdata1, interval="prediction") #sales

newdata2 <- data.frame(x=c(850, 900))
predict(fit1, newdata2, se.fit=T)

pred.clim2 <- predict(fit1, newdata2, interval="confidence")
pred.clim2

pred.plim2 <- predict(fit1, newdata2, interval="prediction")
pred.plim2

#Chp_15
#Least_Squares_Criterion: min Summation (yi-y hat i)^2
#t-test is used to determine whether each of the individual independentvariables is signif-referred
#to as test of significance

year<- c(4,7,1,5,8,10)

score<-c(78,100,86,82,86,84)

salary<- c(24.0,43.0,23.7,34.3,35.8,38.0)

dat<-data.frame(year,score,salary)

fit<-lm(salary~year+score, data=dat)
summary(fit)
###################################################################
stackloss.lm = lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.,data=stackloss) 
#We also wrap the parameters inside a new data frame named newdata. 
newdata = data.frame(Air.Flow=72,  # wrap the parameters 
                     +     Water.Temp=20, 
                     +     Acid.Conc.=85) 
#Lastly, we apply the predict function to stackloss.lm and newdata. 
predict(stackloss.lm, newdata) 
#O/P:1 
24.582 


round(qf(1-0.05,2,12),2)

?qf()

##################################################
##### ANOVA #####

wax<-c(35,25,37,40,33,34,35,38,27,31,30,35,35,37,38)
type<-c(rep("50",5),rep("60",5),rep("70",5))
AutoShine<-data.frame(wax,type)

results<-aov(wax~type, data= AutoShine)
summary(results)

1-pf(0.254,2,12)

####2.Observational Study######

plant<-c(48,54,57,54,62,73,63,66,64,74,51,63,61,54,56)
type<-c(rep("Buffalo",5),rep("Pittsburg",5),rep("Detroit",5))
data1=data.frame(plant,type)
results.ex2<-aov(plant~type,data=data1)
summary(results.ex2)

round(qf(1-0.05,2,12),2)

1-pf(0.939,2,12)

#### Randomized Block Design ######

Gasoline<-c(31,30,29,33,26,30,29,29,31,25,30,29,28,29,26)
type<- c(rep("X",5),rep("Y",5),rep("Z",5))
Automobile<- factor(rep(1:5,times=3))

Crescent=data.frame(Gasoline,Automobile,type)
results.ex3<-aov(Gasoline~type+Automobile,data=Crescent)
summary(results.ex3)

z<-qnorm(1-0.025)
z

1-pt(4.003, 40)

sqrt(7.316)

1-pnorm(1.85)
###################################################
# MAtched sample
UPX <- c(6, 15, 19, 26, 2, 16, 31, 14, 15, 16)
INTEX <- c(-3, 15, 28, 18, 32, 31, 15, 12, 10, 15)

d <- UPX - INTEX
d

mean(d)
sd(d)
n <- length(d)
t_obs <- (mean(d)-0)/(sd(d)/sqrt(n))
t_obs

## p-value approach
pvalue <- 2*pt(abs(t_obs), n-1, lower.tail=F)
pvalue

(sd(d)/sqrt(n))

## Critical Value approach 
alpha <- 0.10
CV <- qt(1-0.05, 9)
CV


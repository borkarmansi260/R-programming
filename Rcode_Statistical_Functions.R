
dat<- read.csv(file="C:/Users/mansi/Downloads/SpeedData.csv",header=T,sep=",")
print(dat)

#Question 2.Mean_Value(Mean_V)
Mean_V<-round(mean(dat$Speeds),digits=1)
print(Mean_V)

#Question 4:Standard Deviations(Std_Dev_Speed)
Std_Dev_Speed<-round(sd(dat$Speeds),digits=1)
print(Std_Dev_Speed)

#Question 3:Chebysheff's Theorem:percentage of cars have speeds between 55.6 miles per hour and 94 miles per hour? 
#Rounded your answer to 2 decimal points, and then provide final answer as percentage.

z1=(55.6-Mean_V)/Std_Dev_Speed
print(z1)
z2=(94-Mean_V)/Std_Dev_Speed
print(z2)
ans<-round((1-1/z1^2)*100,digits=2)
print(ans)


#Question 5:Inter-Quartile Range(IQR_V)
Q3<-quantile(dat$Speeds,0.75,type=6)
print(Q3)
Q1<-quantile(dat$Speeds,0.25,type=6)
print(Q1)
IQR_V<-Q3-Q1
print(IQR_V)

#OR direct function

IQR_V<-IQR(dat$Speeds)
print(IQR_V)

#Question 6:z-score (rounded to 1 decimal point) of the speed of the 7th car in the sample

val<-(dat$Speeds[7])
print(val)
z7<-round((val-Mean_V)/Std_Dev_Speed,digits=1)
print(z7)



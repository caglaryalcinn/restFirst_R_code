#####Q1

####a
x<-rnorm(100,0,1)
hist(x)

####b

hist(y, breaks = 15)
y<-rnorm(100,4,1.5) ##var is 2.25 so sd is 1.5

hist(y, breaks = 5)

hist(y, breaks = 10)


##calculate the manual
z=matrix()
z=matrix()
for (i in seq(y)) {
  z[i]=(y[i]-mean(y))/sd(y)
}

##calculate to scale function
t=scale(y)

all(z==t)
##true they are identical


mean(z)
var(z)
#mean converges to 0
#variance is 1
#so distibuted standart normal


#####Q2

print(possible_exactly5<-dbinom(5,20,0.25))
print(possible_fewerthan3<-sum(dbinom(0:2,20,0.25)))
print(possible_atleast3<-sum(dbinom(3:20,20,0.25)))
print(possible_between5and7<-sum(dbinom(5:7,20,0.25)))

###If we sum possibility of  house holds who are fewer than 3 wireless-only to
#possibility of  house holds who are fewer than 3 wireless-only, it would be equal to 1.
#This means that this two  possiblities include all possiblities in this binomial dist.

##Possibility of  house holds who are exactly 5 wireless-only  in 
#20 households who is randomly selected in all U.S house holds == 0.2023312

##Possibility of  house holds who are fewer than 3 wireless-only  in 
#20 households who is randomly selected in all U.S house holds == 0.09126043

##Possibility of  house holds who are at least 3 wireless-only  in 
#20 households who is randomly selected in all U.S house holds == 0.9087396

##Possibility of  house holds who are between 5 and 7 wireless-only  in 
#20 households who is randomly selected in all U.S house holds == 0.4833466

##smalllest possiblity is possiblilty of house holds who are fewer than 3 wireless-only( '0.09126043' so close 0) and 
#biggest possiblity is possiblilty of house holds who are at least 3 wireless-only(' 0.9087396' so close 1)
#possiblity is possiblilty of house holds who are between 5 and 7 wireless-only almost half to half(0.4833466)
#and this possiblty bigger than possibility of  house holds who are exactly 5 wireless-only('0.2023312')






#####Q3

##this distrubution passion distrubition and lambda is 1.32 per 100 million.

lamda=1.32

##fatalities occur at the rate of 1.32 fatal accidents per 100 million
#miles.Hence lamda is 1.32 per 100 million so possiblities are:


possible_exactly_zero_fatal_accidents=dpois(0,1.32)
atleast_two_fatal_accidents=1-sum(dpois(0:1,1.32))
more_than_one_fatal_accident=1-sum(dpois(0:1,1.32))
# possiblity of at least two fatal accidents and possiblity more than one fatal accident is equal.





#####Q4


#record value
v <- c(781,501,1342,2883,149,543,1692,730,1038,451,1889,480,1291,87,1783,2324,453,1826,580,
       1664,507,798,2186,2823,1446,1348,0,1064,261,673,398,1676,3082,3001,2909,2978,540,2862,
       526,4148)
array(v)


n<-length((v))
summary(v)
range(v)
meanv<-mean(v)
medianV<-median(v)
maxv<-max(v)
minv<-min(v)

k<-sqrt(40) 
floor(k)  

e<-(max(v)-min(v))/floor(k)
classwidth<-ceiling(e)    #so our class width is 692
limit<-seq(0,4148,692)
merkez<-seq(30+692/2,4148-692/2,692)
sinif<-cut(v,limit)
print(datagrouped<-table(sinif))
datagroupednew<-data.frame(datagrouped)
datagroupednew$merkez<-merkez
datagroupednew$kumulatif<-cumsum(datagroupednew$Freq)
datagroupednew$relatifFrekans<-datagroupednew$Freq/sum(datagroupednew$Freq)
datagroupednew$relatifKumulatif<-datagroupednew$kumulatif/sum(datagroupednew$Freq)
print(datagroupednew)
transform(table(datagroupednew))
boxplot(v)

###out liers must not be between (Q1-1.5*(IQR))=-1641,8 and (Q3-1.5*(IQR))=4126,2,
#so there is a 1 outlier whic is 4148 because 4148>4126,2
#this distirubitions is rigt skewed.
#median of dis data is 1177,5 and mean of dis data 1392.8
#both of them are similar,both of them can decribe our data.there is an outlier so median decribe our data better than mean.






























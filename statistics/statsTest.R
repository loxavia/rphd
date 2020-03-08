#Stats Test

#all distributions----
?distribution
#cumulative distribution : pxxxx
#density : dxxxxx
#quantile : qxxxx
#random  : rxxxxx
options(scipen =99)
#binomial----
#bin1----
?rbinom
#MCQ : 1 out 5 correct: prob of ans correctly = 1/5 = .2
#probability of answering 4 or less correct answers: 12 questions

dbinom(4, size=12, prob=.2)  #exactly 4 - 0.13

pbinom(q=4, size=12, prob=.2)  # 4 or less - .927

n=12 #trails
x=4 #no of success desired
p=.2; q=1-.2 #prob of success : q=1-.2 = .8
(P4L <- dbinom(x=0, size=12, prob=.2) + dbinom(x=1, size=12, prob=.2) + dbinom(x=2, size=12, prob=.2) +  dbinom(x=3, size=12, prob=.2) + dbinom(x=4, size=12, prob=.2))
#b2
dbinom
dbinom(x=15, size=18, prob=.85)  #exactly 4 - 0.13

#poission ------

#avg no of success = 35/10;
dpois(x=35, lambda = 3)

#----
dpois(x=10, lambda = 3)
(3 ^ 10) * (2.71 ^ -3) / factorial(10)

#lamdba=12 cars/min; > 17 cars
(ps1 <- ppois(q=16, lambda = 12))  #exactly 16 cars
(1 - ps1)
ppois(q=16, lambda = 12, lower=F)  #upper tail

#t-distribution
#find 2.5th & 97.5 % of t-dist with df-5
qt(c(.025, .975), df=5)
#2 sided

#95th % of F distribution df(5,2)
qf(.95, df1=5, df2=2)
#single sided

#hypothesis tests ------------------------------





#--------------------------------------------
#(P0 = (factorial(12)/(factorial(0) * factorial(12-0))) * ((.2 ^ 12) * (.8 ^ 0)))
(P4 = (factorial(12)/(factorial(4) * factorial(12-4))) * ((.2 ^ 12) * (.8 ^ (12 - 4))))

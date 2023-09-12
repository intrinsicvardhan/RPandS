chest<- c(rep('gold',20), rep('silver', 30), rep('bronze',50))

? sample()

sample(chest, size = 10, replace = FALSE)

outcome<- c('SUCCESS', 'FAILURE')

for (i in 1:10) {
  cat(sample(outcome,size = 1, replace = F, prob<- c(0.9,0.1)), "\n")
}
#function for finding mode of a data
mode<- function(v) {
  uniqv<- unique(v)
  matched<- match(v,uniqv)
  print(matched)
  count<-tabulate(matched)
  #count<-tabulate(v)
  #print(count)
  index<- which.max(count)
  #print(index)
  return (uniqv[index])
}
v<- c(2,1,3,5,1,2,2,3,4,5,1)
cat(mode(v))

#Question 4
data<-iris
iris[1:4,]
head(data)
tail(data) 

str(data)
range(data$Sepal.Length)
round (mean(data$Sepal.Length),3)
median(round(data$Sepal.Length),3)
q1 = quantile(data$Sepal.Length, 0.25)
q3 = quantile(data$Sepal.Length, 0.75)
iqr = q3-q1
xd = sd(data$Sepal.Length)

#Question 3 
?sample
cp<-function(p1, p2, p3) {
  #probability of p1 given p2 is =
  #bayes' theorem 
  #p1(probability of p1 given p2)*p2/p3
  return (p1*p2/p3)
}
p_rain<-0.2
p_cloudy<-0.4
p_cloudyWHENrain<-0.85

cat("The probability of rain when cloudy is ", cp(p_cloudyWHENrain, p_rain, p_cloudy))

#Question 2 
#Birthday paradox
#A room has n people and each has an equal chance of being born on any of the 365 days
#What is the probability of two of them having the same birthday
# = 1-p(none of them having same birthday)
#p(none of them having the same birthday) = 
# (i=1 to n)(365-n+1)/365

birthday_paradox<-function(n) {
  prob<-1.0
  n_values<-c(1:n)
  for (i in n_values) {
    prob = prob*(365-i+1)/(365)
  }
  prob = 1-prob
  return (prob)
}

birthday_paradox(23)

n_values<-c(2:365)

probabilities<-sapply(n_values, birthday_paradox);

plot(n_values, probabilities, type = "p", main = "Birthday Paradox", xlab = "Number of persons", ylab = "Probability")



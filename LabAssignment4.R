#Lab_Assignment4
#The probability distribution of X, the number of imperfections
#per 10 meters of a synthetic 
n_values<-  c(0:4)
p_values<-c(0.41, 0.37, 0.16, 0.05, 0.01)
#Find the average number of imperfections -> expectation
?weighted.mean()
e_of_X<-weighted.mean(p_values, n_values)
cat("The average number of imperfections per 10 metres = ",e_of_X)


#Question 2
?integrate
??functions
pdf<-function(t){
  0.1*exp(-0.1*t)
}
expected_value<-integrate(function(t)t*pdf(t), lower = 0, upper = Inf)$value
cat("The expected value of T is ", expected_value)

#Question3
#Net revenue
p_values<-c(0.1, 0.2, 0.2, 0.5)
n_values<-c(0:3)
expected_revenue_sales<-sum(n_values*12*p_values)
expected_cost_returns<-sum((3-n_values)*2*(1-p_values))

expected_net_revenue<-expected_revenue_sales-expected_cost_returns
cat("The expected net revenue is: ", expected_net_revenue)

#Question 4
#first moment is expectation
#second moment is expectation of X^2
pdf<-function(x) {
  0.5*exp(-abs(x))
}
first_moment<-integrate(function(x)x*pdf(x), lower = 1, upper = 10)$value
second_moment<-integrate(function(x)x^2*pdf(x), lower = 1, upper = 10)$value
Mean<-first_moment
Variance<-second_moment - first_moment^2
cat("The variance and mean of the given function is: ", Variance, Mean)

#Question 5
pdf<-function(x) {
  return ((0.75*(0.25)^(x-1)))
}
X<-c(1:5)
probabilities<-pdf(X^2)
cat("The probabilities are ",probabilities)
Mean<-sum(X^2*probabilities)
Variance<-sum(X^4*probabilities)-Mean^2

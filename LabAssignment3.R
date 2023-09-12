#LabAssignment3

#Roll 12 dies simultaneously and let X denote the number of 6's 
#that appear

#Calculate the probability of getting 7,8 or 9, 6's using R


#Question 1
?pbinom 

n<-12

p<-1/6

prob_6<-pbinom(6, size = n, prob = p)
prob_9 <-pbinom(9, size = n, prob = p)

prob_7_to_9 = prob_9-prob_6

#Question 2
#WHat is the percentage of people scoring 84 or more
#1-percentage of people scoring less than 84
mean_score<-72
st_dev<-15.2
x_i <- 84
?pnorm
prob_less_84<-pnorm(x_i, mean_score, st_dev)
cat("The percentage of people scoring more than 84 is: ", (1-prob_less_84)*100)


#Question 3
p_50<-dpois(50,lambda = 50)
p_48<-dpois(48,lambda = 50)
p_49<-dpois(49,lambda =50)

p_required<-p_50+p_49+p_48

#Question_4
# seventeen defective processors in 250 pentium processors
#a Quality consultant randomly collects 5 processors for inspection
p_success<-17/250
n<-5
#prob_less_3 <-pbinom(3,size = n,prob = p_success)
#prob_less_2<-pbinom(2, size = n, prob = p_success)
#prob_3<- prob_less_3-prob_less_2
prob_3<-dbinom(3, size = n, prob=p_success)
cat("The probability of getting exactly 3 defectives is ",prob_3)

#Question_5
#Approximately 44.7 percent of the college students have used wiki
#X equals the number of students in a random sample size of size n = 31
#who have used wikipedia
n<-31
n_values<-c(0:31)
p_success<-0.447
probabilities<-dbinom(n_values, size = n, prob = p_success)
plot(n_values, probabilities, type = "p", main ="Distribution", xlab = "Number of persons", y_lab = "Probabilities")
cdf_probabilities<- pbinom(n_values, size = n, prob = p_success )
plot(n_values, cdf_probabilities, type = "p", main = "CDF", x_lab = "Number of persons", y_lab = "CDF")
mean_distr<-n*p_success
variance_distr<-n*p_success*(1-p_success)

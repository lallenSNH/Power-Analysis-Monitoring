## Power analysis exploration with Lucy
# 30 January 2019

## Packages ----
library(pwr)


### Defining questions -----

# Are restoration interventions successful in stabilising the annual water table
# fluctuations, increasing sphagnum cover and positive indicator species presence 
# and reducing bare peat? 

# q1: Is there an increase in the water table after restoration?
# q2: Is the a decrease in the variance of the water table with increased time after restration?
# q3: Is there an increase in sphagnum cover after restoration?
# q4: Is there a reduction in the area of area bare peat visible 2(?) years after restoration?
# (compared to before restoration - time for change different depending on type of bog)


### Sphagnum ----
### Hypotheses ----

# H0 = There is no increase in sphagnum cover after peat restoration
# H1 = There is an increase in sphagnum cover after peat restoration


### Key components ----

# What is the purpose of the study and the main endpoint? 
# - Sphagnum is an indicator of bog wetness, and we want to know if sphagnum has increased, 
# indicating successful restoration.  

# What is the minimum difference we are interested in detecting?
# - 100% (double the sphagnum cover)

# What is expected in the control group (mean and SD)
# - range from 0-20, mean 10, sd = 7
sph <- c(0,5,10,15,20,10,10,20,25,0,5,10,10,15,5)
sd(sph)
mean(sph)

# How certain do you want to be of detecting a difference (power)?
# 80%

# n = number of observations per group
# delta = true difference in means (minimum significant difference)
# sd = standard deviation in the data
# sig.level = significance level (probablility of a false positive, usually p = 0.05)
# power = power of test (1 - probablity of false negative (not detecting a real difference))

### Power analysis formula ----

power.t.test(n=NULL, delta=10, sd=7.2, sig.level=0.05, power=0.95,alternative = "one.sided")




##////////////////////
### Water table ----
### Hypotheses ----

# H0 = There is no change in the water table after peat restoration
# H1 = There is a change in the water table after peat restoration


### Key components ----

# What is the purpose of the study and the main endpoint? 
# To see if the bog has become wetter 

# What is the minimum difference we are interested in detecting?
# 5

# What is expected in the control group (mean and SD)
# - range from 20-40cm, mean 32, sd = 9.6
hyd <- c(20,18,19,22,30,33,30,32,35,37,40,22,45,50,44,43,35,32) 
sd(hyd)
mean(hyd)

# How certain do you want to be of detecting a difference (power)?
# 95%

# n = number of observations per group
# delta = true difference in means (minimum significant difference)
# sd = standard deviation in the data
# sig.level = significance level (probablility of a false positive, usually p = 0.05)
# power = power of test (1 - probablity of false negative (not detecting a real difference))

### Power analysis formula ----

power.t.test(n=NULL, delta=10, sd=9.6, sig.level=0.05, power=0.95, alternative = "one.sided") 


##!!!!!!********* For next time: ----
## check Penny Anderson veg data to try and see what range of sphagnum found before restoration 
## check hydrology data - for raised and blanket bog - to see mean and standard deviation of data
## Identify sample unit for hydrology.
## Need to see what power analysis would be required for linear models or other statistical test,
## instead of just t-test.

## Linear model power analysis
# package pwr

var <- 0.2 ## variance explained by model
out <- pwr.f2.test(u = 6, v = NULL, f2 = var/(1-var), sig.level = 0.05, power = 0.95)
n <- out$v+out$u+1
n+(n*0.3) # sample size needed, including a 30% loss of samples

## example of possible model coefficients?
# lm(stability of water table ~ time since restoration,location, type of bog,elevation)

# lm(water table ~ time + dist from nearest drain + elevation + slope + type of bog + rainfall) 
 ## would it be worth including any of these as random effects? how do we account for restoration activities?

# u	= degrees of freedom for numerator (the number of coefficients you’ll have in your model (minus the intercept)
# v = degrees of freedomfor denominator (the number of error degrees of freedom: v=n−u−1. This implies n=v+u+1.)
# f2 = effect size (For example, if I think my model explains 45% of the variance in my dependent variable, 
# the effect size is 0.45/(1 - 0.45) ≈ 0.81.)
# sig.level	= Significance level (Type I error probability)
# power	= Power of test (1 minus Type II error probability)

# Recall n=v+u+1. Therefore we can us the power analysis to calculate v, and knowing u we can 
# calculate the sample size needed 



### HAg erosion monitoring - power analysis ----

# Question/subject - What is the impact of restoration works on peat erosion rates over the first three years post restoration? 
#   
# Design - Control (unrestored) vs Impact (restored), over 1 - 3 years post restoration. Multiple sample points on one site with replicates across the country this should (to some extent) eradicate the variables (altitude, region, peatland type). 
# 
# Parameter - Hag erosion rate, loss of cm/mm over 3 years post restorations works, one measure taken mid-summer once per year. 
# 
# What change are we bothered about, what difference between the control (unrestored) point and impact (restored) point is important? - This I am still trying to narrow down in the literature but we would assume there to be a large change in the figures, that's the whole idea of re-profiling. In which case we could go for a high percentage, so if the impact (restored) point was loss 70% less than the control (unrestored) that would be important to us. 

# What is the purpose of the study and the main endpoint? 
# to understand if reprofiling affects the rate of hag erosion.

# What is the minimum difference we are interested in detecting?
# maybe 20cm?

# What is expected in the control group (mean and SD)
# - range from 0-20, mean 10, sd = 7
hagerosion <- c(10,20,25,30,20,25,40,30,40,25,15,12,22,29,50,34,60,102,20,33,41)
sd(hagerosion)
mean(hagerosion)

# How certain do you want to be of detecting a difference (power)?
# 80%

# n = number of observations per group
# delta = true difference in means (minimum significant difference)
# sd = standard deviation in the data
# sig.level = significance level (probablility of a false positive, usually p = 0.05)
# power = power of test (1 - probablity of false negative (not detecting a real difference))

### Power analysis formula ----

power.t.test(n=NULL, delta=20, sd=20.1, sig.level=0.05, power=0.95,alternative = "one.sided")

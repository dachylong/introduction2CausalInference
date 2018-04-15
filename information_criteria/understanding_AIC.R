
### Understanding how to derive the AIC
# refer: https://www.r-bloggers.com/how-do-i-interpret-the-aic/

### 1. Explaining the likelihood -------------------------------
set.seed(126)
n = 50       # sample size
a = 5
sdy = 3
y = rnorm(n = n, mean = a, sd = sdy)
hist(y)

# model 1: Linear Normal
m1 = glm(formula = y~1, family = "gaussian")
m1                      
names(m1)
sm1 = summary(m1)
sm1$dispersion          

# the likelihood of a new sample 7
mu_est = coef(m1)                         # mu_est = 4.384
sd_est = sqrt(sm1$dispersion)             # dispersion = variance = 5.91
dnorm(x = 7, mean = mu_est, sd = sd_est)

# the likelihood of all ys: visualization
plot(x = y, y = dnorm(y, mu_est, sd_est), xlab = "y", ylab = "Likelihood")

# the total likelihood (in log formation)
y_lik = dnorm(x = y, mean = mu_est, sd = sd_est, log = TRUE)
sum(y_lik)


### 2. Model comparison with likelihoods --------------------------
a = 5; b = 3
n = 100
x1 = rnorm(n)
x2 = rnorm(n)
sdy = 1
y = a + b*x1 + rnorm(n, sd = sdy)

par(mfrow = c(1,1))
plot(x1, y)
plot(x2, y)

# different models
m.1 = glm(y ~ x1)
m.2 = glm(y ~ x2)
m.3 = glm(y ~ x1 + x2)

plot(x1, y)
lines(x1, predict(m.1))

# log-likelihoods
sm.1 = summary(m.1)
log_lik.1 = dnorm(x = y, mean = predict(m.1), sd = sqrt(sm.1$dispersion), log = TRUE)
log_lik.1 = sum(log_lik.1)

sm.2 = summary(m.2)
log_lik.2 = dnorm(x = y, mean = predict(m.2), sd = sqrt(sm.1$dispersion), log = TRUE)
log_lik.2 = sum(log_lik.2)

sm.3 = summary(m.3)
log_lik.3 = dnorm(x = y, mean = predict(m.3), sd = sqrt(sm.1$dispersion), log = TRUE)
log_lik.3 = sum(log_lik.3)

data.frame(log_lik.1 = log_lik.1, log_lik.2 = log_lik.2, log_lik.3 = log_lik.3)   # m.3 has the largest


### 3. AIC as a measure of parsimony ----------------------
aic = function(log_lik, num_theta){
  return(-2 * log_lik + 2*num_theta)
}

aic.1 = aic(log_lik = log_lik.1, num_theta = 1)
aic.2 = aic(log_lik = log_lik.2, num_theta = 1)
aic.3 = aic(log_lik = log_lik.3, num_theta = 2)

data.frame(aic.1, aic.2, aic.3)




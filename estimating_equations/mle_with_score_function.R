
### Data from geometric distribution ----
# f(y) = (1-p)^y * p
#      = (theta^y) / (1 + theta)^(1+y)
#
# E[y]=(1-p)/p  = theta

rm(list = ls())
theta = 4.0
p = 1/(1+theta)      # success probability in one trial = 0.2

n = 50
y = rgeom(n = n, prob = p)
hist(y)


log_lik = function(y, b){          ### log-likelihood of y with parameter b ----
  return(sum(y)*log(b) - sum(y+1)*log(b+1))
}


score = function(y, b){            ### Score function of y with parameter b ----
  sum(y)/b - sum(y+1)/(b+1)           # S(b,y) = d(log_lik(y,b))/db
}


H = function(y, b){                ### Derivative of score function ----
  sum((y+1))/(1+b)^2 - sum(y)/b^2       # H(y,b) = S'(b,y)
}


info = function(y, b){             ### Fisher information ------
  n = length(y)
  n/b - n/(1+b)                   # I(b) = -E[S'(b,y)] = n/b - n/(1+b)
}


log_lik(y = y, b = theta)
score(y = y, b = theta)
H(y = y, b = theta)
info(y = y, b = theta)


### Score estimating equation: Score(b) = 0 ----
theta_mle = sum(y)/n; theta_mle    # 4.48


### Newton-Raphson method -------------------
s = c()
s[1] = 1e-10
for (j in 1:n){
  s[j+1] = s[j] - H(y, s[j])^-1 * score(y, s[j])
}

plot(x = 1:51, y = s)


### Fisher-scoring method -------------------
d = c()
d[1] = 0.01
for (t in 1:n){
  d[t+1] = d[t] + info(y, d[t])^-1 * score(y, d[t])
}

plot(x = 1:51, y = d)
head(d)


### comparisons and Plot ----
beta = seq(0, 5, 0.01)
comp = data.frame(IniVal1e_10 = 1e-10, IniVal0.01 = 0.01, IniVal2 = 2.0, 
                  IniVal3 = 3.0, IniVal4 = 4.0, IniVal5 = 5.0, IniVal6 = 6.0, IniVal7 = 7.0)

par(mfrow = c(1,2))
plot(x = beta, y = score(y,beta), type = "l", ylim = c(-100, 1500), 
     xlab = "beta", ylab = "S(beta)", main = "Newton-Raphson")
for (k in 1:n){
  points(x = s[k], y = score(y,s[k]))
}

plot(x = beta, y = score(y,beta), type = "l", ylim = c(-100, 1500), 
     xlab = "beta", ylab = "S(beta)", main = "Fisher-scoring")
for (k in 1:n){
  points(x = d[k], y = score(y,d[k]))
}

result = data.frame(step = seq(1,n+1), newton = s, fisher_scoring=d)
head(result)



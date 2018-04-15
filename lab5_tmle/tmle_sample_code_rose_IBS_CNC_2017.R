
## Load library -------
library(tmle)
set.seed(2018)


## Generate simulated data ---------------------------
# X1=Gender; X2=Therapy; X3=Antidepressant use
N = 1000
X1 = rbinom(N, 1, prob = 0.55)
X2 = rbinom(N, 1, prob = 0.30)
X3 = rbinom(N, 1, prob = 0.25)
W = cbind(X1, X2, X3)              # N x 3

# Treatment = regular phisical exercise (binary)
A = rbinom(N, 1, prob = plogis(-0.5 + 0.75*X1 + 1*X2 + 1.5*X3))

# Outcome = CES-D score  
Y = 24 - 3*A + 3*X1 - 4*X2 - 6*X3 - 1.5*A*X3 + rnorm(N, mean = 0, sd = 4.5)
ATE.true = -3 -1.5*0.25                      # ATE = -3.375
ATE.empirical = -3 - 1.5*mean(X3)            # 
ATE.naive = mean(Y[A==1]) - mean(Y[A==0])    # naive = -5.943


## Specify a library of algorithms --------
SL.library = c("SL.glm", "SL.step.interaction", "SL.glmnet", "SL.randomForest", "SL.gam", "SL.rpart")


## TMLE approach: super learning ----------------
tmleSL1 = tmle(Y = Y, A = A, W = W, Q.SL.library = SL.library, g.SL.library = SL.library)
tmleSL1       
ATE.tmle = tmleSL1$estimates$ATE             # ATE_TMLE
ATE.tmle$psi                                 # -3.313   (ATE = -3.375)

## TMLE approach: GLM, MT misspecification of outcome -------
# misspecified outcome regression: Y ~ A + X1 + X2 + X3
tmleGLM1 = tmle(Y = Y, A = A, W = W, Qform = Y~A+X1+X2+X3, gform = A~X1+X2+X3)
tmleGLM1       
ATE.tmle.GLM1 = tmleGLM1$estimates$ATE             # ATE_TMLE
ATE.tmle.GLM1$psi                                  # -3.375   (ATE = -3.375)

## TMLE approach: GLM, OV misspecification of outcome --------
# misspecified outcome regression: Y~A+X1+X2
tmleGLM2 = tmle(Y = Y, A = A, W = W, Qform = Y~A+X1+X2, gform = A~X1+X2+X3)
tmleGLM2      
ATE.tmle.GLM2 = tmleGLM2$estimates$ATE             # ATE_TMLE
ATE.tmle.GLM2$psi                                  # -3.295   (ATE = -3.375)

## TMLE approach: GLM, OV misspecification of treatment --------
# misspecified treatment regression: A~X1+X2
tmleGLM3 = tmle(Y = Y, A = A, W = W, Qform = Y~A+X1+X2+X3+A:X3, gform = A~X1+X2)
tmleGLM3      
ATE.tmle.GLM3 = tmleGLM3$estimates$ATE             # ATE_TMLE
ATE.tmle.GLM3$psi                                  # -3.321    (ATE = -3.375)


as.data.frame(list(ATE.true = ATE.true, 
                   ATE.naive = ATE.naive, 
                   ATE.SL = ATE.tmle$psi,
                   ATE.GLM1 = ATE.tmle.GLM1$psi,
                   ATE.GLM2 = ATE.tmle.GLM2$psi,
                   ATE.GLM3 = ATE.tmle.GLM3$psi))



#### Lasso Regression Code ####
# Install and load the required package if not already installed
# install.packages("glmnet")

#Import Data
Large <- read.csv("~/Desktop/Larges.csv") 
Medium <- read.csv("~/Desktop/Mediums.csv") 
Small <- read.csv("~/Desktop/Smalls.csv") 

# Convert your data to a matrix
#Full Model
X <- model.matrix(Win.. ~ Total.Payroll + Catcher.. + Infield.. + Outfield.. + Pitcher.. + CBT.Space + X..players.on.arb + X..players.aquired.via.trade + X..resigned.extended + resigned.extended...spent + X..free.agents.signed + free.agent...spent + Batter.Average.Age + Pitcher.average.age + X..Rookies, data = Large)

#Large Market
##Win Percentage
X <- model.matrix(Win..~ Catcher..+Pitcher..+CBT.Space+X..resigned.extended+X..free.agents.signed+free.agent...spent+Pitcher.average.age, data=Large)
y <- Large$Win..

##Playoff Results
X <- model.matrix(Where.did.they.lose~ Infield..+Outfield..+Pitcher..+CBT.Space+X..resigned.extended+free.agent...spent+Batter.Average.Age+X..Rookies, data=Large)
y <- Large$Where.did.they.lose

#Medium Market
##Win Percentage
X <- model.matrix(Win..~ Total.Payroll+Catcher..+Pitcher..+X..resigned.extended+resigned.extended...spent+Batter.Average.Age+X..Rookies, data=Medium)
y <- Medium$Win..

##Playoff Results
X <- model.matrix(Where.did.they.lose~  Infield..+Outfield..+Pitcher..+CBT.Space+Pitcher.average.age+X..Rookies, data=Medium)
y <- Medium$Where.did.they.lose

#Small Market
##Win Percentage
X <- model.matrix(Win..~ Total.Payroll+Infield..+Outfield..+Pitcher..+X..players.aquired.via.trade+X..Rookies, data=Small)
y <- Small$Win..

##Playoff Results
X <- model.matrix(Where.did.they.lose~ Total.Payroll+Infield..+Outfield..+X..players.aquired.via.trade+resigned.extended...spent+X..Rookies, data=Small)
y <- Small$Where.did.they.lose




# Run Lasso regression
cv_lasso_model <- cv.glmnet(X, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_lasso_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_lasso_model) 

best_model <- glmnet(X, y, alpha = 1, lambda = best_lambda)
coef(best_model)

#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = X)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq

# Calculate MSE
mse <- mean((y_predicted - y)^2)
print(mse)

# Calculate adjusted R-squared
n <- length(y)
p <- ncol(X) - 1 # Subtract 1 for the intercept
adj_rsq <- 1 - (1 - rsq) * ((n - 1) / (n - p - 1))
adj_rsq

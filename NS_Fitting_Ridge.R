# Modeling Term Structure
library(YieldCurve)
library(ggplot2)
library(ridge)

data("FedYieldCurve")

cor(FedYieldCurve) #correlation matrix of yields

# Fit NS with YieldCurve package
rate.Fed <- first(FedYieldCurve,'5 months')
maturity.Fed <- c(3/12, 0.5, 1,2,3,5,7,10)

NSParameters <- Nelson.Siegel( rate= rate.Fed, maturity=maturity.Fed )

y <- NSrates(NSParameters[5,], maturity.Fed)
plot(maturity.Fed,rate.Fed[5,],main="Fitting Nelson-Siegel yield curve", type="o")
lines(maturity.Fed,y, col=2)
legend("topleft",legend=c("observed yield curve","fitted yield curve"),
       col=c(1,2),lty=1)

# NS model 
#library(dplyr) -> be careful using first() function with dplyr loaded, it doesn't work
nelson_siegel_calculate<-function(lambda,tau,beta0,beta1,beta2){
  beta0 + beta1*(1-exp(-lambda*tau))/(lambda*tau) + beta2*((1-exp(-lambda*tau))/(lambda*tau) - exp(-lambda*tau))
}
# Plot the yield curve of NS with the following parameters
maturity <- maturity.Fed
tau <- maturity
lambda <- c(0.5,1,2)
beta0 <- 14.34594
beta1 <- -1.76249
beta2 <- 3.65

ns_data <- data.frame(maturity, ns_yield=nelson_siegel_calculate(tau,lambda,beta0,beta1,beta2))
#head(ns_data)

ggplot() + 
  geom_line(data=df, aes(x=maturity, y=rates, colour="Observed")) + 
  geom_line(data=ns_data, aes(x=maturity, y=ns_yield, colour="Fitted")) + labs(title="Observed vs Fitted Curve")

ggplot(data=ns_data, aes(x=maturity,y=ns_yield)) + geom_point() + geom_line()

rates <- rate.Fed[1,]
rates<-as.vector(rates)
df <- data.frame(maturity.Fed,rates)
ggplot(df, aes(x=maturity.Fed, y=rates)) + geom_point() + geom_line(data=ns_data)

# YieldCurve package


data(FedYieldCurve)

### Fit static NS
n <- 100 # number of periods to fit
rate.Fed <- FedYieldCurve[1:n,]
l <- 1
t_ <- c(3/12, 0.5, 1,2,3,5,7,10)
t <- matrix(t_,nrow=nrow(rate.Fed),ncol=length(t_),byrow=TRUE)
slope <- (1-exp(-l*t))/(l*t)
curvature <- ((1-exp(-l*t))/(l*t)) - exp(-l*t)

slope_single_column <- as.vector(slope)
curvature_single_column <- as.vector(curvature)
rates_single_column <- as.vector(rate.Fed)

NS_OLS <- lm(rates_single_column ~ slope_single_column + curvature_single_column)
summary(NS_OLS)
as.numeric(NS_OLS$coef[2])

# Grid search for lambda
r_sq <- 0
i <- 0
test_lambdas <- seq(.5,20,.5)
for (l in test_lambdas){
  slope <- (1-exp(-l*t))/(l*t)
  curvature <- ((1-exp(-l*t))/(l*t)) - exp(-l*t)
  
  slope_single_column <- as.vector(slope)
  curvature_single_column <- as.vector(curvature)
  rates_single_column <- as.vector(rate.Fed)
  
  NS_gridsearch <- lm(rates_single_column ~ slope_single_column + curvature_single_column)
  print((summary(NS_gridsearch)$r.squared))
  i = i +1
  r_sq[i] <- summary(NS_gridsearch)$r.squared
}
cat("best lambda value is:" ,test_lambdas[which.max(r_sq)]," with an r^2 of",max(r_sq))

### Fit static NS, simple version
y <- c(12.92,	14.28,	13.31,	13.34,	12.71,	13.9,	14.81,	13.83,	13.87,	13.13,	14.32,	14.73,	13.95,	13.98,	13.34,	14.57,	14.82,	14.19,	14.2,	13.78,	14.64,	14.73,	14.13,	14.18,	13.77,	14.65,	14.54,	13.98,	14,	13.75,	14.67,	14.46,	13.93,	13.94,	13.74,	14.59,	14.43,	13.86,	13.87,	13.62)
x1 <- c(0.884807467409305,	0.884807467409305,	0.884807467409305,	0.884807467409305,	0.884807467409305,	0.786956721664576,	0.786956721664576,	0.786956721664576,	0.786956721664576,	0.786956721664576,	0.632146983743356,	0.632146983743356,	0.632146983743356,	0.632146983743356,	0.632146983743356,	0.432362059705898,	0.432362059705898,	0.432362059705898,	0.432362059705898,	0.432362059705898,	0.316764340857747,	0.316764340857747,	0.316764340857747,	0.316764340857747,	0.316764340857747,	0.198671603797386,	0.198671603797386,	0.198671603797386,	0.198671603797386,	0.198671603797386,	0.14274105689049,	0.14274105689049,	0.14274105689049,	0.14274105689049,	0.14274105689049,	0.100005456010361,	0.100005456010361,	0.100005456010361,	0.100005456010361,	0.100005456010361)
x2 <- c(0.105987214074946,	0.105987214074946,	0.105987214074946,	0.105987214074946,	0.105987214074946,	0.180395734660781,	0.180395734660781,	0.180395734660781,	0.180395734660781,	0.180395734660781,	0.264230752788338,	0.264230752788338,	0.264230752788338,	0.264230752788338,	0.264230752788338,	0.296999706705751,	0.296999706705751,	0.296999706705751,	0.296999706705751,	0.296999706705751,	0.26696233412873,	0.26696233412873,	0.26696233412873,	0.26696233412873,	0.26696233412873,	0.191930286982417,	0.191930286982417,	0.191930286982417,	0.191930286982417,	0.191930286982417,	0.141828536384096,	0.141828536384096,	0.141828536384096,	0.141828536384096,	0.141828536384096,	0.0999600106579613,	0.0999600106579613,	0.0999600106579613,	0.0999600106579613,	0.0999600106579613) 

reg <- lm(y ~ x1 + x2 )
summary(reg)
###

# Fit NS with YieldCurve package
maturity.Fed <- c(3/12, 0.5, 1,2,3,5,7,10)

NSParameters <- Nelson.Siegel( rate= rate.Fed, maturity=maturity.Fed )

y <- NSrates(NSParameters[n,], maturity.Fed)
plot(maturity.Fed,rate.Fed[n,],main="Fitting Nelson-Siegel yield curve", type="o")
lines(maturity.Fed,y, col=2)
legend("topleft",legend=c("observed yield curve","fitted yield curve"),
       col=c(1,2),lty=1)

### Svensson function and ECB data-set ###
SvenssonParameters <- Svensson(rate.Fed, maturity.Fed)
Svensson.rate <- Srates( SvenssonParameters ,maturity.Fed,"Spot")
plot(maturity.Fed, rate.Fed[5,],main="Fitting Svensson yield curve", type="o")
lines(maturity.Fed, Svensson.rate[5,], col=2)
legend("topleft",legend=c("observed yield curve","fitted yield curve"),
       col=c(1,2),lty=1)

### Some Bond Functions ###

# Some useful Bond functions

# Bond Price
calculate_bond_price<-function(face_value,coupon_rate,maturity,yearly_coupons){
  #if yearly_coupons == 0, it only pays out at the maturity, ==1 pays annualy, ==2 pays semi-annually
  if(yearly_coupons==0){
    face_value/((1+coupon_rate)^maturity)
  }else{
    face_value/((1+coupon_rate/yearly_coupons)^(yearly_coupons*maturity))
  }
  
}
calculate_bond_price(100,0.0255,10,2) 

# Bond Yield
calculate_yield<-function(bond_price,maturity){ #continuos compounding, price in cents on the dollar
  
  -log(bond_price)/maturity
  
}
calculate_yield(.985,0.5)

# Forward Rate
imply_forward_rate<-function(R0t1,R0t2,t1,t2){
  
  ((1+R0t2)^t2/(1+R0t1)^t1)^(1/(t2-t1)) -1
  
}
imply_forward_rate(0.04,0.045,1,2)

# Zero-coupon prices and yields
get_zero_coupon<-function(coupons,BondPrices,nominal_value){
  
  price_matrix <- matrix(0,nrow=length(coupons),ncol=length(coupons))
  
  for(i in 1:length(coupons)){
    price_matrix[i,1:i] <- coupons[i]
  }
  
  diag(price_matrix) <- diag(price_matrix) + nominal_value
  
  zero_coupon_prices<-solve(price_matrix,BondPrices)
  
  zero_coupon_yields <- (1/zero_coupon_prices)^(1/1:length(coupons))-1
  
  return(list(B0t=zero_coupon_prices,R0t=zero_coupon_yields))
}
get_zero_coupon(c(5,5.5,5,6),c(101,101.5,99,100),100)

# Yield interpolations
yield_interpolate<-approxfun(x=c(3,4),y=c(0.055,0.06)) # linear interpolation
yield_interpolate(3.75)

yield_spline<-splinefun(x=1:4,y=c(0.03,0.05,0.055,0.06)) # cubic spline interpolation
yield_spline(2.5)
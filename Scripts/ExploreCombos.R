# Load Libraries
library(foreign)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(plotly)

# Load the functions
statpt <- function(lm){
  ## Takes a quadratic lm function
  ## coef(lm) gives a vector of all the parameter estimates in the linear model
  ## So here we are grabbing the individual parameter estimates from that vector
  b0 <- as.numeric(coef(lm)[1])
  b1 <- as.numeric(coef(lm)[2])
  b2 <- as.numeric(coef(lm)[3])
  b3 <- as.numeric(coef(lm)[4])
  b4 <- as.numeric(coef(lm)[5])
  b5 <- as.numeric(coef(lm)[6])
  
  ## Stationary Pts. using the formulas in Eq 10 & 11
  x0  <- (b2*b4 - 2*b1*b5)/(4*b3*b5 - b4^2)
  y0  <- (b1*b4 - 2*b2*b3)/(4*b3*b5 - b4^2)
  
  ## Output
  #out <- matrix(c(x0, y0), ncol=2)
  #colnames(out) <- c("x0","y0")
  out <- data.frame(x0=x0, y0=y0)
  return(out)
}
paxis <- function(lm){
  ## Takes a quadratic lm function just as the function before and just as before
  ## coef(lm) gives a vector of all the parameter estimates in the linear model
  ## So here we are grabbing the individual parameter estimates from that vector
  b0 <- as.numeric(coef(lm)[1])
  b1 <- as.numeric(coef(lm)[2])
  b2 <- as.numeric(coef(lm)[3])
  b3 <- as.numeric(coef(lm)[4])
  b4 <- as.numeric(coef(lm)[5])
  b5 <- as.numeric(coef(lm)[6])
  
  ## Stationary Pts.
  x0  <- (b2*b4 - 2*b1*b5)/(4*b3*b5 - b4^2)
  y0  <- (b1*b4 - 2*b2*b3)/(4*b3*b5 - b4^2)
  
  ## First Principal axis
  p11 <- (b5 - b3 + sqrt((b3 - b5)^2 + b4^2))/b4 #slope
  p10 <- y0 - p11*x0 #intercept
  
  ## Second Principal axis
  p21 <- (b5 - b3 - sqrt((b3 - b5)^2 + b4^2))/b4
  p20 <- y0 - p21*x0
  
  ## Output
  out <- data.frame(p10=p10, p11=p11, p20=p20, p21=p21)
  return(out)
}
principal_plot_surface <- function(pax, lm){
  require(ggplot2)
  require(reshape2)
  require(gridExtra)
  x_temp <- seq(-3,3,length.out= 1000)
  y_p1 <- pax$p10 + pax$p11*x_temp
  y_p2 <- pax$p20 + pax$p21*x_temp
  
  x <- x_temp
  y <- y_p1
  pred1 <- predict(lm, data.frame(x,y,x^2,x*y,y^2))
  
  x <- x_temp
  y <- y_p2
  pred2 <- predict(lm, data.frame(x,y,x^2,x*y,y^2))
  
  a <- ggplot() + geom_line(aes(x=y_p1, y=pred1)) + labs(x=paste("y = ", round(pax$p10,2), " + ", round(pax$p11,2), "x"), y="z", title="p1")
  b <- ggplot() + geom_line(aes(x=y_p2, y=pred2)) + labs(x=paste("y = ", round(pax$p20,2), " + ", round(pax$p21,2), "x"), y="z", title="p2")
  grid.arrange(a, b)
}

# Load in Data
d <- read.csv("../../ccData/CurrentData/ModData_6_9.csv", header=TRUE)

# Setup Data
x <- d$OHhostW2A - 5
y <- d$OWhostH2A - 5
z <- d$OHrqW2A - 5

# Plot Histograms
ggplot() + geom_histogram(aes(x=d$HHhostW0))
ggplot() + geom_histogram(aes(x=d$WHhostW0))
ggplot() + geom_histogram(aes(x=d$OHrqW0))

ggplot() + geom_histogram(aes(x=d$OHhostW0))
ggplot() + geom_histogram(aes(x=d$OWhostH0))
ggplot() + geom_histogram(aes(x=d$OHrqW0))


# Create Quad Reg
QF <- lm(z ~ x + y + I(x^2) + I(x*y) + I(y^2))
LF <- lm(z ~ x + y)
CF <- lm(z ~ x + y + I(x^2) + I(x*y) + I(y^2) + I(x^3) + I(x^2*y) + I(x*y^2) + I(y^3))

summary(QF)
anova(LF, QF)
anova(QF, CF)

plot(predict(QF), residuals(QF))

statpt(QF) # Stationary Pt

x0 <- statpt(QF)$x0
y0 <- statpt(QF)$y0
predstatpt <- as.numeric(coef(QF)[1] + coef(QF)[2]*x0 + coef(QF)[3]*y0 + coef(QF)[4]*x0^2 + coef(QF)[5]*x0*y0 + coef(QF)[6]*y0^2)
predstatpt # Pred Response at Stationary Pt

paxis(QF) # Principal Axis

principal_plot_surface(paxis(QF), QF) # Principal Axis Plot

# Predictions
n     <- 100
xp    <- seq(-4,4,length.out= n)
y    <- seq(-4,4,length.out= n)
preds <- matrix(rep(0, n))

for(i in 1:n){
  x <- rep(xp[i], times=n)
  preddat <- data.frame(x, y, x^2, x*y, y^2)
  preds   <- cbind(preds, as.numeric(predict(QF, preddat)))
}
predictions <- list(x=xp,y=y, z=preds[,-1])

p <- plot_ly(x=xp, y=y, z=preds[,-1]) %>% add_surface() # Plotly plot
p

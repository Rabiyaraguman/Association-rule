#import dataset
Toyota <- read_excel("C:/Users/Rafiya/Downloads/Toyota.xlsx")
 View(Toyota)
 str(Toyota)
   attach(Toyota) 
   Corolla <- cbind(Price,Age_08_04,KM,HP,cc,Doors,Gears,Quarterly_Tax,Weight)
   Corolla <- as.data.frame(Corolla)
   class(Corolla)   
   View(Corolla)
   attach(Corolla)   
summary(Corolla)   
plot(Age_08_04,Price)
plot(HP,Price)
corolla.price <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight)
summary(corolla.price)
corolla.price2 <-lm(Price~Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight)
summary(corolla.price2)

### Scatter plot matrix with Correlations inserted in graph
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
   usr <- par("usr"); on.exit(par(usr))
   par(usr = c(0, 1, 0, 1))
   r = (cor(x, y))
   txt <- format(c(r, 0.123456789), digits=digits)[1]
   txt <- paste(prefix, txt, sep="")
   
   if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
   text(0.5, 0.5, txt, cex = cex)
}
pairs(Corolla, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")
library(corpcor)
cor2pcor(cor(Corolla))
library(corpcor)
library(car)


# Deletion Diagnostics for identifying influential variable
influence.measures(corolla.price)
influenceIndexPlot(corolla.price) # Index Plots of the influence measures
influencePlot(corolla.price)# A user friendly representation of the above
?influencePlot

## Regression after deleting the 77th observation
corolla.price1<-lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data=Corolla[-81,])
summary(corolla.price1)


### Variance Inflation Factors
vif(corolla.price)  # VIF is > 10 => collinearity

#### Added Variable Plots ######
avPlots(corolla.price, id.n=5, id.cex=50, col="red")
#install.packages("MASS")
library("MASS")
stepAIC(corolla.price) # backward

plot(corolla.price)

model.final <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data=Corolla)
summary(model.final)


model.final1 <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data=Corolla[-81,])
summary(model.final1)

avPlots(model.final1, id.n=2, id.cex=0.8, col="red")

vif(model.final1)

# Lower the AIC (Akaike Information Criterion) value better is the model. AIC is used only if you build
# multiple models.
library("MASS")
stepAIC(model.final1)

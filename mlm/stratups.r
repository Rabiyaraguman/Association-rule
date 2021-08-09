#import dataset
Data1 <- read.csv("C:/Users/Rafiya/Downloads/50_Startups.csv")
   View(Data1)
  Data1$State <- revalue(Data1$State,
                             c("New York"="0", "California"="1", "Florida"="2")) 
attach(Data1)
Data1 <- cbind(RD_Spend=R.D.Spend,Administration,Marketing_Spend=Marketing.Spend,State,Profit)
Data1 <- as.data.frame(Data1)
attach(Data1) 
summary(Data1)   
plot(Administration,Profit)
plot(State,Profit)
plot(Marketing.Spend,Profit)
cor(Data1)

#linerar model
Model <- lm(Profit~RD_Spend+Administration+Marketing_Spend+State)
summary(Model)
Model1 <- lm(Profit~RD_Spend+log(Administration))
summary(Model1) 

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
pairs(Data1, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")
library(corpcor)
cor2pcor(cor(Data1))
library(car)
influence.measures(Model)
influenceIndexPlot(Model, id.n=3)
influencePlot(Model,id.n=3)

# Logarthimic Transformation 
Model_Log<-lm(Profit~RD_Spend+log(Administration)+Marketing_Spend+log(State),data=Data1[-c(49,50),]) 
summary(Model_Log)

confint(Model_Log,level=0.95)
predict(Model_Log,interval="predict")
Fin1<-lm(Profit~RD_Spend+Administration+Marketing_Spend+State,data=Data1[-c(49,50),])
summary(Fin1)
vif(Model_Log)
library("MASS")
stepAIC(Fin1)

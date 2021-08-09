#import dataset
Computer_Data <- read.csv("C:/Users/Rafiya/Downloads/Computer_Data.csv")
   View(Computer_Data)
   library(plyr)
   Computer_Data$cd <- as.numeric(revalue(Computer_Data$cd,c("yes"=1, "no"=0)))
   Computer_Data$multi <- as.numeric(revalue(Computer_Data$multi,c("yes"=1, "no"=0)))
   Computer_Data$premium <- as.numeric(revalue(Computer_Data$premium,c("yes"=1, "no"=0)))
   View(Computer_Data)
   class(Computer_Data)
attach(Computer_Data)   
summary(Computer_Data)   
plot(ram,price)
plot(cd,price)
cor(Computer_Data)
model1<-lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend)
summary(model1)

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
pairs(Computer_Data, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")
library(corpcor)
cor2pcor(cor(Computer_Data))
library(car)
influence.measures(model1)
influenceIndexPlot(model1, id.n=3) 
influencePlot(model1,id.n=3)
Model_Log <- lm(price~log(speed)+log(hd)+log(ram)+log(screen)+
                               log(cd)+log(multi)+log(premium)+log(ads)+log(trend)
                             ,data=Computer_Data[-c(1441,1701),])
summary(Model_Log)    

# Poly Modal
Model1_Poly <- lm(price~speed+I(speed^2)+I(speed^3)+hd+I(hd^2)+I(hd^3)+ram+I(ram^2)+I(ram^3)+screen+I(screen^2)+I(screen^3)+cd+I(cd^2)+I(cd^3)+multi+I(multi^2)+I(multi^3)+premium+I(premium^2)+I(premium^3)+ads+I(ads^2)+I(ads^3)+trend+I(trend^2)+I(trend^3),data=Computer_Data[-c(1441,1701),])
summary(Model1_Poly) 
FinalModel<-lm(price~speed+I(speed^2)+I(speed^3)+ hd+I(hd^2)+I(hd^3)+ram+I(ram^2)+I(ram^3)+screen+I(screen^2)+I(screen^3)+ cd+I(cd^2)+I(cd^3)+
                 multi+I(multi^2)+I(multi^3)+premium+I(premium^2)+I(premium^3)+ads+I(ads^2)+I(ads^3)+trend+I(trend^2)+I(trend^3),data=Computer_Data[-c(1441,1701),])

summary(FinalModel)
Profit_Predict <- predict(FinalModel)
View(Profit_Predict)
finplot <- Computer_Data[-c(1441,1701),]
View(finplot)

plot1 <- cbind(finplot$price, Profit_Predict)
pairs(plot1)
Final <- cbind(speed,hd,ram,screen,cd,multi,premium,ads,trend,price,Profit_Predict)
View(Final)
plot(FinalModel)


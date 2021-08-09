Cutlets <- read.csv("C:/Users/Rafiya/Downloads/Cutlets.csv")
   View(Cutlets)
   str(Cutlets)
attach(Cutlets)
boxplot(Unit.A,Unit.B)

#NORMALITY TEST#
shapiro.test(Unit.A)
shapiro.test(Unit.B)

#VARIENCE TEST#
var.test(Unit.A,Unit.B)

t.test(Cutlets,alternative = "greater",mu=0.3)


#TWO SAMPLE T TEST#
t.test(Unit.A,Unit.B,alternative = "two.side",conf.level = 0.95,correct=TRUE)


?t.test
t.test(Unit.A,Unit.B,alternative = "greater",var.equal = T)
#p-value>0.05 and hence p high so its accept null hypotesis


LabTATT <- read_excel("C:/Users/Rafiya/Downloads/LabTATT.xlsx")
 View(LabTATT)

#normality test
shapiro.test(`Laboratory 1`) 
shapiro.test(`Laboratory 2`)
shapiro.test(`Laboratory 3`)
shapiro.test(`Laboratory 4`)
Stacked_Data<-stack(LabTATT)
View(Stacked_Data)
attach(Stacked_Data)
#VARIENCE TEST#
bartlett.test(values~ind,data = Stacked_Data)
anova_results<-aov(values~ind,data=Stacked_Data)

summary(anova_results) 
#P -value is > 0.05. P High and Ho accept.



BuyerRatio <- read.csv("C:/Users/Rafiya/Downloads/BuyerRatio.csv")
   View(BuyerRatio)
  BuyerRatio<- as.numeric(c("male", "female"))
  
na.omit(BuyerRatio)
BuyerRatio<-as.numeric(c("male","female"))
View(BuyerRatio)




`Costomer+OrderForm` <- read.csv("C:/Users/Rafiya/Downloads/Costomer+OrderForm.csv")
   View(`Costomer+OrderForm`)


attach(`Costomer+OrderForm`)
#NORMALITY TEST#
shapiro.test(Phillippines)
shapiro.test(Indonesia)
shapiro.test(Malta)
shapiro.test(India)

chisq.test()

#VARIENCE TEST# 
var.test(Unit.A,Unit.B)
#TWO SAMPLE T TEST#
t.test(Unit.A,Unit.B,alternative = "two.side",conf.level = 0.95,correct=TRUE)


?t.test
t.test(Unit.A,Unit.B,alternative = "greater",var.equal = T)

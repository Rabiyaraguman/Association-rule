Cutlets <- read.csv("C:/Users/Rafiya/Downloads/Cutlets.csv")
View(Cutlets)
str(Cutlets)
attach(Cutlets)
boxplot(Unit.A,Unit.B)
plot(Unit.A)
plot(Unit.B)

#NORMALITY TEST#
shapiro.test(Unit.A)
shapiro.test(Unit.B)
#unita and unit b is normally distributed ,unit A=p-value = 0.32,unit B=p-value = 0.5225

#VARIENCE TEST#
var.test(Unit.A,Unit.B)

t.test(Cutlets,alternative = "greater")


#TWO SAMPLE T TEST#
t.test(Unit.A,Unit.B,alternative = "two.side",conf.level = 0.95,correct=TRUE)


?t.test
t.test(Unit.A,Unit.B,alternative = "greater",var.equal = T)

#P-value is 0.236 > 0.05=>P high Ho fly => Accept Ho, hence Average of unit A = Average of unit B
#As per above results we can say that there is similarity between unitA and unitB.unitA = unitB



##################LabTAT##################
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

#P-value is 0.00 < 0.05= Accept Ha, hence Average of atleast 1 laboratory are different
#As per results we can say that these are not equal i.e Average of atleast 1 laboratory are different





####################Fantaloons##################
Faltoons <- read.csv("C:/Users/Rafiya/Downloads/Faltoons.csv")
View(Faltoons)
attach(Faltoons)
data<-data.frame(Faltoons)
t<-table(Weekend,Weekdays)
t
t
#2-Proprotion Test
prop.test(t)
prop.test(t,alternative = "less",conf.level = 0.95,correct=TRUE)
prop.test(t,alternative = "two.side",conf.level = 0.95,correct=TRUE)
#p-value< <-0.05 accept null hypotesis

#P-value is 0.968 > 0.05=>P high Ho fly => Accept Ho
#Hence Proportion of male vs female in weekdays = Proportion of male vs female in weekends






###############Buyer ration############

BuyerRatio <- read.csv("C:/Users/Rafiya/Downloads/BuyerRatio.csv")
View(BuyerRatio)
table_line1=c(50,142,131,70)
table_line2=c(435,1523,1356,750)
buy<-rbind(table_line1,table_line2)
rownames(buy)<-c("Male","Female")
colnames(buy)<-c("C.Observed.Values","C.East","C.West","C.North","C.South")
buy
chisq.test(buy,correct = FALSE)
fisher.test(buy)


#P-value is 0.660 > 0.05=>P high Ho fly => Accept Ho, hence Average are same
#As per results we can say that there is proportion of male and female buying is similar





##########CustomerOrderForm############
library(readxl)
cof<-read_excel(file.choose()) 
View(cof)
stacked_cof<-stack(cof) 
attach(stacked_cof) 
View(stacked_cof) 
table(ind,values)
chisq.test(table(ind,values))

#P-value is 0.227 > 0.05=>P high Ho fly => Accept Ho, hence Average are same
#As per results we can say that all the canters are equal.

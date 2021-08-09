liver_data<- read.csv("D:\\sujeeth\\Indian Liver Patient Dataset (ILPD) (5).csv",header=FALSE)
View(liver)
colnames(liver_data) <- c("Age", "Sex", "Tot_Bil", "Dir_Bil", "Alkphos", "Alamine", 
                          "Aspartate", "Tot_Prot", "Albumin", "A_G_Ratio", "Disease")
liver_data$Sex <- (ifelse(liver_data$Sex == "Male", "M", "F"))
liver_data$Disease <- as.numeric(ifelse(liver_data$Disease == 2, 0, 1)) 
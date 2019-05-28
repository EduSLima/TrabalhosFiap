


Pacotes_Necessarios <- c("ggplot2","readr","dplyr", "corrgram","corrplot", "plotly","skimr","gridExtra","GGally")
lapply(Pacotes_Necessarios, require, character.only = TRUE)

nome_colunas <- c("id",
                  "acidez_fixa",
                  "acidez_volatil",
                  "acido_citrico",
                  "acucar_residual",
                  "cloretos", 
                  "fsd", 
                  "tsd",
                  "densidade",
                  "PH", 
                  "sulfatos",
                  "grau_alcolico",
                  "qualidade",
                  "tipo")

Vinhos.RAW <- read_csv2("./DataSets/BaseWine_Red_e_White.csv" ,col_names = nome_colunas, skip = 1)



par (mfrow=c(4,3))

hist(Vinhos.RAW$acidez_fixa)
hist(Vinhos.RAW$acidez_volatil)
hist(Vinhos.RAW$acido_citrico)
hist(Vinhos.RAW$acucar_residual)
hist(Vinhos.RAW$cloretos)
hist(Vinhos.RAW$fsd)
hist(Vinhos.RAW$tsd)
hist(Vinhos.RAW$densidade)
hist(Vinhos.RAW$PH)
hist(Vinhos.RAW$sulfatos)
hist(Vinhos.RAW$grau_alcolico)
hist(Vinhos.RAW$qualidade)


par (mfrow=c(4,3))

boxplot(Vinhos.RAW$acidez_fixa)       ; mtext("Acidez Fixa" , cex=0.8, side=1, line=2)
boxplot(Vinhos.RAW$acidez_volatil)    ; mtext("acidez_volatil" , cex=0.8, side=1, line=2)
boxplot(Vinhos.RAW$acido_citrico)     ; mtext("acido_citrico" , cex=0.8, side=1, line=2)
boxplot(Vinhos.RAW$acucar_residual)   ; mtext("acucar_residual" , cex=0.8, side=1, line=2)
boxplot(Vinhos.RAW$cloretos)          ; mtext("cloretos" , cex=0.8, side=1, line=2)
boxplot(Vinhos.RAW$fsd)               ; mtext("fsd" , cex=0.8, side=1, line=2)
boxplot(Vinhos.RAW$tsd)               ; mtext("tsd" , cex=0.8, side=1, line=2)
boxplot(Vinhos.RAW$densidade)         ; mtext("densidade" , cex=0.8, side=1, line=2)
boxplot(Vinhos.RAW$PH)                ; mtext("PH" , cex=0.8, side=1, line=2)
boxplot(Vinhos.RAW$sulfatos)          ; mtext("sulfatos" , cex=0.8, side=1, line=2)
boxplot(Vinhos.RAW$grau_alcolico)     ; mtext("grau_alcolico" , cex=0.8, side=1, line=2)
boxplot(Vinhos.RAW$qualidade)         ; mtext("qualidade" , cex=0.8, side=1, line=2)


#claramente é engano nao existe teor alcolico menor que 11 para vinhos 
boxplot.stats(Vinhos.RAW$grau_alcolico)$out  
vinhos_clean <-subset(Vinhos.RAW,grau_alcolico >2)




boxplot.stats(Vinhos.RAW$acidez_fixa)$out
boxplot.stats(Vinhos.RAW$acidez_volatil)$out 
boxplot.stats(Vinhos.RAW$acido_citrico)$out  
boxplot.stats(Vinhos.RAW$acucar_residual)$out
boxplot.stats(Vinhos.RAW$cloretos)$out       
boxplot.stats(Vinhos.RAW$fsd)$out            
boxplot.stats(Vinhos.RAW$tsd)$out            
boxplot.stats(Vinhos.RAW$densidade)$out      
boxplot.stats(Vinhos.RAW$PH)$out             
boxplot.stats(Vinhos.RAW$sulfatos)$out       


  



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
vinhos_clean <-subset(vinhos_clean,densidade < 1.01030)
vinhos_clean <-subset(vinhos_clean,id != 385)   #qualidade == 3 & fsd > 250)
vinhos_clean <-subset(vinhos_clean,id != 2488)   #qualidade ==6 & acido_citrico > 1.3) 
vinhos_clean <-subset(vinhos_clean,sulfatos < 1.8) 

#esses prints do infero dão uma visão de como os dados estão distribuidos em relação a variavel qualidade
attach(vinhos_clean)
par (mfrow=c(4,3))
plot(acidez_fixa,qualidade, main="Acidez Fixa", col=rgb(0,100,0,50,maxColorValue=255), pch=16)
abline(lm(qualidade ~ acidez_fixa))
plot(acidez_volatil,qualidade, main="Acidez Volatil", col=rgb(0,100,0,50,maxColorValue=255), pch=16)
abline(lm(qualidade ~ acidez_volatil))
plot(acido_citrico,qualidade, main="Acido Citrico", col=rgb(0,100,0,50,maxColorValue=255), pch=16)
abline(lm(qualidade ~ acido_citrico))
plot(acucar_residual,qualidade, main="Acucar Residual", col=rgb(0,100,0,50,maxColorValue=255), pch=16)
abline(lm(qualidade ~ acucar_residual))
plot(cloretos,qualidade, main="Cloreto", col=rgb(0,100,0,50,maxColorValue=255), pch=16)
abline(lm(qualidade ~ cloretos))
plot(fsd,qualidade, main="fsd", col=rgb(0,100,0,50,maxColorValue=255), pch=16)
abline(lm(qualidade ~ fsd))
plot(tsd,qualidade, main="tsd", col=rgb(0,100,0,50,maxColorValue=255), pch=16)
abline(lm(qualidade ~ tsd))
plot(densidade,qualidade, main="densidade", col=rgb(0,100,0,50,maxColorValue=255), pch=16)
abline(lm(qualidade ~ densidade))
plot(PH,qualidade, main="PH", col=rgb(0,100,0,50,maxColorValue=255), pch=16)
abline(lm(qualidade ~ PH))
plot(sulfatos,qualidade, main="Sulfatos", col=rgb(0,100,0,50,maxColorValue=255), pch=16)
abline(lm(qualidade ~ sulfatos))
plot(grau_alcolico,qualidade, main="Grau Alcolico", col=rgb(0,100,0,50,maxColorValue=255), pch=16)
abline(lm(qualidade ~ grau_alcolico))



summary(vinhos_clean)

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



par (mfrow=c(1,1))
plot(sulfatos,qualidade, main="Sulfatos", col=rgb(0,100,0,50,maxColorValue=255), pch=16)


mat_cor <-cor(vinhos_clean[2:12])
corrplot::corrplot(mat_cor, method="number", order="hclust")




par (mfrow=c(1,1))
plot(grau_alcolico,qualidade, main="Teor Alcoolico", col=rgb(0,100,0,50,maxColorValue=255), pch=16)
abline(lm(qualidade ~ grau_alcolico))

plot(qualidade,acidez_fixa, main="Acidez Fixa", col=rgb(0,100,0,50,maxColorValue=255), pch=16)


ggplot(data = vinhos_clean, mapping = aes(x = qualidade  , y = tipo )) + 
  geom_point()


ggplot(data = vinhos_clean, mapping = aes(x = tipo, y = qualidade)) +  geom_boxplot()
ggplot(data = vinhos_clean) +   geom_point(mapping = aes(x = grau_alcolico, y = densidade), alpha = 1 / 100)
ggplot(data = vinhos_clean) +  geom_hex(mapping = aes(x = grau_alcolico, y = densidade))

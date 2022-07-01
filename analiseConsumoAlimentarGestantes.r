#
#                                        CONSUMO ALIMENTAR-GESTANTES
#-------------------------------------------------------------------------
library(readxl)

#tabelas 
cgestante<-read_excel("Tabela_gestante.xlsx", sheet=1, col_names = T)

regiao<-as.vector(cgestante$regiao)
alimento<-cgestante$Consumo
c_gestante<-cgestante[,14:19]

#--------------------------------------------
#modelo de tendência linear temporal
variacao<-c()
pvalor<-c()
for(i in 1:dim(c_gestante)[1])
{
  Temporal<-ts(t(c_gestante[i,]), start = 2015,
               end = 2020,frequency = 1)
  tempo <- time(Temporal)
  modelo <- lm(Temporal ~ tempo) 
  variacao[i]<-summary(modelo)$coefficients[2,1] #variação média anual
  pvalor[i]<-summary(modelo)$coefficients[2,4] #p-valor
}

#--------------------------------------------
#Tabelas -Resultados
TabelaCgestante<-cbind(regiao,alimento,c_gestante,variacao,pvalor)

write.table(TabelaCgestante,file="TabelaCgestante.csv",sep=";",quote=F,dec=",",
            row.names=F)


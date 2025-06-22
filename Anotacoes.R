Implementação de documentos no R
setwd("C:\\Programas")

dados <- read.csv("dados.csv",sep=";",dec=",")
Programa do Software R - Estatística DescritivaArquivo

#######################################################################
# Media
#######################################################################

mean(dados$Perda)
tapply(dados$Perda,dados$Dieta,mean)

#######################################################################
# Mediana
#######################################################################

median(dados$Perda)
tapply(dados$Perda,dados$Dieta,median)

#######################################################################
# Amplitude
#######################################################################

diff(range(dados$Perda))

maxmin <- tapply(dados$Perda,dados$Dieta,range)
diff(maxmin$A)
diff(maxmin$B)

#######################################################################
# Desvio Padrao
#######################################################################

sd(dados$Perda)
tapply(dados$Perda,dados$Dieta,sd)

#######################################################################
# Intervalo Interquartil
#######################################################################

IQR(dados$Perda)

tapply(dados$Perda,dados$Dieta,IQR)

#######################################################################
# Tabela de Frequencias (absoluta e relativa)
#######################################################################

f  <- table(dados$Hipertensao)
fr <- prop.table(f)

# Tabela Cruzada

 f2 <- table(dados$Dieta,dados$Hipertensao)
fr2 <- prop.table(f2,1)

#######################################################################
# Grafico de Dispersao
#######################################################################

jpeg("figura1.jpg")
plot(dados$Idade,dados$Perda,xlab="Idade (anos)",ylab="Proporção de Perda de Peso (%)")
graphics.off()

cor(dados$Idade,dados$Perda,method="pearson")
cor(dados$Idade,dados$Perda,method="spearman")
cor(dados$Idade,dados$Perda,method="kendall")

#######################################################################
# Boxplot
#######################################################################

boxplot(dados$Perda,ylab="Proporção de Perda de Peso (%)")

boxplot(dados$Perda~dados$Dieta,ylab="Proporção de Perda de Peso (%)",xlab="Tipo de Dieta",names=c("Dieta A","Dieta B"))

#######################################################################
# Diagrama de Colunas
#######################################################################

tabela <- table(dados$Hipertensao)

barplot(tabela,ylab="Frequência Absoluta",xlab="Hipertensão")

barplot(prop.table(tabela),ylab="Frequência Relativa",xlab="Hipertensão",col=c("green","red"))

# Grafico de barras Cruzados

f.absolutas <- table(dados$Dieta,dados$Hipertensao)
f.relativas <- prop.table(f.absolutas,1)

pdf("bar.pdf")
barplot(f.relativas,beside=T,col=c("blue","yellow"),xlab="Hipertensão",ylab="Frequência Relativa")
legend("topleft",fill=c("blue","yellow"),legend=c("Dieta A","Dieta B"),bty="n")
graphics.off()
Programa do Software R - ProbabilidadeArquivo
#######################################################################################
# Distribuicao Exponencial
#######################################################################################

   media <- mean(tempo)
       x <- seq(0,12000,by=1)
fteorico <- dexp(x,rate=1/media)

hist(tempo,freq=F,xlab="Tempo (em dias)",ylab="Frequência Relativa",main="")
lines(x,fteorico,col="red")

#######################################################################################
# Distribuicao Normal
#######################################################################################

   peso <- dados$Peso
   media <- mean(peso)
  desvio <- sd(peso)
       x <- seq(min(peso),max(peso),by=0.001)
fteorico <- dnorm(x,mean=media,sd=desvio)

hist(peso,freq=F,xlab="Peso Empacotado (Kg)",ylab="Frequência Relativa",main="")
lines(x,fteorico,col="red")

qqnorm(peso,ylab="Quantis Observados da Amostra",xlab="Quantis Teóricos da Distribuição Normal")
qqline(peso,col="red")

#######################################################################################
# Analise Peso Empacotado (Maquina A)
#######################################################################################

MaqA <- dados[dados$Maquina=="A",]

    media1 <- mean(MaqA$Peso)
   desvio1 <- sd(MaqA$Peso)
        x1 <- seq(min(MaqA$Peso),max(MaqA$Peso),by=0.001)
fteoricoN1 <- dnorm(x1,mean=media1,sd=desvio1)

hist(MaqA$Peso,freq=F,xlab="Peso Empacotado (Kg)",ylab="Frequência Relativa",main="")
lines(x1,fteoricoN1,col="red")

qqnorm(MaqA$Peso,ylab="Quantis Observados da Amostra",xlab="Quantis Teóricos da Distribuição Normal")
qqline(MaqA$Peso,col="red")

#######################################################################################
# Analise Peso Empacotado (Maquina B)
#######################################################################################

MaqB <- dados[dados$Maquina=="B",]

    media2 <- mean(MaqB$Peso)
   desvio2 <- sd(MaqB$Peso)
        x2 <- seq(min(MaqB$Peso),max(MaqB$Peso),by=0.001)
fteoricoN2 <- dnorm(x2,mean=media2,sd=desvio2)

hist(MaqB$Peso,freq=F,xlab="Peso Empacotado (Kg)",ylab="Frequência Relativa",main="")
lines(x2,fteoricoN2,col="red")

qqnorm(MaqB$Peso,ylab="Quantis Observados da Amostra",xlab="Quantis Teóricos da Distribuição Normal")
qqline(MaqB$Peso,col="red")
Programa do Software R - Inferência EstatísticaArquivo
#######################################################################################
# Intervalo de Confiança Para a Média e Variância Populacionais
#######################################################################################

DietaA <- dados[dados$Dieta=="A",]
qqnorm(DietaA$Perda,ylab="Quantis Observados",xlab="Quantis Teóricos")
qqline(DietaA$Perda,col="red")

t.test(DietaA$Perda,conf.level=0.95)$conf.int

install.packages("BSDA")
library(BSDA)
z.test(DietaA$Perda,sigma.x=8.5,conf.level=0.95)$conf.int

install.packages("EnvStats")
library(EnvStats)
varTest(DietaA$Perda,conf.level=0.95)$conf.int
sqrt(varTest(DietaA$Perda,conf.level=0.95)$conf.int)

#######################################################################################
# Intervalo de Confiança Para a Proporção Populacional
#######################################################################################

tabela <- table(DietaA$Hipertensao)
prop.test(tabela[1],sum(tabela),conf.level=0.95)$conf.int

#######################################################################################
# Teste de Hipoteses Para a Média Populacional
#######################################################################################

qqnorm(dados$Perda,ylab="Quantis Observados",xlab="Quantis Teóricos")
qqline(dados$Perda,col="red")

t.test(dados$Perda,mu=20,conf.level=0.95)

shapiro.test(dados$Perda)

library(BSDA)
z.test(dados$Perda,sigma.x=sqrt(72.25),mu=20,conf.level=0.95)

#######################################################################################
# Teste de Hipoteses Para a Variância Populacional
#######################################################################################

library(EnvStats)
varTest(dados$Perda,sigma.squared=72.25,conf.level=0.90)

#######################################################################################
# Teste de Hipoteses Para a Proporção Populacional
#######################################################################################

tabela <- table(dados$Hipertensao)
prop.test(tabela[1],sum(tabela),p=0.35,conf.level=0.90)

#######################################################################################
# Comparação Entre Duas Médias
#######################################################################################

# Comparando preço/m2 (desvios padrão populacionais: DP.RC=71 e DP.RO=82)

RC <- c(4120,4050,3960,3940,3890,3910,4090,4120,4040,4060,4030,3920,4060,3970,4030,4090,
        3960,3970,4000,4120)
RO <- c(3720,3490,3810,3540,3570,3770,3640,3660,3610,3740,3610,3590,3690,3740,3750,3800,
        3680,3640)

qqnorm(RC,ylab="Quantis Observados",xlab="Quantis Teóricos")
qqline(RC,col="red")
shapiro.test(RC)

qqnorm(RO,ylab="Quantis Observados",xlab="Quantis Teóricos")
qqline(RO,col="red")
shapiro.test(RO)

library(BSDA)
z.test(RC,RO,sigma.x=71,sigma.y=82,conf.level=0.90)

# Comparando o nível sérico de ferro em umol/l

fibrose.cistica <- c(13.78,18.01,19.65,19.66,12.64,11.53,13.44,18.31,15.4,14.92,10.06,14.62,13.97)
      saudaveis <- c(26.04,30.42,11.53,29.08,19.82,16.01,20.39,21.46,6.95,20.5,30.2)

qqnorm(fibrose.cistica,ylab="Quantis Observados",xlab="Quantis Teóricos")
qqline(fibrose.cistica)
shapiro.test(fibrose.cistica)

qqnorm(saudaveis,ylab="Quantis Observados",xlab="Quantis Teóricos")
qqline(saudaveis)
shapiro.test(saudaveis)

var.test(fibrose.cistica,saudaveis)$p.value
t.test(fibrose.cistica,saudaveis,var.equal=FALSE,conf.level=0.90)

# Comparando pressão sistólica em mm/Hg

          Placebo <- c(211,210,210,203,196,190,191,177,173,170,163)
Hidroclorotiazida <- c(181,172,196,191,167,161,178,160,149,119,156)

qqnorm(Placebo,ylab="Quantis Observados",xlab="Quantis Teóricos")
qqline(Placebo,col="red")
shapiro.test(Placebo)

qqnorm(Hidroclorotiazida,ylab="Quantis Observados",xlab="Quantis Teóricos")
qqline(Hidroclorotiazida,col="red")
shapiro.test(Hidroclorotiazida)

t.test(Placebo,Hidroclorotiazida,paired=TRUE,conf.level=0.99)

# Comparando Proporção de Perda de Peso em %

DietaA <- dados[dados$Dieta=="A",]
DietaB <- dados[dados$Dieta=="B",]

qqnorm(DietaA$Perda,ylab="Quantis Observados",xlab="Quantis Teóricos")
qqline(DietaA$Perda,col="red")
shapiro.test(DietaA$Perda)

qqnorm(DietaB$Perda,ylab="Quantis Observados",xlab="Quantis Teóricos")
qqline(DietaB$Perda,col="red")
shapiro.test(DietaB$Perda)

var.test(dados$Perda~dados$Dieta)$p.value
t.test(dados$Perda~dados$Dieta,var.equal=TRUE,conf.level=0.90)

#########################################################################################
# Teste de correlacao
#########################################################################################

qqnorm(dados$Perda,ylab="Quantis Observados",xlab="Quantis Teóricos")
qqline(dados$Perda,col="red")
shapiro.test(dados$Perda)

qqnorm(dados$Idade,ylab="Quantis Observados",xlab="Quantis Teóricos")
qqline(dados$Idade,col="red")
shapiro.test(dados$Idade)

cor.test(dados$Perda,dados$Idade,method="pearson",conf.level=0.90)
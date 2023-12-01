##############################################################################################################################
# MICRODADOS DA PNAD CONTÍNUA TRIMESTRAL

# Endereço do repositório dos microdados: https://ftp.ibge.gov.br/

##############################################################################################################################

# Limpando arquivos armazenados na memoria

rm(list=ls(all=TRUE))

# Definindo limite de memoria para compilacao do programa

aviso <- getOption("warn")
options(warn=-1)
memory.limit(size=20000)
options(warn=aviso)
rm(aviso)

# Definindo opcao de codificacao dos caracteres e linguagem

aviso <- getOption("warn")
options(warn=-1)
options(encoding="latin1")
options(warn=aviso)
rm(aviso)

# Definindo opcao de exibicao de numeros sem exponencial

aviso <- getOption("warn")
options(warn=-1)
options(scipen=999)
options(warn=aviso)
rm(aviso)

# Definindo opcao de repositorio para instalacao dos pacotes necessarios

aviso <- getOption("warn")
options(warn=-1)
options(repos=structure(c(CRAN="https://cran.r-project.org/")))
options(warn=aviso)
rm(aviso)

# Carregando informacoes do pacote PNADcIBGE

if("PNADcIBGE" %in% rownames(installed.packages())==FALSE)
{
  install.packages("PNADcIBGE", dependencies=TRUE)
}
library("PNADcIBGE")
packageDescription("PNADcIBGE")
help(package="PNADcIBGE")

# Descrevendo as funcoes do pacote PNADcIBGE

get_pnadc
read_pnadc
pnadc_labeller
pnadc_deflator
pnadc_design
pnadc_example

# Obtendo informacoes do pacote haven

if("haven" %in% rownames(installed.packages())==FALSE)
{
  install.packages("haven")
}
library("haven")
write_sas
write_dta
write_sav

# Carregando informacoes do pacote survey

if("survey" %in% rownames(installed.packages())==FALSE)
{
  install.packages("survey")
}
library("survey")


# Carregando informacoes do pacote convey

if("convey" %in% rownames(installed.packages())==FALSE)
{
  install.packages("convey")
}
library("convey")
help(package="convey")


# Carregando O pacote tidyverse

library(tidyverse)

# Gerando microdados da PNAD Continua

var_sel <- c("UF", "Capital", "V2007", "V2009", "V2010", "VD3004", "VD4019", "VD4031", "V2001", "V1022","VD4001","VD4002", "VD3005", "VD4002", "VD4008", "VD4016", "VD4017", "V4010", "V4013", "VD4031", "VD4032", "VD4035", "VD4036", "VD4037")

p1_21 <- get_pnadc(year=2021, quarter=1, vars=var_sel, labels=TRUE, deflator=TRUE, design=TRUE)
p2_21 <- get_pnadc(year=2021, quarter=2, vars=var_sel, labels=TRUE, deflator=TRUE, design=TRUE)
p3_21 <- get_pnadc(year=2021, quarter=3, vars=var_sel, labels=TRUE, deflator=TRUE, design=TRUE)
p4_21 <- get_pnadc(year=2021, quarter=4, vars=var_sel, labels=TRUE, deflator=TRUE, design=TRUE)
p1_22 <- get_pnadc(year=2022, quarter=1, vars=var_sel, labels=TRUE, deflator=TRUE, design=TRUE)
p2_22 <- get_pnadc(year=2022, quarter=2, vars=var_sel, labels=TRUE, deflator=TRUE, design=TRUE)

# Aplicando os deflatores

p1_21$variables <- transform(p1_21$variables, rhtt_1_21=VD4019*Habitual)
p2_21$variables <- transform(p2_21$variables, rhtt_2_21=VD4019*Habitual)
p3_21$variables <- transform(p3_21$variables, rhtt_3_21=VD4019*Habitual)
p4_21$variables <- transform(p4_21$variables, rhtt_4_21=VD4019*Habitual)
p1_22$variables <- transform(p1_22$variables, rhtt_1_22=VD4019*Habitual)
p2_22$variables <- transform(p2_22$variables, rhtt_2_22=VD4019*Habitual)

# Estimando total da renda mensal habitual de todos os trabalhos deflacionada

total_rhtt_1_21 <- svytotal(x=~rhtt_1_21, design=p1_21, na.rm=TRUE)
total_rhtt_1_21 
print(total_rhtt_1_21, row.names=FALSE)

total_rhtt_2_21 <- svytotal(x=~rhtt_2_21, design=p2_21, na.rm=TRUE)
total_rhtt_2_21 
print(total_rhtt_2_21, row.names=FALSE)

total_rhtt_3_21 <- svytotal(x=~rhtt_3_21, design=p3_21, na.rm=TRUE)
total_rhtt_3_21 
print(total_rhtt_3_21, row.names=FALSE)

total_rhtt_4_21 <- svytotal(x=~rhtt_4_21, design=p4_21, na.rm=TRUE)
total_rhtt_4_21
print(total_rhtt_4_21, row.names=FALSE)

total_rhtt_1_22 <- svytotal(x=~rhtt_1_22, design=p1_22, na.rm=TRUE)
total_rhtt_1_22 
print(total_rhtt_1_22, row.names=FALSE)

total_rhtt_2_22 <- svytotal(x=~rhtt_2_22, design=p2_22, na.rm=TRUE)
total_rhtt_2_22 
print(total_rhtt_2_22, row.names=FALSE)


# Criando base com informacoes da renda de todos os trabalhos - vários trimestres

rhtt_vt <- matrix(0, nrow=6, ncol=1)
rhtt_vt[1,] <- total_rhtt_1_21 
rhtt_vt[2,] <- total_rhtt_2_21 
rhtt_vt[3,] <- total_rhtt_3_21 
rhtt_vt[4,] <- total_rhtt_4_21 
rhtt_vt[5,] <- total_rhtt_1_22 
rhtt_vt[6,] <- total_rhtt_2_22 
rhtt_vt <- as.data.frame(rhtt_vt)

colnames(rhtt_vt) <- c("rhtt")
rownames(rhtt_vt) <- c("1t21","2t21","3t21","4t21","1t22","2t22")
View(rhtt_vt)

# Criando coluna com trimestre 

rhtt_vt$trim <- rownames(rhtt_vt)
rhtt_vt$trim <- factor(rhtt_vt$trim, levels=c("1t21","2t21","3t21","4t21","1t22","2t22"))


ggplot(rhtt_vt, aes(x=trim, y=rhtt, group=1))+
  geom_point() +
  geom_line() +
  labs(x = "Trimestres", 
       y = "Renda de todos os trabalhos", 
       title = "Evolução da massa de Renda de todos os trabalhos")



# Estimando a renda média mensal real habitual de todos os trabalhos

rhttm_1_21 <- svymean(x=~rhtt_1_21, design=p1_21, na.rm=TRUE)
rhttm_1_21 
print(rhttm_1_21, row.names=FALSE)

rhttm_2_21 <- svymean(x=~rhtt_2_21, design=p2_21, na.rm=TRUE)
rhttm_2_21 
print(rhttm_2_21, row.names=FALSE)

rhttm_3_21 <- svymean(x=~rhtt_3_21, design=p3_21, na.rm=TRUE)
rhttm_3_21
print(rhttm_3_21, row.names=FALSE)

rhttm_4_21 <- svymean(x=~rhtt_4_21, design=p4_21, na.rm=TRUE)
rhttm_4_21 
print(rhttm_4_21, row.names=FALSE)

rhttm_1_22 <- svymean(x=~rhtt_1_22, design=p1_22, na.rm=TRUE)
rhttm_1_22 
print(rhttm_1_22, row.names=FALSE)

rhttm_2_22 <- svymean(x=~rhtt_2_22, design=p2_22, na.rm=TRUE)
rhttm_2_22 
print(rhttm_2_22, row.names=FALSE)


# Criando base com informacoes da renda média de todos os trabalhos - vários trimestres

rhttm <- matrix(0, nrow=6, ncol=1)
rhttm[1,] <- rhttm_1_21 
rhttm[2,] <- rhttm_2_21 
rhttm[3,] <- rhttm_3_21 
rhttm[4,] <- rhttm_4_21 
rhttm[5,] <- rhttm_1_22 
rhttm[6,] <- rhttm_2_22 
rhttm <- as.data.frame(rhttm)

colnames(rhttm) <- c("rhttm")
rownames(rhttm) <- c("1t21","2t21","3t21","4t21","1t22","2t22")
View(rhttm)

# Criando coluna com trimestre 

rhttm$trim <- rownames(rhttm)
rhttm$trim <- factor(rhttm$trim, levels=c("1t21","2t21","3t21","4t21","1t22","2t22"))


ggplot(rhttm, aes(x=trim, y=rhttm, group=1))+
  geom_point() +
  geom_line() +
  labs(x = "Trimestres", 
       y = "Renda média de todos os trabalhos", 
       title = "Evolução da Renda Média Real de todos os trabalhos")+
  theme(legend.position = "none")



# Estimando total de pessoas ocupadas e desocupadas 

t_ocup_1_21 <- svytotal(x=~VD4002, design=p1_21, na.rm=TRUE)
t_ocup_1_21
print(t_ocup_1_21, row.names=FALSE)

t_ocup_2_21 <- svytotal(x=~VD4002, design=p2_21, na.rm=TRUE)
t_ocup_2_21
print(t_ocup_2_21, row.names=FALSE)

t_ocup_3_21 <- svytotal(x=~VD4002, design=p3_21, na.rm=TRUE)
t_ocup_3_21
print(t_ocup_3_21, row.names=FALSE)

t_ocup_4_21 <- svytotal(x=~VD4002, design=p4_21, na.rm=TRUE)
t_ocup_4_21
print(t_ocup_4_21, row.names=FALSE)

t_ocup_1_22 <- svytotal(x=~VD4002, design=p1_22, na.rm=TRUE)
t_ocup_1_22
print(t_ocup_1_22, row.names=FALSE)

t_ocup_2_22 <- svytotal(x=~VD4002, design=p2_22, na.rm=TRUE)
t_ocup_2_22
print(t_ocup_2_22, row.names=FALSE)


# Criando base com informacoes das pessoas ocupadas e desocupadas - vários trimestres

t_ocup <- matrix(0, nrow=6, ncol=2)
t_ocup[1,] <- t_ocup_1_21 
t_ocup[2,] <- t_ocup_2_21 
t_ocup[3,] <- t_ocup_3_21 
t_ocup[4,] <- t_ocup_4_21 
t_ocup[5,] <- t_ocup_1_22 
t_ocup[6,] <- t_ocup_2_22 
t_ocup <- as.data.frame(t_ocup)

colnames(t_ocup) <- c("t_ocupad", "t_desocup")
rownames(t_ocup) <- c("1t21","2t21","3t21","4t21","1t22","2t22")
View(t_ocup)

# Criando coluna com trimestre 

t_ocup$trim <- rownames(t_ocup)
t_ocup$trim <- factor(t_ocup$trim, levels=c("1t21","2t21","3t21","4t21","1t22","2t22"))

# Gráfico com o total de ocupados 

ggplot(t_ocup, aes(x=trim, y=t_ocupad, group=1))+
  geom_point() +
  geom_line() +
  labs(x = "Trimestres", 
       y = "Total de ocupados", 
       title = "Evolução do número de pesssoas ocupadas")+
  theme(legend.position = "none")


# Gráfico com o total de desocupados 

ggplot(t_ocup, aes(x=trim, y=t_desocup, group=1))+
  geom_point() +
  geom_line() +
  labs(x = "Trimestres", 
       y = "Total de desocupados", 
       title = "Evolução do número de pesssoas desocupadas")+
  theme(legend.position = "none")


# Criando a taxa de desocupação

taxa = mutate (t_ocup, taxa_desocup = t_desocup /(t_ocupad +t_desocup)*100)

# Gráfico da taxa de desemprego 

ggplot(taxa, aes(x=trim, y=taxa_desocup, group=1))+
  geom_point() +
  geom_line() +
  labs(x = "Trimestres", 
       y = "taxa de desocupação", 
       title = "Evolução da taxa de desocupação")+
  theme(legend.position = "none")

################

# Estimando total de pessoas segundo o sexo 

tp_sexo_1_21 <- svytotal(x=~V2007, design=p1_21, na.rm=TRUE)
tp_sexo_1_21
print(tp_sexo_1_21, row.names=FALSE)

tp_sexo_2_21 <- svytotal(x=~V2007, design=p2_21, na.rm=TRUE)
tp_sexo_2_21
print(tp_sexo_2_21, row.names=FALSE)

tp_sexo_3_21 <- svytotal(x=~V2007, design=p3_21, na.rm=TRUE)
tp_sexo_3_21
print(tp_sexo_3_21, row.names=FALSE)

tp_sexo_4_21 <- svytotal(x=~V2007, design=p4_21, na.rm=TRUE)
tp_sexo_4_21
print(tp_sexo_4_21, row.names=FALSE)

tp_sexo_1_22 <- svytotal(x=~V2007, design=p1_22, na.rm=TRUE)
tp_sexo_1_22
print(tp_sexo_1_22, row.names=FALSE)

tp_sexo_2_21 <- svytotal(x=~V2007, design=p2_22, na.rm=TRUE)
tp_sexo_2_21
print(tp_sexo_2_21, row.names=FALSE)


# Criando base com informacoes das pessoas segundo o sexo - vários trimestres

tp_sexo <- matrix(0, nrow=6, ncol=2)
tp_sexo[1,] <- tp_sexo_1_21 
tp_sexo[2,] <- tp_sexo_2_21 
tp_sexo[3,] <- tp_sexo_3_21 
tp_sexo[4,] <- tp_sexo_4_21 
tp_sexo[5,] <- tp_sexo_1_22 
tp_sexo[6,] <- tp_sexo_2_22 
tp_sexo <- as.data.frame(tp_sexo)

colnames(tp_sexo) <- c("t_homens", "t_mulheres")
rownames(tp_sexo) <- c("1t21","2t21","3t21","4t21","1t22","2t22")
View(tp_sexo)

# Criando coluna com trimestre 

tp_sexo$trim <- rownames(tp_sexo)
tp_sexo$trim <- factor(tp_sexo$trim, levels=c("1t21","2t21","3t21","4t21","1t22","2t22"))

# Gráfico com o total de homens

ggplot(tp_sexo, aes(x=trim, y=t_homens, group=1))+
  geom_point() +
  geom_line() +
  labs(x = "Trimestres", 
       y = "Total de homens", 
       title = "Evolução do número de pesssoas do sexo masculino")+
  theme(legend.position = "none")


# Gráfico com o total de mulheres

ggplot(tp_sexo, aes(x=trim, y=t_mulheres, group=1))+
  geom_point() +
  geom_line() +
  labs(x = "Trimestres", 
       y = "Total de mulheres", 
       title = "Evolução do número de pesssoas do sexo feminino")+
  theme(legend.position = "none")


# Gráfico com o total de mulheres e homens - lado a lado

# Escala livre

tp_sexo_long = tp_sexo %>%
  gather(variavel, valor, -trim)
filter(tp_sexo_long, 
       variavel %in% c('t_homens', 't_mulheres')) %>%
  
  ggplot(aes(x=trim, y=valor, group=variavel))+
  geom_line (size =.9) +
  facet_wrap (~variavel, scales = 'free') +
  theme(legend.position = "none")


# Escala default

tp_sexo_long = tp_sexo %>%
  gather(variavel, valor, -trim)
filter(tp_sexo_long, 
       variavel %in% c('t_homens', 't_mulheres')) %>%
  
  ggplot(aes(x=trim, y=valor, group=variavel))+
  geom_line (size =.9) +
  facet_wrap (~variavel, scales = 'fixed') +
  theme(legend.position = "none")



# Modelos de Regressão


eq_rend_1_21 = svyglm(log(VD4019)~V2007+V2010+V2009*V2009+VD3005, design=p1_21)
summary(eq_rend_1_21)

eq_rend_2_21 = svyglm(log(VD4019)~V2007+V2010+V2009*V2009+VD3005, design=p2_21)
summary(eq_rend_2_21)

eq_rend_3_21 = svyglm(log(VD4019)~V2007+V2010+V2009*V2009+VD3005, design=p3_21)
summary(eq_rend_3_21)

eq_rend_4_21 = svyglm(log(VD4019)~V2007+V2010+V2009*V2009+VD3005, design=p4_21)
summary(eq_rend_4_21)


eq_rend_1_22 = svyglm(log(VD4019)~V2007+V2010+V2009*V2009+VD3005, design=p1_22)
summary(eq_rend_1_22)

eq_rend_2_22 = svyglm(log(VD4019)~V2007+V2010+V2009*V2009+VD3005, design=p2_22)
summary(eq_rend_2_22)

##############################################################################################################################
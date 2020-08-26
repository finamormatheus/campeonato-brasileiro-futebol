#__________________________________________________________________________
# Campeonato Brasileiro 2012-2020 # 
#__________________________________________________________________________
# Probabilidades de vitória do mandante, do visitante e de empate por ano #
#__________________________________________________________________________

# Load packages #
library(data.table)
library(tidyverse)
library(broom)
library(lfe)
library(patchwork)

# Clean enviroment and get working directory #
rm(list=ls())

# Open the dataset #
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data <- fread('Resultados.csv')
data2020 <- fread('Resultados-2020.csv')

data <- rbind(data,data2020)
rm(data2020)

# Create variables #
data <- data[,":=" (V=if_else(golMandante>golVisitante,1,0),
                    E=if_else(golMandante==golVisitante,1,0),
                    D=if_else(golMandante<golVisitante,1,0))]

# Máxima Rodada em 2020 #
RodMax = max(data[ano==2020]$rodada)

# Preparing data for regression # 
data_aux1 <- data %>% gather(variable,value,V,E,D) %>% mutate(analysis="all")
data_aux2 <- data %>% filter(rodada<=RodMax) %>% gather(variable,value,V,E,D) %>% mutate(analysis="restricted")
data_aux <- rbind(data_aux1,data_aux2)

# Regression # 
reg <- data_aux %>%
  group_by(variable,analysis) %>%
  do(tidy(felm(value~factor(ano)-1,data=.))) %>%
  mutate(ano = as.numeric(substr(term,12,15)),
         var = factor(variable,levels=c("V","E","D"))) 

# 2012-2019 Means # 
means <- data_aux %>%
  filter(ano<=2019) %>% 
  group_by(variable,analysis) %>%
  do(tidy(felm(value~1,data=.))) %>%
  data.table()
  

# Plots # 
ggplot(reg %>% filter(analysis=="all"),
             aes(x=ano,y=estimate,shape=var,color=var))+
  geom_point(position=position_dodge(width=.5),size=1.8)+
  geom_linerange(aes(ymin=estimate-1.96*std.error,ymax=estimate+1.96*std.error),
                position=position_dodge(width=.5),linetype="dotted")+
  scale_x_continuous("Ano do Campeonato",breaks=seq(2011,2020,1),labels=c("",seq(2012,2020,1)))+
  scale_y_continuous("Probabilidade",breaks=seq(0,1,.1))+
  scale_color_manual("",values=c("darkgreen","blue","red"),labels=c("Vitória do Mandante","Empate","Vitória do Visitante"))+
  scale_shape_discrete("",labels=c("Vitória do Mandante","Empate","Vitória do Visitante"))+
  geom_hline(yintercept=means[variable=="V"&analysis=="all",.(estimate)][[1]],color="darkgreen")+
  geom_hline(yintercept=means[variable=="D"&analysis=="all",.(estimate)][[1]],color="blue")+
  geom_hline(yintercept=means[variable=="E"&analysis=="all",.(estimate)][[1]],color="red")+
  annotate("text",x=2011,y=means[variable=="V"&analysis=="all",.(estimate)][[1]],label="Média\n2012-2019",color="darkgreen",hjust=0.5,size=2.3)+
  theme_classic()+
  labs(title="Campeonato Brasileiro - Probabilidade de vitória do mandante, do visitante e de empate",
       subtitle="Todos os jogos",
    caption=paste0("Jogos de 2020 atualizados até ",Sys.Date()," (rodada ",RodMax,").\nProbabilidade de vitória do mandante, do visitante e de empate para cada ano do Campeonato Brasileiro.\nOs pontos representam a probabilidade média e o tracejeado o intervalo de confiança (95%). As linhas sólidas são a média entre os anos 2012-2019."))+
  theme(axis.text.x = element_text(hjust=.5,size=rel(1)),
        axis.title.x = element_text(face = "bold",vjust = -1,size=10),
        axis.title.y = element_text(face = "bold", vjust = 3,size=10),
        strip.text.x = element_text(size = 8, face = "bold"),
        panel.grid.major.y = element_line(color="gray95",linetype="solid"),
        panel.grid.minor.x = element_line(color="gray90",linetype="dashed"),
        legend.position = "bottom",
        plot.title = element_text(hjust=0.5,face="bold",size=11),
        plot.caption = element_text(face="italic",size=7.5,hjust=0,lineheight=1),
        plot.subtitle = element_text(hjust = .5))
ggsave(paste0("figure01.png"),width=16,height=8,unit="cm",scale=1.5)

ggplot(reg %>% filter(analysis!="all"),
       aes(x=ano,y=estimate,shape=var,color=var))+
  geom_point(position=position_dodge(width=.5),size=1.8)+
  geom_linerange(aes(ymin=estimate-1.96*std.error,ymax=estimate+1.96*std.error),
                 position=position_dodge(width=.5),linetype="dotted")+
  scale_x_continuous("Ano do Campeonato",breaks=seq(2011,2020,1),labels=c("",seq(2012,2020,1)))+
  scale_y_continuous("Probabilidade",breaks=seq(0,1,.1))+
  scale_color_manual("",values=c("darkgreen","blue","red"),labels=c("Vitória do Mandante","Empate","Vitória do Visitante"))+
  scale_shape_discrete("",labels=c("Vitória do Mandante","Empate","Vitória do Visitante"))+
  geom_hline(yintercept=means[variable=="V"&analysis!="all",.(estimate)][[1]],color="darkgreen")+
  geom_hline(yintercept=means[variable=="D"&analysis!="all",.(estimate)][[1]],color="blue")+
  geom_hline(yintercept=means[variable=="E"&analysis!="all",.(estimate)][[1]],color="red")+
  annotate("text",x=2011,y=means[variable=="V"&analysis!="all",.(estimate)][[1]],label="Média\n2012-2019",color="darkgreen",hjust=0.5,size=2.3)+
  theme_classic()+
  labs(title="Campeonato Brasileiro - Probabilidade de vitória do mandante, do visitante e de empate",
            subtitle=paste0("Jogos até a rodada ",RodMax),
    caption=paste0("Jogos de 2020 atualizados até ",Sys.Date(),". Contém apenas jogos até a rodada ",RodMax," para todos os anos.\nProbabilidade de vitória do mandante, do visitante e de empate para cada ano do Campeonato Brasileiro.\nOs pontos representam a probabilidade média e o tracejeado o intervalo de confiança (95%). As linhas sólidas são a média entre os anos 2012-2019."))+
  theme(axis.text.x = element_text(hjust=.5,size=rel(1)),
        axis.title.x = element_text(face = "bold",vjust = -1,size=10),
        axis.title.y = element_text(face = "bold", vjust = 3,size=10),
        strip.text.x = element_text(size = 8, face = "bold"),
        panel.grid.major.y = element_line(color="gray95",linetype="solid"),
        panel.grid.minor.x = element_line(color="gray90",linetype="dashed"),
        legend.position = "bottom",
        plot.title = element_text(hjust=0.5,face="bold",size=11),
        plot.caption = element_text(face="italic",size=7.5,hjust=0,lineheight=1),
        plot.subtitle = element_text(hjust = .5))
ggsave(paste0("figure02.png"),width=16,height=8,unit="cm",scale=1.5)


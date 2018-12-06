
#install.packages("ffbase")
#install.packages("ddply")
library(ffbase)
library(tidyverse)
library(dplyr)
library(ggplot2)

library(viridis)
library(viridisLite)

df_base <- read.csv("Input/DOCENTES_NORTE.CSV",sep="|")


print(object.size(df_base),units = "MB")

View(df_base)

glimpse(df_base)

summary(df_base)

df_summary_escolaridade<-
  df_base %>%
  group_by(TP_ESCOLARIDADE) %>%
  summarise(qtd = n()) %>%
  ungroup()

df_summary_escolaridade<-
  df_base %>%
  group_by(TP_ESCOLARIDADE) %>%
  summarise(qtd = n()) %>%
  ungroup()


df_summary_escolaridade$qtd <- as.numeric(df_summary_escolaridade$qtd)

df_summary_escolaridade$DescEscolaridade <- c("Fundamental incompleto", "Fundamental completo", "Ensino Médio completo", "Superior completo")

df_summary_escolaridade$fraction <- df_summary_escolaridade$qtd / sum(df_summary_escolaridade$qtd)

ggplot(df_summary_escolaridade, aes(x = factor(1), y=df_summary_escolaridade$qtd,fill=factor(df_summary_escolaridade$DescEscolaridade)) ) + geom_bar(width = 1,stat="identity")+coord_polar(theta = "y")+coord_polar()

ggplot(df_summary_escolaridade, aes(x = "teste")) +
  geom_bar(width = 1, colour = "black")+coord_polar(theta = "y")

bp <- ggplot(df_summary_escolaridade, aes(x="", y=qtd, fill=DescEscolaridade))+
  geom_bar(width = 1, stat = "identity")
bp
pie <- bp + coord_polar("y", start=0)
pie

library(scales)

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
pie + scale_fill_grey() +  blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = qtd/4 + c(0, cumsum(qtd)[-length(qtd)]), 
                label = percent(qtd/100)), size=5)

table <- df_summary_escolaridade
label <- "TESTE"
fill_label <- "teste2"
text_color = "black"
legenda = 'bottom'
ggplot(data=table, aes(x=table$DescEscolaridade, y=table$fraction,fill=table$DescEscolaridade)) +
  geom_bar(position="dodge", stat = "identity") +
  scale_x_discrete(limits=table$DescEscolaridade, labels=table$DescEscolaridade) +
  labs(title=label,x="", y="", fill=fill_label)  + #label de eixos
  #scale_fill_gradient(discrete=TRUE,option = "plasma") + 
  #scale_fill_viridis(discrete=TRUE,option = "plasma") + 
  scale_fill_brewer(palette="YlGnBu") + #paleta de cores
  theme(panel.border = element_blank(), #ajusta tema(bordas e textos)
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 8, colour = text_color,face = "bold"),
        legend.title = element_text(size = 8, colour = text_color,face = "bold"),
        legend.position = legenda,
        axis.text.y = element_blank(), #element_text(size = 16, colour = text_color,face = "bold"),
        axis.text.x = element_text(size = 12, colour = text_color,face = "bold"),
        plot.title = element_text(size = 18, colour = text_color, hjust = 0.5,face = "bold"),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(), 
        #if(flip) #se possui parametro "flip" remove grid de Y, se nao de X
        panel.grid.major.y  = element_blank(),
        #else 
        panel.grid.major.x  = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA)) +
  geom_text(aes(label=paste(round(fraction*100,1),"%"), vjust = -.3))


ggplot(table,aes(x="",y=qtd,fill=DescEscolaridade))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar(theta = "y", start = 0)+
  scale_fill_manual(values=c("Lightblue","#AD7366","Lightgreen","Orange","Coral","Yellow"))+
  labs(x="",y="",title="teste \n",
       fill="teste")+
  geom_text(aes(x=1.2, y=midpoint,label=labels),color="black",
            fontface="bold",size=3.3)+
  theme(plot.title = element_text(hjust=0.5),
        legend.title = element_text(hjust=0.5,face = "bold",size=10))

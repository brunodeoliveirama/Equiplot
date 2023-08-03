###Artigo equiplot

rm(list=ls())## remover. Realiza um clear
gc()#garbage collector

library(ggplot2)
library(dplyr)
library(readxl)
library(ggpubr)
library(svglite)

d <- read_xlsx("Dados_equiplot_fatores.xlsx")
d <- d %>% filter(local!="Brasil")

d$PNS <- as.factor(d$PNS)
d$local <- factor(d$local,levels = c("Capital","RM excluindo capital","Interior"),labels=c("Capital","RM excluindo capital","Interior"))

##Gráfico Equiplot: Figura 1
g1 <- ggplot(data=d,aes(x=percentual,y=1))+
  facet_grid(Comportamentos~.,scales = "fixed",labeller = label_wrap_gen(width=15))+
  #geom_segment(aes(x=inferior,y=PNS,xend=superior,yend=PNS,color=local),size=0.7)+
  geom_point(aes(x=percentual,y=PNS,color=local),shape=19, size=1.7)+
  #geom_point(aes(x=inferior,y=PNS,color=local),shape="|",size=4)+
  #geom_point(aes(x=superior,y=PNS,color=local),shape="|",size=4)+
  labs(x="Prevalência (%)" , y= "PNS", title = "Prevalência de comportamentos de saúde em adultos brasileiros", caption = "Fonte: Brasil, PNS 2013 e 2019.")+
  theme(plot.title = element_text(size = 12))+
  theme(legend.position = "bottom",axis.text = element_text(face="bold"))+
  theme(legend.title = element_text(face = "bold", size= 9))+
  labs(colour = "Localidade:", size = 5); g1
  #scale_colour_grey(start = 0.0, end = .8)
  #theme_bw()

ggsave("Figura_1.pdf",units = "cm",width = 30,height = 15)
ggsave("Figura_1.svg",units = "cm",width = 30,height = 15)
ggsave("Figura_1.jpg",units = "cm",width = 30,height = 15)
ggsave("Figura_1.TIFF",units = "cm",width = 30,height = 15)


##Gráfico Equiplot: Figura 2
g2 <- ggplot(data=d,aes(x=percentual,y=1))+
  facet_grid(Comportamentos~.,scales = "fixed",labeller = label_wrap_gen(width=15))+
  geom_segment(aes(x=inferior,y=PNS,xend=superior,yend=PNS,color=local),size=0.7)+
  geom_point(aes(x=percentual,y=PNS,color=local),shape=19, size=1.7)+
  geom_point(aes(x=inferior,y=PNS,color=local),shape="|",size=4)+
  geom_point(aes(x=superior,y=PNS,color=local),shape="|",size=4)+
  labs(x="Prevalência (%)" , y= "PNS", title = "Prevalência e IC95% de comportamentos de saúde em adultos brasileiros", caption = "Fonte: Brasil, PNS 2013 e 2019.")+
  theme(plot.title = element_text(size = 12))+
  theme(legend.position = "bottom",axis.text = element_text(face="bold"))+
  theme(legend.title = element_text(face = "bold", size= 9))+
  labs(colour = "Localidade:", size = 5); g2
#scale_colour_grey(start = 0.0, end = .8)
#theme_bw()

ggsave("Figura_2.pdf",units = "cm",width = 30,height = 15)
ggsave("Figura_2.svg",units = "cm",width = 30,height = 15)
ggsave("Figura_2.jpg",units = "cm",width = 30,height = 15)
ggsave("Figura_2.TIFF",units = "cm",width = 30,height = 15)

##Diferença absoluta (2019 vs 2013)

d_dif <- read_xlsx("Dados_equiplot_fatores_dif.xlsx")
d_dif$local <- factor(d_dif$local,levels = c("Interior","RM excluindo capital","Capital"),labels=c("Interior","RM excluindo capital","Capital"))
d_dif$Comportamentos <- factor(d_dif$Comportamentos)
n <- abs(max(d_dif$superior))
n <- (n+0:4)[which((n+0:4)%%5==0)] # Busco o multiplo de 5, para fazer o tic do eixo x simetrico.


##Gráfico Equiplot: Figura 3
g3 <- ggplot(data=d_dif,aes(x=diferenca,y=1))+
  facet_grid(Comportamentos~.,scales = "fixed",labeller = label_wrap_gen(width=15))+
  geom_point(aes(x=diferenca,y=local,color=local),shape=19, size=1.7)+
  scale_x_continuous(limits=c(-n,n), breaks=seq(-n,n,5) )+
  geom_vline(xintercept = 0,color="grey")+
  labs(x="Diferenças de prevalência" , y= "Localidade (2019 vs 2013)", title = "Diferenças de prevalência de comportamentos de saúde em adultos brasileiros", caption = "Fonte: Brasil, PNS 2013 e 2019.")+
  theme(plot.title = element_text(size = 12))+
  theme(legend.position = "bottom",axis.text = element_text(face="bold"))+
  theme(legend.title = element_text(face = "bold", size= 9))+
  labs(colour = "Localidade:   ", size = 5); g3

ggsave("Figura_3.pdf",units = "cm",width = 30,height = 15)
ggsave("Figura_3.svg",units = "cm",width = 30,height = 15)
ggsave("Figura_3.jpg",units = "cm",width = 30,height = 15)
ggsave("Figura_3.TIFF",units = "cm",width = 30,height = 15)


##Gráfico Equiplot: Figura 4
g4 <- ggplot(data=d_dif,aes(x=diferenca,y=1))+
  facet_grid(Comportamentos~.,scales = "fixed",labeller = label_wrap_gen(width=15))+
  geom_point(aes(x=diferenca,y=local,color=local),shape=19, size=1.7)+
  scale_x_continuous(limits=c(-n,n), breaks=seq(-n,n,5) )+
  geom_vline(xintercept = 0,color="grey")+
  geom_segment(aes(x=inferior,y=local,xend=superior,yend=local,color=local),size=0.7)+
  geom_point(aes(x=inferior,y=local,color=local),shape="|",size=4)+
  geom_point(aes(x=superior,y=local,color=local),shape="|",size=4)+
  labs(x="Diferenças de prevalência e IC95%" , y= "Localidade (2019 vs 2013)", title = "Diferenças de prevalência e IC95% de comportamentos de saúde em adultos brasileiros", caption = "Fonte: Brasil, PNS 2013 e 2019.")+
  theme(plot.title = element_text(size = 12))+
  theme(legend.position = "bottom",axis.text = element_text(face="bold"))+
  theme(legend.title = element_text(face = "bold", size= 9))+
  labs(colour = "Localidade:   ", size = 5); g4

ggsave("Figura_4.pdf",units = "cm",width = 30,height = 15)
ggsave("Figura_4.svg",units = "cm",width = 30,height = 15)
ggsave("Figura_4.jpg",units = "cm",width = 30,height = 15)
ggsave("Figura_4.TIFF",units = "cm",width = 30,height = 15)



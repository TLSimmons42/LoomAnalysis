library(plot3D)
library(rgl)
library(dplyr)
library(ggplot2)
library(tidyverse)
#library(ggtext)
library(ggsci)
library(ggpubr)
library(ez)
library(ggpubr)
library(cowplot)

getPACstay <- function(){
  dat.avg <-
    read.table("pacMoving_2-25-23_OneMin.csv",
               header=T, sep=",") %>%
    dplyr::mutate(Participant = as.factor(Participant)) %>%
    dplyr::mutate(Condition = factor(condition, levels=c("s", "co")) %>%
                    fct_recode(Solo="s", Cooperative="co")) %>%
    dplyr::mutate(Group = as.factor(partGroup) %>%
                    fct_recode(Autistic="e", "Non-Autistic"="c"))
  
  
   dat.avg %>%
    ggplot(aes(x=Condition, y= pacStay, color=Group, fill=Group)) +
    geom_boxplot() +
    scale_color_brewer(palette = "Set1") +
    xlab("") +
    ylab("Cube Placing Sequence Time (ms)") +
    ylim(0, 850)+
    theme_pubr()+ 
    theme(plot.margin = unit(c(1, 0.5, 1, 0.5), "lines"))+
    theme(axis.title.y = element_text(face="bold"))+
    theme(text = element_text(size = 10))
}  
getPACmove <- function(){
  dat.avg <-
    read.table("pacMoving_2-25-23_OneMin.csv",
               header=T, sep=",") %>%
    dplyr::mutate(Participant = as.factor(Participant)) %>%
    dplyr::mutate(Condition = factor(condition, levels=c("s", "co")) %>%
                    fct_recode(Solo="s", Cooperative="co")) %>%
    dplyr::mutate(Group = as.factor(partGroup) %>%
                    fct_recode(Autistic="e", "Non-Autistic"="c"))
  
  
   dat.avg %>%
    ggplot(aes(x=Condition, y= pacMove, color=Group, fill=Group)) +
    geom_boxplot() +
    scale_color_brewer(palette = "Set1") +
    xlab("") +
    ylab("Cube Grabbing Sequence Time (ms)") +
    ylim(0, 850)+
    theme_pubr()+
    theme(legend.position = "none")+
    theme(plot.margin = unit(c(1, 0.5, 1, 0.5), "cm"))+
    theme(axis.title.y  = element_text(face="bold"))+
    theme(text = element_text(size = 10))
  
}  



v <- getPACstay() 
p <- getPACmove()


prow <- plot_grid(
  v + theme(legend.position = "none"),
  p + theme(legend.position = "none"),
  rel_heights = 10,
  #labels = c("A", "B"),
  nrow = 2, ncol = 1)



legend <- cowplot::get_legend(v) + theme(legend.position = "top")
  

plot_grid(v,p,nrow = 2, ncol = 1,
          rel_heights =  c(1, 1), legend
          #plot_margin = unit(c(1, 0.5, 1, 0.5), "cm")
          )


# #-----------------------------------------------------------------------------------------------------------
# 
# dat.avg <-
#   read.table("pacMoving_2-25-23_OneMin.csv",
#              header=T, sep=",") %>%
#   dplyr::mutate(Participant = as.factor(Participant)) %>%
#   dplyr::mutate(Condition = factor(condition, levels=c("s", "co")) %>%
#                   fct_recode(Solo="s", Cooperative="co")) %>%
#   dplyr::mutate(Group = as.factor(partGroup) %>%
#                   fct_recode(Autistic="e", "Non-Autistic"="c"))
# 
# ezANOVA(dat.avg, dv = pacStay, wid = Participant, 
#         within = c(Condition), between = c(Group), type=2)
# 
# #-----------------------------------------------------------------------------------------------------------
# 
# dat.avg <-
#   read.table("pacCo.csv",
#              header=T, sep=",") %>%
#   dplyr::mutate(Participant = as.factor(Participant)) %>%
#   dplyr::mutate(Condition = factor(condition, levels=c("s", "co")) %>%
#                   fct_recode(Solo="s", Cooperative="co")) %>%
#   dplyr::mutate(Group = as.factor(partGroup) %>%
#                   fct_recode(Autistic="e", "Non-Autistic"="c"))
# 
# ezANOVA(dat.avg, dv = pacStay, wid = Participant, 
#         between = c(Group), type=2)
# 
# 
# #-----------------------------------------------------------------------------------------------------------
# 
# dat.avg <-
#   read.table("pacSolo.csv",
#              header=T, sep=",") %>%
#   dplyr::mutate(Participant = as.factor(Participant)) %>%
#   dplyr::mutate(Condition = factor(condition, levels=c("s", "co")) %>%
#                   fct_recode(Solo="s", Cooperative="co")) %>%
#   dplyr::mutate(Group = as.factor(partGroup) %>%
#                   fct_recode(Autistic="e", "Non-Autistic"="c"))
# 
# ezANOVA(dat.avg, dv = pacStay, wid = Participant, 
#         between = c(Group), type=2)
# 
# 
# #-----------------------------------------------------------------------------------------------------------
# 
# dat.avg <-
#   read.table("pacMoving_2-25-23_OneMin.csv",
#              header=T, sep=",") %>%
#   dplyr::mutate(Participant = as.factor(Participant)) %>%
#   dplyr::mutate(Condition = factor(condition, levels=c("s", "co")) %>%
#                   fct_recode(Solo="s", Cooperative="co")) %>%
#   dplyr::mutate(Group = as.factor(partGroup) %>%
#                   fct_recode(Autistic="e", "Non-Autistic"="c"))
# 
# ezANOVA(dat.avg, dv = pacMove, wid = Participant, 
#         within = c(Condition), between = c(Group), type=2)
# 
# #-----------------------------------------------------------------------------------------------------------
# 
# dat.avg <-
#   read.table("pacCo.csv",
#              header=T, sep=",") %>%
#   dplyr::mutate(Participant = as.factor(Participant)) %>%
#   dplyr::mutate(Condition = factor(condition, levels=c("s", "co")) %>%
#                   fct_recode(Solo="s", Cooperative="co")) %>%
#   dplyr::mutate(Group = as.factor(partGroup) %>%
#                   fct_recode(Autistic="e", "Non-Autistic"="c"))
# 
# ezANOVA(dat.avg, dv = pacMove, wid = Participant, 
#         between = c(Group), type=2)
# 
# 
# #-----------------------------------------------------------------------------------------------------------
# 
# dat.avg <-
#   read.table("pacSolo.csv",
#              header=T, sep=",") %>%
#   dplyr::mutate(Participant = as.factor(Participant)) %>%
#   dplyr::mutate(Condition = factor(condition, levels=c("s", "co")) %>%
#                   fct_recode(Solo="s", Cooperative="co")) %>%
#   dplyr::mutate(Group = as.factor(partGroup) %>%
#                   fct_recode(Autistic="e", "Non-Autistic"="c"))
# 
# ezANOVA(dat.avg, dv = pacMove, wid = Participant, 
#         between = c(Group), type=2)
# 
# 
# 

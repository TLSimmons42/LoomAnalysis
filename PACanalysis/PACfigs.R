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
    xlab("Cube Placing Sequence") +
    ylab("Time (ms)") +
    ylim(0, 850)+
    theme_pubr()+ 
    theme(plot.margin = unit(c(0,2,0,0), "lines"))+
    theme(axis.title.x = element_text(face="bold"))
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
    xlab("Cube Grabbing Sequence") +
    ylab("Play Wall Fixation Time (ms)") +
    ylim(0, 850)+
    theme_pubr()+
    theme(plot.margin = unit(c(0,2,0,0), "lines"))+
    theme(axis.title.y.left  = element_text(face="bold"))
  
}  



v <- getPACstay() 
p <- getPACmove()+ ylab(NULL)


prow <- plot_grid(
  v + theme(legend.position = "none"),
  p + theme(legend.position = "none"),
  #labels = c("A", "B"),
  nrow = 2, ncol = 1)



legend <- get_legend(
  p + theme(legend.position = "right")
)
plot_grid(prow, legend,  rel_widths = c(4,.7))


#-----------------------------------------------------------------------------------------------------------

dat.avg <-
  read.table("pacMoving_2-25-23_OneMin.csv",
             header=T, sep=",") %>%
  dplyr::mutate(Participant = as.factor(Participant)) %>%
  dplyr::mutate(Condition = factor(condition, levels=c("s", "co")) %>%
                  fct_recode(Solo="s", Cooperative="co")) %>%
  dplyr::mutate(Group = as.factor(partGroup) %>%
                  fct_recode(Autistic="e", "Non-Autistic"="c"))

ezANOVA(dat.avg, dv = pacStay, wid = Participant, 
        within = c(Condition), between = c(Group), type=2)

#-----------------------------------------------------------------------------------------------------------

dat.avg <-
  read.table("pacCo.csv",
             header=T, sep=",") %>%
  dplyr::mutate(Participant = as.factor(Participant)) %>%
  dplyr::mutate(Condition = factor(condition, levels=c("s", "co")) %>%
                  fct_recode(Solo="s", Cooperative="co")) %>%
  dplyr::mutate(Group = as.factor(partGroup) %>%
                  fct_recode(Autistic="e", "Non-Autistic"="c"))

ezANOVA(dat.avg, dv = pacStay, wid = Participant, 
        between = c(Group), type=2)


#-----------------------------------------------------------------------------------------------------------

dat.avg <-
  read.table("pacSolo.csv",
             header=T, sep=",") %>%
  dplyr::mutate(Participant = as.factor(Participant)) %>%
  dplyr::mutate(Condition = factor(condition, levels=c("s", "co")) %>%
                  fct_recode(Solo="s", Cooperative="co")) %>%
  dplyr::mutate(Group = as.factor(partGroup) %>%
                  fct_recode(Autistic="e", "Non-Autistic"="c"))

ezANOVA(dat.avg, dv = pacStay, wid = Participant, 
        between = c(Group), type=2)


#-----------------------------------------------------------------------------------------------------------

dat.avg <-
  read.table("pacMoving_2-25-23_OneMin.csv",
             header=T, sep=",") %>%
  dplyr::mutate(Participant = as.factor(Participant)) %>%
  dplyr::mutate(Condition = factor(condition, levels=c("s", "co")) %>%
                  fct_recode(Solo="s", Cooperative="co")) %>%
  dplyr::mutate(Group = as.factor(partGroup) %>%
                  fct_recode(Autistic="e", "Non-Autistic"="c"))

ezANOVA(dat.avg, dv = pacMove, wid = Participant, 
        within = c(Condition), between = c(Group), type=2)

#-----------------------------------------------------------------------------------------------------------

dat.avg <-
  read.table("pacCo.csv",
             header=T, sep=",") %>%
  dplyr::mutate(Participant = as.factor(Participant)) %>%
  dplyr::mutate(Condition = factor(condition, levels=c("s", "co")) %>%
                  fct_recode(Solo="s", Cooperative="co")) %>%
  dplyr::mutate(Group = as.factor(partGroup) %>%
                  fct_recode(Autistic="e", "Non-Autistic"="c"))

ezANOVA(dat.avg, dv = pacMove, wid = Participant, 
        between = c(Group), type=2)


#-----------------------------------------------------------------------------------------------------------

dat.avg <-
  read.table("pacSolo.csv",
             header=T, sep=",") %>%
  dplyr::mutate(Participant = as.factor(Participant)) %>%
  dplyr::mutate(Condition = factor(condition, levels=c("s", "co")) %>%
                  fct_recode(Solo="s", Cooperative="co")) %>%
  dplyr::mutate(Group = as.factor(partGroup) %>%
                  fct_recode(Autistic="e", "Non-Autistic"="c"))

ezANOVA(dat.avg, dv = pacMove, wid = Participant, 
        between = c(Group), type=2)




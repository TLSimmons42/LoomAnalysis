library(tidyverse)
library(readxl)
library(glue)
library(ggtext)

set.seed(19760620)

metadata <- read_excel(path="C:/Users/Trent Simmons/Desktop/Data/LoomAnalysis/Violin Plot Tutorial code/schubert.metadata.xlsx", na="NA") %>%
  mutate(disease_stat = factor(disease_stat,
                               levels=c("NonDiarrhealControl",
                                        "DiarrhealControl",
                                        "Case")
  )
  )

alpha_diversity <- read_tsv("C:/Users/Trent Simmons/Desktop/Data/LoomAnalysis/Violin Plot Tutorial code/schubert.groups.ave-std.summary") %>%
  filter(method == "ave") %>%
  select(-label, -method)

metadata_alpha <- inner_join(metadata, alpha_diversity,
                             by=c('sample_id'='group')
)

healthy_color <- "#BEBEBE"
diarrhea_color <- "#0000FF"
case_color <- "#FF0000"

disease_count <- metadata_alpha %>%
  count(disease_stat)

healthy_n <- disease_count %>%
  filter(disease_stat == "NonDiarrhealControl") %>%
  pull(n)

diarrhea_n <- disease_count %>%
  filter(disease_stat == "DiarrhealControl") %>%
  pull(n)

case_n <- disease_count %>%
  filter(disease_stat == "Case") %>%
  pull(n)


metadata_alpha %>%
  ggplot(aes(x=disease_stat, y=invsimpson, fill=disease_stat)) +
  # geom_boxplot(show.legend=FALSE, outlier.shape=NA, alpha=0.25, width=0.6,
  #              coef=0)+
  stat_summary(fun.data = median_hilow, fun.args=0.50, show.legend=FALSE,
               geom="crossbar", alpha=0.25, width=0.6) +
  geom_jitter(show.legend=FALSE, width=0.25, shape=21, color="black") +
  labs(x=NULL,
       y="Inverse Simpson Index") +
  scale_x_discrete(breaks=c("NonDiarrhealControl","DiarrhealControl","Case"),
                   labels=c(glue("Healthy<br>(N={healthy_n})"),
                            glue("Diarrhea and<br>*C.difficile* negative<br>\\
                                 (N={diarrhea_n})"),
                            glue("Diarrhea and<br>*C.difficile* positive<br>\\
                                 (N={case_n})"))
  ) +
  scale_fill_manual(name=NULL,
                    breaks=c("NonDiarrhealControl","DiarrhealControl","Case"),
                    labels=c("Healthy",
                             "Diarrhea and<br>*C.difficile* negative",
                             "Diarrhea and<br>*C.difficile* positive"),
                    values=c(healthy_color, diarrhea_color, case_color)) +
  theme_classic() +
  theme(axis.text.x = element_markdown())

#ggsave("schubert_diversity.tiff", width=4.5, height=3.5)
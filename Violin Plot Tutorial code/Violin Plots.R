library(tidyverse)
library(readxl)
library(glue)
library(ggtext)

# This is making the random number generator used in the script reproduce the same values 
# This will allow reproducability of random things like the point jitter on plots
set.seed(19760620)


# Levels allows you to control thhe order of the catagories which would normally be set to alphbetical
metadata <- read_excel(path="C:/Users/Trent Simmons/Desktop/Data/LoomAnalysis/Violin Plot Tutorial code/schubert.metadata.xlsx", na="NA") %>%
  mutate(disease_stat = factor(disease_stat,
                               levels=c("NonDiarrhealControl",
                                        "DiarrhealControl",
                                        "Case")))
# select is going to remove the specified column from the df
alpha_diversity <- read_tsv("C:/Users/Trent Simmons/Desktop/Data/LoomAnalysis/Violin Plot Tutorial code/schubert.groups.ave-std.summary") %>%
  filter(method == "ave") %>%
  select(-label, -method)

# this is merging the two df's based on the similarity of a column in each
# the merged df will include only the rows that apear in both specified cols within the df's
metadata_alpha <- inner_join(metadata, alpha_diversity,
                             by=c('sample_id'='group')
)

healthy_color <- "#BEBEBE"
diarrhea_color <- "#0000FF"
case_color <- "#FF0000"

# this count function is going to take all the unique catagories from the col and output the total number of times that
# they occur in the df
disease_count <- metadata_alpha %>%
  count(disease_stat)

# this will assign a singular value to the variable from the specified column and catagory- IS NOT A DF
healthy_n <- disease_count %>%
  filter(disease_stat == "NonDiarrhealControl") %>%
  pull(n)

# this will assign a singular value to the variable from the specified column and catagory- IS NOT A DF
diarrhea_n <- disease_count %>%
  filter(disease_stat == "DiarrhealControl") %>%
  pull(n)

# this will assign a singular value to the variable from the specified column and catagory- IS NOT A DF
case_n <- disease_count %>%
  filter(disease_stat == "Case") %>%
  pull(n)


p <- metadata_alpha %>%
  ggplot(aes(x=disease_stat, y=invsimpson, fill=disease_stat)) +
  geom_violin(show.legend = FALSE, adjust = 0.75, alpha = 0.5)+
  # geom_boxplot(show.legend=FALSE, outlier.shape=NA, alpha=0.25, width=0.6,
  #              coef=0)+
  stat_summary(fun = median, show.legend=FALSE,
               geom="crossbar") +
  # geom_jitter(show.legend=FALSE, width=0.25, shape=21, color="black") +
  geom_dotplot(binaxis = "y", show.legend = FALSE, binwidth = 0.5, stackdir = "center" ) +
  labs(x=NULL,
       y="Inverse Simpson Index") +
  scale_x_discrete(breaks=c("NonDiarrhealControl","DiarrhealControl","Case"),
                   labels=c(glue("Healthy<br>(N={healthy_n})"),
                            ("Diarrhea and<br>*C.difficile* negative<br>\\
                                 (N={diarrhea_n})"),
                            glue("Diarrhea and<br>*C.difficile* positive<br>\\
                              glue   (N={case_n})"))
  ) +
  scale_fill_manual(name=NULL,
                    breaks=c("NonDiarrhealControl","DiarrhealControl","Case"),
                    labels=c("Healthy",
                             "Diarrhea and<br>*C.difficile* negative",
                             "Diarrhea and<br>*C.difficile* positive"),
                    values=c(healthy_color, diarrhea_color, case_color)) +
  theme_classic() +
  theme(axis.text.x = element_markdown())
p
#ggsave("schubert_diversity.tiff", width=4.5, height=3.5)
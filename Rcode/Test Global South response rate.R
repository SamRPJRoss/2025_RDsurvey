## load R script
library(tidyverse)
GS<-readr::read_csv("~/Desktop/Research/In progress/2023 RDN survey/GS analysis.csv")
glm(Response ~ Region, family = binomial, data = GS) %>%  
  summary()
# those in Global South responded less often to our survey than those in Global North

binom.test(x = length(GS$Region[GS$Region %in% "North"]),n = 377,p = 0.5)
# But literature also significantly biased towards Global North
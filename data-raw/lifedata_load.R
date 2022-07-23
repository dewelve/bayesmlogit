library(dplyr)

lifedata<- read.csv("./lifedata.csv") %>%
  mutate(black = ifelse(black == 1 & hispanic ==0,1,
                        ifelse(black == 0, 0, NA)),
         other = ifelse(other == 1 & hispanic == 0, 1,
                        ifelse(other==0,0,NA))) %>%
  filter(!is.na(black) & !is.na(other))
  


htouh <- c(1:8)
htod <- c(9)
uhtod <- c(9*(2:8))
uhtoh <- c(9*(1:7)+1)

lifedata <- lifedata %>%
  mutate(trans = ifelse(trans %in% htouh,1,
                        ifelse(trans %in% htod,2,
                               ifelse(trans %in% uhtod, 4, 
                                      ifelse(trans %in% uhtoh,3,NA))))) %>%
  filter(!is.na(trans))

usethis::use_data(lifedata, overwrite = TRUE)



library(dplyr)

lifedata<- read.csv("./lifedata.csv")[1:7194,]
  


htouh <- c(1:8)
htod <- c(9)
uhtod <- c(9*(2:8))

lifedata <- lifedata %>%
  mutate(trans = ifelse(trans %in% htouh,1,
                        ifelse(trans %in% htod,2,
                               ifelse(trans %in% uhtod, 4, 
                                      ifelse(is.na(trans),NA,3)))))

usethis::use_data(lifedata, overwrite = TRUE)



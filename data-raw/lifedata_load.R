library(dplyr)

lifedata<- read.csv("./lifedata.csv",row.names=1)

htouh <- c(1:8)
htod <- c(9)
uhtod <- c(9*(2:8))

lifedata_simp <- lifedata %>%
  mutate(trans = ifelse(trans %in% htouh,1,
                        ifelse(trans %in% htod,2,
                               ifelse(trans %in% uhtod, 4, 3))))

#write.csv(lifedata_simp,"./lifedata_simp.csv")


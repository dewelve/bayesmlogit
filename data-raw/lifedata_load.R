library(dplyr)

lifedata<- read.csv("./lifedata.csv") %>%
  mutate(black = ifelse(black == 1 & hispanic ==0,1,
                        ifelse(black == 0, 0, 0)),
         other = ifelse(other == 1 & hispanic == 0, 1,
                        ifelse(other==0,0,0))) %>%
  filter(other !=1) %>%
  select(colnames(lifedata)[-5])
  

htoh <- c(1)
htouh <- c(2:8)
htod <- c(9)
uhtoh <- c(9*(1:7)+1)

uhtod <- c(9*(2:8))


lifedata <- lifedata %>%
  mutate(trans = ifelse(trans %in% htoh,1,
                        ifelse(trans %in% htouh,2,
                               ifelse(trans %in% htod, 3, 
                                      ifelse(trans %in% uhtoh,4,
                                             ifelse(trans %in% uhtod,6,
                                                    ifelse(!is.na(trans),5,NA))))))) %>%
  filter(!is.na(trans))

lifedata <- lifedata[1:8198,]
usethis::use_data(lifedata, overwrite = TRUE)

#write.csv(lifedata, "lifedata2.csv",row.names = F)


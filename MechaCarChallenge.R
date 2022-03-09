library(tidyverse)

car_table <- read.csv(file= "MechaCar_mpg.csv")
sup_coil  <- read.csv(file= "Suspension_Coil.csv")

car_df<- as.data.frame(car_table, row.names= NULL, optionaL = FALSE)

summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, car_df))

total_summary<- sup_coil%>% summarize(MEAN=mean(PSI), MEDIAN=median(PSI), VARIANCE=var(PSI), SD=sd(PSI), .groups = "keep")

lot_summary<- sup_coil%>% group_by(Manufacturing_Lot) %>% summarize(MEAN=mean(PSI), MEDIAN=median(PSI), VARIANCE=var(PSI), SD=sd(PSI), .groups = "keep")

t.test((sup_coil$PSI),mu=mean(1500))

t.test(subset(sup_coil,Manufacturing_Lot = ("Lot1"), select = PSI),mu=mean(1500))

t.test(subset(sup_coil,Manufacturing_Lot = ("Lot2"), select = PSI),mu=mean(1500))

t.test(subset(sup_coil,Manufacturing_Lot == ("Lot3"), select = PSI),mu=mean(1500))


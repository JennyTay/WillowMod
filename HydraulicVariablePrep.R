#any of the individual relationships can be a threshold or a probability model.
library(sf)
library(lubridate)
library(tidyverse)



#In this script, we are modeling the suitabilty of different locations (nodes) throughout the Los Angeles River (LAR) mainstem for supporting
# the different life history phases of the Black willow, salix gooddingii.  We read in a series of dataframes to develop relationships 
# between each of the hydraulic variables, and the suitability for the willow's life phases



#read in hydraulic relationships 
shear <- read_csv("shear.csv", skip = 2)
force <- read_csv("pullout_force.csv")
table <- read_csv("watertable.csv")






#Water table
names(table)[4:10] <- c("draw_down_cm_day", "x50_day_survival", "mean_42_day_survival", "medium_survival_days", "survivor_root_growth_rate_mm_day",
                        "died_root_growth_rate_mm_day", "mean_depth_to_water_table_m")
table <- table %>% 
  filter(species %in% c("sgooddingii", "sexigua"), lifestage == "seedling", x50_day_survival != "na")

table$x50_day_survival <- as.numeric(table$x50_day_survival)

ggplot(data = table, mapping = aes(x = draw_down_cm_day, y = x50_day_survival, color = location, shape = species))+
  geom_point(size = 5)+
  theme_classic()+
  labs(x = "Draw down rate (cm/day)", y = "50 day survival (%)")+
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20),
        legend.title = element_text(size = 15), legend.text = element_text(size = 12))







# Force
# For force, we only have a population of values that successfully broke or pulled out a seedling
#we will take percentiles to determine P(seedling death)
# Break: 1 denotes roots breaking; 0 denotes roots slipping out.

force <- force %>% 
  dplyr::select(-c("ID","River", "Easting (m)2","Northing (m)2", "Elevation (m)3")) %>% 
  filter(Species4 %in% c("P", "S"), Type == "Pull test", !is.na(Break5)) 

names(force)[2] <- "Scour_depth_m"
names(force)[3] <- "Species"
names(force)[9] <- "Pullout_force_N"
names(force)[10] <- "Break"

force$Scour_depth_m <- as.factor(force$Scour_depth_m)
force$Break <- as.factor(force$Break)
force$Breakc<-ifelse(force$Break ==0, "slip", "break")

ggplot(data = force, mapping = aes(x = Scour_depth_m, y = Pullout_force_N))+
  geom_boxplot(aes(color = Breakc))+
  labs(x = "Scour Depth (m)", y = "Force (N)", color = "Type")+
  theme_classic()+
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20))

force1 <- force %>% 
  group_by(Scour_depth_m, Break) %>% 
  summarize(low = quantile(Pullout_force_N, probs = 0.25, na.rm = T),
            med = quantile(Pullout_force_N, probs = 0.25, na.rm = T),
            high = quantile(Pullout_force_N, probs = 0.75, na.rm = T),
            critical = quantile(Pullout_force_N, probs = 0.95, na.rm = T)
            )






# shear
names(shear)[c(1,4)] <- c("shear", "mortality")
shear$year <- as.factor(shear$year)
ggplot(data = shear, mapping = aes(x = shear, y = mortality, color = year, shape = year))+
  geom_point(size = 4)+
  geom_smooth(method = "lm")+
  labs(y = "Mortality (%)", x = "Bed Shear Stress (Pa)")+
  theme_classic()+
  geom_hline(yintercept = 50, color = "red", lwd = 1.5)+
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20))







############################ PART II #########################################

# we apply the relationships developed in HydrauilcVariablePrep to the hdyralics modeled for the LAR under various 
# waste water treatment scenarios to determine under which scenarios are the life histories supported.

#inundatin tolerance: 58 days of submergance of cuttings that were 4 weeks old and the salix spp survived just fine 
#inundation: Friedman et al - inundation of more than 85 days during the growing season will kill them (box elders)
#inundation: Amlin and Rood 2001 salix spp
#stream power: range of stream power that had > 25% cover for salix spp, Jacob bendix paper figure 5
#shear stress: Pasquale et al on salix cuttings
#shear stress: Friedman et al - shear stress that mobilization the underlying sediment will kill box elders 
#uprooting velocity: Bywater-Reyes (as a function of scour depth) - calcualted from the resisting force - could calcuate the other way to get velocity from the pullout force
#pullout force: Bywater-Reyes (as a function of scour depth) - 


#read in HEC-RAS output for modeling



hydlics <- read.csv("hecras.csv")
hydlics$Date <- mdy(hydlics$Date)
hydlics$Year <- year(hydlics$Date)
hydlics$Month <- month(hydlics$Date)
wateryr <- function(x){
  ifelse(month(x) %in% c(10, 11, 12), year(x)+1, year(x))
}

hydlics$Wateryr <- wateryr(hydlics$Date)

hydlics <- hydlics %>% 
  gather(key = "DepthPoint", value = "Depth", D1:D10)%>% 
  gather(key = "VelocityPoint", value = "Velocity", V1:V3) %>% 
  gather(key = "ShearPoint", value = "Shear", S1:S3) %>% 
  gather(key= "watabPoint", value = "TableDepth", W1:W3) %>% 
  gather(key = "PowerPoint", value = "Power", P2:P3) %>% 
  select(Date, Year, Month, Wateryr, Time, Node, DepthPoint, VelocityPoint, ShearPoint, PowerPoint, watabPoint, Flow..cfs., Depth, Shear, Velocity, Power, TableDepth)

save(hydlics, file = "hydlics.RData", compress = "xz")

patches <- unique(hydlics$Node)
patches <- patches %>% 
  filter(patches %in% c(11092450)) #list of nodes that are in the soft bottomed areas)

most_consecutive_val = function(vector, val = 1) { 
  with(rle(vector), max(lengths[values == val]))
}




#read in models and assign thresholds for use in model

#models

load("shear_seedling.rda")
load("watab_seedling.rda")
load("depth_germ.rda")
load("depth_seedling_mod.rda")

#thresholds
depth_pre_germ <- 3
watab_adult <- 300




#survival funcation
DecadeWillowAgeDist <- function(depth)
  
{
  
  # clip hydrologic and hydraulic variables to the boundaries of the soft bottom areas suitable for germination
  
  # dep_pre_germ_WY2011 - what is the durationn time that the depth >= 3 between 20110401 and 20110430
  # pre_germ_WY2011 - is the duration of time greater than 360 (half a month) ie than the ground is primed for germination
  # dep_germ_WY2011 - what is the durationn time that the depth <= 3 between 20110430 and 20110831
  # germ_WY2011 - is the duration of time greater than 720 (1 month) ie germination can occur because ground is not too flooded
  # dep_seedling_WY2011 - what is the durationn time that the depth >=10 between 20101001 and 20110930
  # seedling_WY2011 - is the duration of time greater than 240 (10 days )
  
  #vertical dist above the stream channel bottm as a surrograte for height above the water table (Bendix 1999)
  
  temp <- hydlics %>% 
    group_by(Node, Point, Year) %>% 
    mutate(
      
      #pre-germ
      
      #depth
      dep_pregrm_WY2011_val = most_consecutive_val(vector=abs(diff(which(hydlics$Depth[hydlics$Month == 4]>=3))==1)),
      dep_pregrm_WY2011_log = ifelse(dep_pregrm_WY2011_val>=360, TRUE, FALSE)),
  
  #germ
  
  #depth
  dep_germ_WY2011_val = most_consecutive_val(vector=abs(diff(which(hydlics$Depth[hydlics$Date >= ymd(20110430)& hydlics$Date <=ymd(20110831)]<=3))==1)),
  dep_germ_WY2011_log = ifelse(dep_germ_WY2011_val>720, TRUE, FALSE),
  
  #seedling
  
  #depth
  dep_seedlng_WY2011_val = most_consecutive_val(vector=abs(diff(which(hydlics$Depth[hydlics$Date >= ymd(20101001)& hydlics$Date <=ymd(20110930)]>=5))==1)),
  dep_seedlng_WY2011_log = ifelse(dep_seedlng_WY2011_val>1944, FALSE, TRUE),
  
  #shear stress
  shr_seedlng_WY2011_val = most_consecutive_val(vector=abs(diff(which(hydlics$Shear[hydlics$Date >= ymd(20101001)& hydlics$Date <=ymd(20110930)]>=30))==1)),
  shr_seedlng_WY2011_log = ifelse(shr_seedlng_WY2011_val>10, FALSE, TRUE),
  
  #adult
  
  #Shear_Pa
  dep_yng_WY2011_val = most_consecutive_val(vector=abs(diff(which(hydlics$Shear_Pa[hydlics$Date >= ymd(20101001)& hydlics$Date <=ymd(20110930)]>=100))==1)),
  dep_yng_WY2011_log = ifelse(dep_yng_WY2011_val>10, FALSE, TRUE),
  #power
  dep_yng_WY2011_val = most_consecutive_val(vector=abs(diff(which(hydlics$Power_W.m2[hydlics$Date >= ymd(20101001)& hydlics$Date <=ymd(20110930)]>=900))==1)),
  dep_yng_WY2011_log = ifelse(dep_yng_WY2011_val>10, FALSE, TRUE),
  
  ) 
temp <- temp %>%   
  select(Node, Point, 11: ncol(temp)) %>% 
  unique() 





# seedling growth

# for rows with willow age ==1:

# Inundation threshold: if velocty > x, 0, 1

# model for depth to groundwater vs survivability (set lenghts of roots depending on age per published papers)
# if p(survival)>0.5, 0, 1

# if survival is possible based on these two processes, than add one to willow age.

#Survival 2-5 years

#for rows with willow age == c(2,3,4):

# Inundation threshold: if velocty > x, 0, 1

# model for depth to groundwater vs survivability (set lenghts of roots depending on age per published papers)
# if p(survival)>0.5, 1, 0

# if survival is possible based on these two processes, than add one to willow age.

#survival 5-10 years

# for rows with willow age == c(5,6,7,8,9,10):

# Inundation threshold: if velocty > x, 0, 1

# model for depth to groundwater vs survivability (set lenghts of roots depending on age per published papers)
# if p(survival)>0.5, 1, 0

# if survival is possible based on these two processes, than add one to willow age.


# count the values in willow age to determine the age distribution
}

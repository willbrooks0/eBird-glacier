#
# Glacier eBird data analysis
# Jan 25, 2022
#
##########################

library(tidyverse)
library(dplyr)

############# Load glacier data ################
# read files
g1 <- read.csv("g1_100m.csv",sep = "\t")
g2 <- read.csv("g2_100m.csv",sep = "\t")
g3 <- read.csv("g3_100m.csv",sep = "\t")
g4 <- read.csv("g4_100m.csv",sep = "\t")
g5 <- read.csv("g5_100m.csv",sep = "\t")
g6 <- read.csv("g6_100m.csv",sep = "\t")
g7 <- read.csv("g7_100m.csv",sep = "\t")
glacier <- rbind(g1,g2,g3,g4,g5,g6,g7) #combine bird groups
rm("g1","g2","g3","g4","g5","g6","g7")       #delete separate dataframes

#Filtering
g_A <- glacier %>%            #low travel distance
  subset(effort_distance_km < 1) 
g_B <- subset(glacier,protocol_type == "Stationary") #stationary
g_filt1 <- rbind(g_A,g_B)                            #combine traveling and sationary
glacier_filt <- subset(g_filt1,latitude < 54.7)

###### Plot glacier birds #######
names <- c("Common Redpoll","Cooper's Hawk","Sharp-shinned Hawk","Northern Goshawk",
           "Spotted Sandpiper","Mallard","American Pipit","California Scrub-Jay",
           "Golden Eagle","Cedar Waxwing","Bohemian Waxwing","Ruffed Grouse",
           "Canada Goose","Snowy Owl","Barrow’s Goldeneye","Red-tailed Hawk",
           "Rough-legged Hawk","Baird's Sandpiper","Western Sandpiper","Pectoral Sandpiper",
           "Least Sandpiper","Semipalmated Sandpiper","Anna's Hummingbird","Wilson's Warbler",
           "Turkey Vulture","Hermit Thrush","Swainson's Thrush","Brown Creeper","Vaux's Swift",
           "Semipalmated Plover","Killdeer","American Dipper","Northern Harrier",
           "Evening Grosbeak","Northern Flicker","Rock Pigeon","Olive-sided Flycatcher",
           "American Crow","Common Raven","Steller's Jay","Black Swift","Sooty Grouse",
           "Dusky Grouse","Downy Woodpecker","Hairy Woodpecker","Pileated Woodpecker",
           "Pacific-slope Flycatcher","Hammond's Flycatcher","Horned Lark",
           "Brewer's Blackbird","Spruce Grouse","Merlin","Prairie Falcon","Peregrine Falcon",
           "American Kestrel","American Coot","MacGillivray's Warbler","Common Yellowthroat",
           "Northern Pygmy-Owl","Cassin's Finch","House Finch","Purple Finch","Bald Eagle",
           "Barn Swallow","Varied Thrush","Dark-eyed Junco","Willow Ptarmigan",
           "White-tailed Ptarmigan","Orange-crowned Warbler","Nashville Warbler",
           "Gray-crowned Rosy-finch","Long-billed Dowitcher","Red Crossbill",
           "White-winged Crossbill","Swamp Sparrow","Lincoln's Sparrow","Song Sparrow",
           "Brown-headed Cowbird","Townsend's Solitaire","Clark's Nutcracker",
           "House Sparrow","Savannah Sparrow","Fox Sparrow","Band-tailed Pigeon",
           "Canada Jay","Black-billed Magpie","American Three-toed Woodpecker",
           "Pine Grosbeak","Spotted Towhee","Western Tanager","Black-bellied Plover",
           "Black-capped Chickadee","Mountain Chickadee","Boreal Chickadee",
           "Chestnut-backed Chickadee","Vesper Sparrow","Ruby-crowned Kinglet",
           "Golden-crowned Kinglet","Rock Wren","Calliope Hummingbird","Rufous Hummingbird",
           "Yellow-rumped Warbler","Black-throated Gray Warbler","Hermit Warbler", 
           "Yellow Warbler","Townsend's Warbler","Mountain Bluebird","Western Bluebird",
           "Red-breasted Nuthatch","Red-naped Sapsucker","Red-breasted Sapsucker",
           "Pine Siskin","American Goldfinch","Chipping Sparrow","American Tree Sparrow",
           "Western Meadowlark","European Starling","Tree Swallow", "Violet-green Swallow",
           "Lesser Yellowlegs","Greater Yellowlegs","Solitary Sandpiper","Pacific Wren",
           "American Robin","Cassin’s Vireo","Warbling Vireo","Mourning Dove",
           "Golden-crowned Sparrow","White-crowned Sparrow")
# Add reporting frequency
g_percent <- glacier_filt %>% group_by(scientific_name) %>%  #summarize observation frequency by species
  summarize(obs_freq = mean(species_observed)*100) %>%
  ungroup() %>% mutate(common_name=names)
# >1% reporting
g_0percent <- subset(g_percent, obs_freq > 0) # Filter to 1% reporting 
# Plot
ggplot(data=g_0percent, aes(x=reorder(common_name, obs_freq), y=obs_freq)) +
  geom_bar(stat="identity") + theme_bw() + 
  labs(x="Species",y="eBird Observation frequency (%)") + coord_flip()

########## Statistics Glacier #############

# Effort data
n_distinct(glacier_filt$SAMPLING.EVENT.IDENTIFIER) # Number of checklists sampled
g_metadata <- distinct(glacier_filt, SAMPLING.EVENT.IDENTIFIER, .keep_all= TRUE)
sum(g_metadata$duration_minutes)/60

# Richness
g_data <- subset(glacier_filt,species_observed != FALSE)
g_richness <- n_distinct(g_data$scientific_name) # Species richness
g_richness

#diversity
g_data2 <- subset(g_data,observation_count != ("X"))
g_data3 <- transform(g_data2, observation_count = as.numeric(observation_count))
g_sum <- sum(g_data3$observation_count)
g_diversity <- g_data3 %>% group_by(scientific_name) %>%  
  summarise(count = sum(observation_count)) %>%  
  mutate(pi = count / c(g_sum)) %>%  
  mutate(ln_pi = log(pi)) %>%  
  mutate(hi = pi * ln_pi) %>%  
  ungroup()
g_diversity2 <- -sum(g_diversity$hi)
g_diversity2

############# Load non-glacier data ################
ng1 <- read.csv("g1_2100m.csv",sep = "\t")
ng2 <- read.csv("g2_2100m.csv",sep = "\t")
ng3 <- read.csv("g3_2100m.csv",sep = "\t")
ng4 <- read.csv("g4_2100m.csv",sep = "\t")
ng5 <- read.csv("g5_2100m.csv",sep = "\t")
ng6 <- read.csv("g6_2100m.csv",sep = "\t")
ng7 <- read.csv("g7_2100m.csv",sep = "\t")

non_glacier <- rbind(ng1,ng2,ng3,ng4,ng5,ng6,ng7) #combine bird groups
rm("ng1","ng2","ng3","ng4","ng5","ng6","ng7")       #delete separate dataframes

#Filtering
ng_A <- non_glacier %>%            #low travel distance
  subset(effort_distance_km < 1) 
ng_B <- subset(non_glacier,protocol_type == "Stationary") #stationary
ng_filt <- rbind(ng_A,ng_B)         #combine traveling and sationary
non_glacier_filt <- subset(ng_filt,latitude < 54.7)

########## Statistics non-Glacier #############

# Effort data
n_distinct(non_glacier_filt$SAMPLING.EVENT.IDENTIFIER) # Number of checklists sampled
ng_metadata <- distinct(non_glacier_filt, SAMPLING.EVENT.IDENTIFIER, .keep_all= TRUE)
sum(ng_metadata$duration_minutes)/60
sum(ng_metadata$effort_distance_km)

# Richness and checklist count non-glacier
ng_obs <- subset(non_glacier_filt,species_observed != FALSE)
n_distinct(ng_obs$scientific_name) # Species richness

#Diversity
ng_data2 <- subset(ng_obs,observation_count != ("X")) 
ng_data3 <- transform(ng_data2, observation_count = as.numeric(observation_count))
ng_sum <- sum(ng_data3$observation_count)
ng_diversity <- ng_data3 %>% group_by(scientific_name) %>%  
  summarise(count = sum(observation_count)) %>%  
  mutate(pi = count / c(ng_sum)) %>%  
  mutate(ln_pi = log(pi)) %>%  
  mutate(hi = pi * ln_pi) %>%  
  ungroup()
ng_diversity2 <- -sum(ng_diversity$hi)
ng_diversity2

############## Comparative statistics ###################

library(ggpubr)
library(data.table)
library(rstatix)
library(stats)
library(ggplot2)
library(gridExtra)

#### Species in common for Sorenson's Coefficient 
# glacier vs. non-glacier
g_1 <- distinct(g_data,scientific_name)
ng_1 <- distinct(ng_obs,scientific_name)
same1 = merge(g_1,ng_1,by="scientific_name")
n_distinct(same1$scientific_name)
n_distinct(g_1$scientific_name)
n_distinct(ng_1$scientific_name)

# non-glacier vs. Boyle and Martin (2015)
bm <- read.csv("BoyleMartin.csv",na.strings=c("", "NA"))%>% 
  mutate(scientific_name = Scientific.name)
same2 = merge(ng_1,bm,by="scientific_name")
n_distinct(same2$scientific_name)
n_distinct(bm$scientific_name)

#### Comparing frequencices with regression 
#glacier and non-glacier
gf_percent <- glacier_filt %>% group_by(scientific_name) %>%  #Generate observation frequency and apply natural log tranformation
  summarize(ofg = mean(species_observed)) %>% 
  ungroup()
ngf_percent <- non_glacier_filt %>% group_by(scientific_name) %>% #Generate observation frequency and apply natural log tranformation
  summarize(ofng = mean(species_observed)) %>% mutate(ofngt=ofng^(1/3))%>% 
  ungroup()
comp1 <- merge(gf_percent, ngf_percent, by = "scientific_name")

#Test for normality before tranformation
  #Density plots
ggdensity(comp1$ofng)
  #QQ plots
ggqqplot(comp1$ofng)

#Test for normality after tranformation
  #Density plots
ggdensity(comp1$ofngt)
  #QQ plots
ggqqplot(comp1$ofngt)

#Linear regression
lm1 <- lm(ofngt~ofg,data=comp1)
summary(lm1)

#Plot
comp_plot1 <- ggplot(comp1,aes(x=ofg,y=ofngt)) + geom_point() + theme_bw() +
  labs(x="Glacier and snowfield observation frequency",y="Other mountain habitats observation frequency") +
  geom_smooth(method="lm", se=FALSE,colour = "black")
comp_plot1

#non-glacier and Boyle and Marin (2015)
comp2 <- merge(ngf_percent, bm, by = "scientific_name",all=TRUE) %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
#Regression
lm2<-lm(ofngt~Obs_freq,data=comp2)
summary(lm2)
#Plot
comp_plot2 <- ggplot(comp2,aes(x=Obs_freq,y=ofngt)) + geom_point() + theme_bw() +
  labs(x="Professional observation frequency",y="eBird observation frequency") +
  geom_smooth(method="lm", se=FALSE,colour = "black")
comp_plot2

##### Comparing frequency of each species in glacier and non glacier
#Find species more common in glaciers
freq_compare <- comp1 %>% mutate(common_name=names) %>%
  mutate(compare = (ofg - ofng),perc_diff = (ofg - ofng)/ofng*100)    #Compare glacier to non glacier
big_diff <- subset(freq_compare, abs(compare) > 0.03)
g_common <- subset(freq_compare, compare > 0) %>% #select only species more common in glaciers
  select(c(scientific_name))
g_uncommon <- subset(freq_compare, compare < 0) %>% #select only species less common in glaciers
  select(c(scientific_name))

#Plot frequency difference
diff_plot<-ggplot(data=big_diff, aes(x=reorder(common_name, compare), y=compare)) +
  geom_bar(stat="identity") + theme_bw() + 
  scale_y_continuous(limits = c(-100, 1200), breaks = seq(-100, 1000, by = 100)) +
  labs(x="Species",y="% increase in eBird observation frequency") + coord_flip()
diff_plot 

#Filter dataset by more common glacier species
gf <- glacier_filt %>% select(c(SAMPLING.EVENT.IDENTIFIER,scientific_name,species_observed)) %>% 
  mutate(Area = "glacier")  #Fewer columns and adding glacier variable
ngf <- non_glacier_filt %>% select(c(SAMPLING.EVENT.IDENTIFIER,scientific_name,species_observed)) %>% 
  mutate(Area = "non_glacier")      #Fewer columns and adding non-glacier variable
combined <- rbind(ngf,gf)                 #Combine glacier and non-glacier data

sample1 <- left_join(g_common,combined,by = "scientific_name") #filter species
sample2 <- left_join(g_uncommon,combined,by = "scientific_name") #filter species

#Create chi square table
table1 <- as.data.table(sample1)
chi_table1 <- table1[, chisq_test(.SD$species_observed,.SD$Area), by=scientific_name]

table2 <- as.data.table(sample2)
chi_table2 <- table2[, chisq_test(.SD$species_observed,.SD$Area), by=scientific_name]

######Bootstrap#########

# Set lists for loop
rand <- list()
g <- list()
g2 <- list()
richness <- list()
sum <- list()
hi_set <- list()
div_set <- list()

#Create set of checklist IDs
ng_lists <- distinct(non_glacier_filt,SAMPLING.EVENT.IDENTIFIER, .keep_all= T) %>% 
  select(c(SAMPLING.EVENT.IDENTIFIER,SAMPLING.EVENT.IDENTIFIER.y))

# Loop
for (i in 1:10000) {
  rand[[i]] <- ng_lists[sample(nrow(ng_lists), 70), ]                   #Resample checklists
  g[[i]] <- left_join(rand[[i]],non_glacier_filt,by = "SAMPLING.EVENT.IDENTIFIER") %>%  #Join with original dataset
    subset(species_observed != FALSE)                       #Filter for observed sp
  g2[[i]] <- g[[i]] %>% subset(observation_count != ("X"))  %>%        #Filter for non X counts
    transform(observation_count = as.numeric(observation_count))  #Make count numerical
  richness[[i]] <- n_distinct(g[[i]]$scientific_name)         #Calculate species richness
  sum[[i]] <- sum(g2[[i]]$observation_count)                 #Get total count of individuals
  hi_set[[i]] <- g2[[i]] %>% group_by(scientific_name) %>%   #Calculate Shannon's diversity
    summarise(count = sum(observation_count)) %>%  
    mutate(pi = count / c(sum[[i]])) %>%  
    mutate(ln_pi = log(pi)) %>%  
    mutate(hi = pi * ln_pi) %>%  
    ungroup()
  div_set[[i]] <- -sum(hi_set[[i]]$hi)                       
}

# Average results from bootstrap
summary(unlist(richness))
sd(unlist(richness))
quantiles <- quantile(unlist(richness), probs = c(.05, .95))
quantiles

# Plot bootstrap data
hist(unlist(richness),xlim=c(40,85),ylim=c(0,2000),xlab="Species richness")
abline(v = c(quantiles), lwd=2, lty=2,col="gray")
abline(v = g_richness, col="red", lwd=3)
abline(v = mean(unlist(richness)), col="red", lwd=3)
text(53, 1800, "glacier/snowfield",col="red")
text(74, 1800, "other habitats",col="red")


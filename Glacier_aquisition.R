#
# eBird data extraction glaciers
# June 15, 2021
#
##########################

library(auk)
library(lubridate)
library(sf)
library(tidyverse)

#Set eBird observation and sampling files
ebd <- auk_ebd("ebd_relJun-2021.txt", 
               file_sampling = "ebd_sampling_relJun-2021.txt")

############## SET SPECIES (one at a time) ############
  #species set 1
species <- c("Barrow’s Goldeneye", "Ruffed Grouse", "White-tailed Ptarmigan", 
             "Sooty Grouse", "Rock Pigeon", "Band-tailed Pigeon", "Mourning Dove", "Black Swift", 
             "Vaux's Swift", "Anna's Hummingbird", "Rufous Hummingbird", "Calliope Hummingbird",
             "Turkey Vulture", "Golden Eagle", "Northern Harrier", 
             "Sharp-shinned Hawk", "Cooper's Hawk", "Northern Goshawk", "Bald Eagle", 
             "Red-tailed Hawk", "Rough-legged Hawk", "Northern Pygmy-Owl", "Snowy Owl")
  #species set 2
species <- c("American Three-toed Woodpecker", "Red-breasted Sapsucker", "Downy Woodpecker", 
             "Hairy Woodpecker", "Pileated Woodpecker", "Northern Flicker", "American Kestrel", 
             "Merlin", "Peregrine Falcon", "Prairie Falcon", "Olive-sided Flycatcher", 
             "Hammond's Flycatcher", "Pacific-slope Flycatcher", "Cassin’s Vireo", 
             "Warbling Vireo", "Common Yellowthroat", "Nashville Warbler", "Yellow Warbler", "Yellow-rumped Warbler", 
             "Black-throated Gray Warbler", "Townsend's Warbler", "Hermit Warbler", 
             "Wilson's Warbler", "Townsend’s Warbler", "Western Tanager")
  #species set 3
species <- c("Boreal Chickadee", "Horned Lark",
             "Tree Swallow", "Violet-green Swallow", "Barn Swallow", "Golden-crowned Kinglet", 
             "Ruby-crowned Kinglet", "Red-breasted Nuthatch", "Brown Creeper", "Pacific Wren", 
             "Rock Wren", "American Dipper", "European Starling", "Western Bluebird", 
             "Mountain Bluebird", "Townsend's Solitaire", "Varied Thrush", "Swainson's Thrush", 
             "Hermit Thrush", "American Robin", "Cedar Waxwing", "House Sparrow", "American Pipit")
  #species set 4
species <- c("Evening Grosbeak", "Pine Grosbeak", "Gray-crowned Rosy-finch", "Purple Finch", 
             "Cassin's Finch", "Red Crossbill", "Common Redpoll", "Pine Siskin", "American Goldfinch", 
             "Chipping Sparrow", "American Tree Sparrow", "Fox Sparrow", "Dark-eyed Junco", 
             "White-crowned Sparrow", "Golden-crowned Sparrow", "Savannah Sparrow", "Song Sparrow", 
             "Lincoln's Sparrow", "Spotted Towhee", "Western Meadowlark", "Brown-headed Cowbird", 
             "Brewer's Blackbird", "Orange-crowned Warbler", "MacGillivray's Warbler")
  #species set 5
species <- c("Canada Jay", "Steller's Jay", "California Scrub-Jay", 
             "Clark's Nutcracker", "American Crow", "Common Raven", "Black-capped Chickadee", 
             "Mountain Chickadee", "Chestnut-backed Chickadee")
  #species set 6 (shorebirds)
species <- c("Baird's Sandpiper","Least Sandpiper","Western Sandpiper","Pectoral Sandpiper",
             "Lesser Yellowlegs","Greater Yellowlegs","Solitary Sandpiper","Long-billed Dowitcher",
             "Black-bellied Plover","Semipalmated Plover","Semipalmated Sandpiper","Killdeer")
  #species set 7
species <- c("American Coot","Black-billed Magpie","Bohemian Waxwing","Canada Goose","Dusky Grouse",
             "House Finch","Mallard","Red-naped Sapsucker","Spotted Sandpiper","Spruce Grouse",
             "Swamp Sparrow","Vesper Sparrow","White-winged Crossbill","Willow Ptarmigan")

#######Filter by state

# WASHINGTON
  # WA
ebd_WA <- ebd %>% 
  auk_state("US-WA") %>% auk_species(species) %>% 
  auk_bbox(bbox = c(-123.780,45.870, -120.300,49.001)) %>% 
  auk_complete()
  #output files
data_dir <- "WA"
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}
wa_ebd <- file.path(data_dir, "7ebd_wa.txt")
wa_sampling <- file.path(data_dir, "7sed_wa.txt")

if (!file.exists(wa_ebd)) {
  auk_filter(ebd_WA, file = wa_ebd, file_sampling = wa_sampling)
}

  #WA checklists
wa_lists1 <- read.delim("WA/sed_wa.txt") %>% 
  select(c(SAMPLING.EVENT.IDENTIFIER,LATITUDE,LONGITUDE))
#write csv for GIS
write.table(wa_lists1, file = "WA/wa_lists.csv", sep = "\t",
            row.names = FALSE)

# OREGON
  # OR stationary and travelling
ebd_OR <- ebd %>% 
  auk_state("US-OR") %>% auk_species(species) %>% 
  auk_bbox(bbox = c(-121.948,43.965, -121.461,45.456)) %>% 
  auk_complete()
  #output files
data_dir <- "OR"
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}
or_ebd <- file.path(data_dir, "7ebd_or.txt")
or_sampling <- file.path(data_dir, "7sed_or.txt")

if (!file.exists(or_ebd)) {
  auk_filter(ebd_OR, file = or_ebd, file_sampling = or_sampling)
}

  #OR checklists
or_lists1 <- read.delim("OR/sed_or.txt") %>% 
  select(c(SAMPLING.EVENT.IDENTIFIER,LATITUDE,LONGITUDE))
  #write csv for GIS
write.table(or_lists1, file = "OR/or_lists.csv", sep = "\t",
            row.names = FALSE)

# BRITISH COLUMBIA
  #BC travelling and stationary A
ebd_BC1 <- ebd %>% 
  auk_state("CA-BC") %>% auk_species(species) %>% 
  auk_bbox(bbox = c(-127.000,48.999, -114.050,50.000)) %>% 
  auk_complete()
  #output files
data_dir <- "BC"
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}
bc_ebd1 <- file.path(data_dir, "7ebd_bc1.txt")
bc_sampling1 <- file.path(data_dir, "7sed_bc1.txt")

if (!file.exists(bc_ebd1)) {
  auk_filter(ebd_BC1, file = bc_ebd1, file_sampling = bc_sampling1)
}
  #BC 2
ebd_BC2 <- ebd %>% 
  auk_state("CA-BC") %>% auk_species(species) %>% 
  auk_bbox(bbox = c(-139.780,50.000, -114.050,60.000)) %>% 
  auk_complete()
#output files
data_dir <- "BC"
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}
bc_ebd2 <- file.path(data_dir, "7ebd_bc2.txt")
bc_sampling2 <- file.path(data_dir, "7sed_bc2.txt")

if (!file.exists(bc_ebd2)) {
  auk_filter(ebd_BC2, file = bc_ebd2, file_sampling = bc_sampling2)
}

  #BC checklists
bc_lists1 <- read.delim("BC/sed_bc1.txt") %>% 
  select(c(SAMPLING.EVENT.IDENTIFIER,LATITUDE,LONGITUDE))
bc_lists2 <- read.delim("BC/sed_bc2.txt") %>% 
  select(c(SAMPLING.EVENT.IDENTIFIER,LATITUDE,LONGITUDE))
#write csv for GIS
write.table(bc_lists1, file = "BC/bc_lists1.csv", sep = "\t",
            row.names = FALSE)
write.table(bc_lists2, file = "BC/bc_lists2.csv", sep = "\t",
            row.names = FALSE)

#Exported datasets to be used spatial analysis in QGIS (and text analysis in "Glacier_fieldnotes.R"), 
# then returned to this file to finish assembling the dataset in the next steps

########################################################################################

###### Re-assemble filtered dataset ######
  # Load set of points selected by buffered glacier polygons in QGIS
wa_lists_glacier <- read.csv("WA/wa_lists_glacier.csv") %>% 
  select(c(SAMPLING.EVENT.IDENTIFIER))
or_lists_glacier <- read.csv("OR/or_lists_glacier.csv") %>% 
  select(c(SAMPLING.EVENT.IDENTIFIER))
bc_lists1_glacier <- read.csv("BC/bc_lists1_glacier.csv") %>% 
  select(c(SAMPLING.EVENT.IDENTIFIER))
bc_lists2_glacier <- read.csv("BC/bc_lists2_glacier.csv") %>% 
  select(c(SAMPLING.EVENT.IDENTIFIER))
  # Lists for 100m and 2100m
lists100m <- read.csv("lists100m.csv") %>% 
  select(c(SAMPLING.EVENT.IDENTIFIER))
lists2100m <- read.csv("lists2100m.csv") %>% 
  select(c(SAMPLING.EVENT.IDENTIFIER))

###Species set 1, zero fill then filter###
zf_wa1 <- auk_zerofill("WA/1ebd_wa.txt", "WA/sed_wa.txt", collapse = TRUE)
wa1 <- left_join(wa_lists_glacier, 
                 zf_wa1,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))

zf_or1 <- auk_zerofill("OR/1ebd_or.txt", "OR/sed_or.txt", collapse = TRUE)
or1_temp <- left_join(or_lists_glacier, 
                 zf_or1,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))
WTPT <- subset(or1_temp,scientific_name=="Accipiter cooperii") %>%  #Add white-tailed ptarmigan bc it was absent from all checklists
  select(-c(scientific_name,observation_count,species_observed)) %>%
  mutate(scientific_name = "Lagopus leucura",observation_count = "0",species_observed = "FALSE")
or1 <- rbind(or1_temp, WTPT)

zf_bc11 <- auk_zerofill("BC/1ebd_bc1.txt", "BC/sed_bc1.txt", collapse = TRUE)
bc11 <- left_join(bc_lists1_glacier, 
                 zf_bc11,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))

zf_bc21 <- auk_zerofill("BC/1ebd_bc2.txt", "BC/sed_bc2.txt", collapse = TRUE)
bc21 <- left_join(bc_lists2_glacier, 
                  zf_bc21,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))
#Comine regions
glacier_sp1 <- rbind(wa1,or1,bc11,bc21)

#Filter by 100m and 2100m
g1_100m <- left_join(lists100m, 
                  glacier_sp1,by = c("SAMPLING.EVENT.IDENTIFIER" = "checklist_id"))
g1_2100m <- left_join(lists2100m, 
                  glacier_sp1,by = c("SAMPLING.EVENT.IDENTIFIER" = "checklist_id"))

#Write file
write.table(glacier_sp1, file = "glacier_sp1.csv", sep = "\t",
            row.names = FALSE)
write.table(g1_100m, file = "g1_100m.csv", sep = "\t",
            row.names = FALSE)
write.table(g1_2100m, file = "g1_2100m.csv", sep = "\t",
            row.names = FALSE)
#Delete dataframes
rm("wa1","or1","bc11","bc21","zf_wa1","zf_or1","zf_bc11","zf_bc21")

###Species set 2, zero fill then filter###
zf_wa2 <- auk_zerofill("WA/2ebd_wa.txt", "WA/sed_wa.txt", collapse = TRUE)
wa2 <- left_join(wa_lists_glacier, 
                 zf_wa2,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))

zf_or2 <- auk_zerofill("OR/2ebd_or.txt", "OR/sed_or.txt", collapse = TRUE)
or2 <- left_join(or_lists_glacier, 
                 zf_or2,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))

zf_bc12 <- auk_zerofill("BC/2ebd_bc1.txt", "BC/sed_bc1.txt", collapse = TRUE)
bc12 <- left_join(bc_lists1_glacier, 
                  zf_bc12,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))

zf_bc22 <- auk_zerofill("BC/2ebd_bc2.txt", "BC/sed_bc2.txt", collapse = TRUE)
bc22_temp <- left_join(bc_lists2_glacier, 
                  zf_bc22,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))
HEWA <- subset(bc22_temp,scientific_name=="Cardellina pusilla") %>%  #Add hermit warbler bc it was absent from all checklists
  select(-c(scientific_name,observation_count,species_observed)) %>%
  mutate(scientific_name = "Setophaga occidentalis",observation_count = "0",species_observed = "FALSE")
bc22 <- rbind(bc22_temp, HEWA)

#Comine regions
glacier_sp2 <- rbind(wa2,or2,bc12,bc22)
#Filter by 100m and 2100m
g2_100m <- left_join(lists100m, 
                     glacier_sp2,by = c("SAMPLING.EVENT.IDENTIFIER" = "checklist_id"))
g2_2100m <- left_join(lists2100m, 
                      glacier_sp2,by = c("SAMPLING.EVENT.IDENTIFIER" = "checklist_id"))

#Write files
write.table(glacier_sp2, file = "glacier_sp2.csv", sep = "\t",
            row.names = FALSE)
write.table(g2_100m, file = "g2_100m.csv", sep = "\t",
            row.names = FALSE)
write.table(g2_2100m, file = "g2_2100m.csv", sep = "\t",
            row.names = FALSE)
#Delete dataframes
rm("wa2","or2","bc12","bc22","zf_wa2","zf_or2","zf_bc12","zf_bc22","glacier_sp2")

###Species set 3, zero fill then filter###
zf_wa3 <- auk_zerofill("WA/3ebd_wa.txt", "WA/sed_wa.txt", collapse = TRUE)
wa3 <- left_join(wa_lists_glacier, 
                 zf_wa3,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))

zf_or3 <- auk_zerofill("OR/3ebd_or.txt", "OR/sed_or.txt", collapse = TRUE)
or3 <- left_join(or_lists_glacier, 
                 zf_or3,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))

zf_bc13 <- auk_zerofill("BC/3ebd_bc1.txt", "BC/sed_bc1.txt", collapse = TRUE)
bc13 <- left_join(bc_lists1_glacier, 
                  zf_bc13,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))

zf_bc23 <- auk_zerofill("BC/3ebd_bc2.txt", "BC/sed_bc2.txt", collapse = TRUE)
bc23 <- left_join(bc_lists2_glacier, 
                  zf_bc23,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))
#Comine regions
glacier_sp3 <- rbind(wa3,or3,bc13,bc23)
#Filter by 100m and 2100m
g3_100m <- left_join(lists100m, 
                     glacier_sp3,by = c("SAMPLING.EVENT.IDENTIFIER" = "checklist_id"))
g3_2100m <- left_join(lists2100m, 
                      glacier_sp3,by = c("SAMPLING.EVENT.IDENTIFIER" = "checklist_id"))

#Write files
write.table(glacier_sp3, file = "glacier_sp3.csv", sep = "\t",
            row.names = FALSE)
write.table(g3_100m, file = "g3_100m.csv", sep = "\t",
            row.names = FALSE)
write.table(g3_2100m, file = "g3_2100m.csv", sep = "\t",
            row.names = FALSE)
#Delete dataframes
rm("wa3","or3","bc13","bc23","zf_wa3","zf_or3","zf_bc13","zf_bc23","glacier_sp3")

###Species set 4, zero fill then filter###
zf_wa4 <- auk_zerofill("WA/4ebd_wa.txt", "WA/sed_wa.txt", collapse = TRUE)
wa4 <- left_join(wa_lists_glacier, 
                 zf_wa4,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))

zf_or4 <- auk_zerofill("OR/4ebd_or.txt", "OR/sed_or.txt", collapse = TRUE)
or4_temp <- left_join(or_lists_glacier, 
                 zf_or4,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))
ATSP <- subset(or4_temp,scientific_name=="Acanthis flammea") %>%  #Add American Tree Sparrow bc it was absent from all checklists
  select(-c(scientific_name,observation_count,species_observed)) %>%
  mutate(scientific_name = "Spizelloides arborea",observation_count = "0",species_observed = "FALSE")
or4 <- rbind(or4_temp, ATSP)

zf_bc14 <- auk_zerofill("BC/4ebd_bc1.txt", "BC/sed_bc1.txt", collapse = TRUE)
bc14 <- left_join(bc_lists1_glacier, 
                  zf_bc14,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))

zf_bc24 <- auk_zerofill("BC/4ebd_bc2.txt", "BC/sed_bc2.txt", collapse = TRUE)
bc24 <- left_join(bc_lists2_glacier, 
                  zf_bc24,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))
#Comine regions
glacier_sp4 <- rbind(wa4,or4,bc14,bc24)
#Filter by 100m and 2100m
g4_100m <- left_join(lists100m, 
                     glacier_sp4,by = c("SAMPLING.EVENT.IDENTIFIER" = "checklist_id"))
g4_2100m <- left_join(lists2100m, 
                      glacier_sp4,by = c("SAMPLING.EVENT.IDENTIFIER" = "checklist_id"))

#Write file
write.table(glacier_sp4, file = "glacier_sp4.csv", sep = "\t",
            row.names = FALSE)
write.table(g4_100m, file = "g4_100m.csv", sep = "\t",
            row.names = FALSE)
write.table(g4_2100m, file = "g4_2100m.csv", sep = "\t",
            row.names = FALSE)
#Delete dataframes
rm("wa4","or4","bc14","bc24","zf_wa4","zf_or4","zf_bc14","zf_bc24","glacier_sp4")

###Species set 5, zero fill then filter###
zf_wa5 <- auk_zerofill("WA/5ebd_wa.txt", "WA/sed_wa.txt", collapse = TRUE)
wa5 <- left_join(wa_lists_glacier, 
                 zf_wa5,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))

zf_or5 <- auk_zerofill("OR/5ebd_or.txt", "OR/sed_or.txt", collapse = TRUE)
or5 <- left_join(or_lists_glacier, 
                 zf_or5,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))

zf_bc15 <- auk_zerofill("BC/5ebd_bc1.txt", "BC/sed_bc1.txt", collapse = TRUE)
bc15 <- left_join(bc_lists1_glacier, 
                  zf_bc15,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))

zf_bc25 <- auk_zerofill("BC/5ebd_bc2.txt", "BC/sed_bc2.txt", collapse = TRUE)
bc25_temp <- left_join(bc_lists2_glacier, 
                  zf_bc25,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))
CASJ <- subset(bc25_temp,scientific_name=="Corvus brachyrhynchos") %>%  #Add California Scrub-jay bc it was absent from all checklists
  select(-c(scientific_name,observation_count,species_observed)) %>%
  mutate(scientific_name = "Aphelocoma californica",observation_count = "0",species_observed = "FALSE")
bc25 <- rbind(bc25_temp, CASJ)

#Comine regions
glacier_sp5 <- rbind(wa5,or5,bc15,bc25)

#Filter by 100m and 2100m
g5_100m <- left_join(lists100m, 
                     glacier_sp5,by = c("SAMPLING.EVENT.IDENTIFIER" = "checklist_id"))
g5_2100m <- left_join(lists2100m, 
                      glacier_sp5,by = c("SAMPLING.EVENT.IDENTIFIER" = "checklist_id"))
#Write file
write.table(glacier_sp5, file = "glacier_sp5.csv", sep = "\t",
            row.names = FALSE)
write.table(g5_100m, file = "g5_100m.csv", sep = "\t",
            row.names = FALSE)
write.table(g5_2100m, file = "g5_2100m.csv", sep = "\t",
            row.names = FALSE)

#Delete dataframes
rm("wa5","or5","bc15","bc25","zf_wa5","zf_or5","zf_bc15","zf_bc25","glacier_sp5")

###Species set 6, zero fill then filter###
zf_wa6 <- auk_zerofill("WA/6ebd_wa.txt", "WA/sed_wa.txt", collapse = TRUE)
wa6 <- left_join(wa_lists_glacier, 
                 zf_wa6,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))

zf_or6 <- auk_zerofill("OR/6ebd_or.txt", "OR/sed_or.txt", collapse = TRUE)
or6 <- left_join(or_lists_glacier, 
                 zf_or6,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))

zf_bc16 <- auk_zerofill("BC/6ebd_bc1.txt", "BC/sed_bc1.txt", collapse = TRUE)
bc16 <- left_join(bc_lists1_glacier, 
                  zf_bc16,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))

zf_bc26 <- auk_zerofill("BC/6ebd_bc2.txt", "BC/sed_bc2.txt", collapse = TRUE)
bc26 <- left_join(bc_lists2_glacier, 
                  zf_bc26,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))
#Comine regions
glacier_sp6 <- rbind(wa6,or6,bc16,bc26)
#Filter by 100m and 2100m
g6_100m <- left_join(lists100m, 
                     glacier_sp6,by = c("SAMPLING.EVENT.IDENTIFIER" = "checklist_id"))
g6_2100m <- left_join(lists2100m, 
                      glacier_sp6,by = c("SAMPLING.EVENT.IDENTIFIER" = "checklist_id"))

#Write file
write.table(glacier_sp6, file = "glacier_sp6.csv", sep = "\t",
            row.names = FALSE)
write.table(g6_100m, file = "g6_100m.csv", sep = "\t",
            row.names = FALSE)
write.table(g6_2100m, file = "g6_2100m.csv", sep = "\t",
            row.names = FALSE)
#Delete dataframes
rm("wa6","or6","bc16","bc26","zf_wa6","zf_or6","zf_bc16","zf_bc26","glacier_sp6")

###Species set 7, zero fill then filter###
zf_wa7 <- auk_zerofill("WA/7ebd_wa.txt", "WA/sed_wa.txt", collapse = TRUE)
wa7_temp <- left_join(wa_lists_glacier, 
                 zf_wa7,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))
WIPT1 <- subset(wa7_temp,scientific_name=="Actitis macularius") %>%  #Add Willow Ptarmigan bc it was absent from all checklists
  select(-c(scientific_name,observation_count,species_observed)) %>%
  mutate(scientific_name = "Lagopus lagopus",observation_count = "0",species_observed = "FALSE")
wa7 <- rbind(wa7_temp, WIPT1)

zf_or7 <- auk_zerofill("OR/7ebd_or.txt", "OR/sed_or.txt", collapse = TRUE)
or7_temp <- left_join(or_lists_glacier, 
                 zf_or7,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))
WIPT2 <- subset(or7_temp,scientific_name=="Actitis macularius") %>%  #Add Willow Ptarmigan bc it was absent from all checklists
  select(-c(scientific_name,observation_count,species_observed)) %>%
  mutate(scientific_name = "Lagopus lagopus",observation_count = "0",species_observed = "FALSE")
DUGR <- subset(or7_temp,scientific_name=="Actitis macularius") %>%  #Add Dusky Grouse bc it was absent from all checklists
  select(-c(scientific_name,observation_count,species_observed)) %>%
  mutate(scientific_name = "Dendragapus obscurus",observation_count = "0",species_observed = "FALSE")
SPGR <- subset(or7_temp,scientific_name=="Actitis macularius") %>%  #Add Spruce Grouse bc it was absent from all checklists
  select(-c(scientific_name,observation_count,species_observed)) %>%
  mutate(scientific_name = "Falcipennis canadensis",observation_count = "0",species_observed = "FALSE")
or7 <- rbind(or7_temp, WIPT2, DUGR, SPGR)

zf_bc17 <- auk_zerofill("BC/7ebd_bc1.txt", "BC/sed_bc1.txt", collapse = TRUE)
bc17_temp <- left_join(bc_lists1_glacier, 
                  zf_bc17,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))
WIPT3 <- subset(bc17_temp,scientific_name=="Actitis macularius") %>%  #Add Willow Ptarmigan bc it was absent from all checklists
  select(-c(scientific_name,observation_count,species_observed)) %>%
  mutate(scientific_name = "Lagopus lagopus",observation_count = "0",species_observed = "FALSE")
bc17 <- rbind(bc17_temp, WIPT3)

zf_bc27 <- auk_zerofill("BC/7ebd_bc2.txt", "BC/sed_bc2.txt", collapse = TRUE)
bc27 <- left_join(bc_lists2_glacier, 
                  zf_bc27,by = c("SAMPLING.EVENT.IDENTIFIER" = "sampling_event_identifier"))

#Comine regions
glacier_sp7 <- rbind(wa7,or7,bc17,bc27)
#Filter by 100m and 2100m
g7_100m <- left_join(lists100m, 
                     glacier_sp7,by = c("SAMPLING.EVENT.IDENTIFIER" = "checklist_id"))
g7_2100m <- left_join(lists2100m, 
                      glacier_sp7,by = c("SAMPLING.EVENT.IDENTIFIER" = "checklist_id"))

#Write file
write.table(glacier_sp7, file = "glacier_sp7.csv", sep = "\t",
            row.names = FALSE)
write.table(g7_100m, file = "g7_100m.csv", sep = "\t",
            row.names = FALSE)
write.table(g7_2100m, file = "g7_2100m.csv", sep = "\t",
            row.names = FALSE)
#Delete dataframes
rm("wa7","or7","bc17","bc27","zf_wa7","zf_or7","zf_bc17","zf_bc27","glacier_sp7")

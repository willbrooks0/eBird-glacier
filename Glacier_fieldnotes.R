#
# Text analysis Glacier Birds
# June 28, 2021
#
# Will Brooks
#
#####################

library(tidyverse)

###### LOAD DATA (one group at a time) #########
#Species group 1
  wa1 <- read.delim("WA/1ebd_wa.txt")
  or1 <- read.delim("OR/1ebd_or.txt")
  bc11 <- read.delim("BC/1ebd_bc1.txt")
  bc12 <- read.delim("BC/1ebd_bc2.txt")
txt <- rbind(wa1,or1,bc11,bc12)
  rm("wa1","or1","bc11","bc12")
#Species group 2
  wa2 <- read.delim("WA/2ebd_wa.txt")
  or2 <- read.delim("OR/2ebd_or.txt")
  bc21 <- read.delim("BC/2ebd_bc1.txt")
  bc22 <- read.delim("BC/2ebd_bc2.txt")
txt <- rbind(wa2,or2,bc21,bc22)
  rm("wa2","or2","bc21","bc22")
#Species group 3
  wa3 <- read.delim("WA/3ebd_wa.txt")
  or3 <- read.delim("OR/3ebd_or.txt")
  bc31 <- read.delim("BC/3ebd_bc1.txt")
  bc32 <- read.delim("BC/3ebd_bc2.txt")
txt <- rbind(wa3,or3,bc31,bc32)
  rm("wa3","or3","bc31","bc32")
#Species group 4
  wa4 <- read.delim("WA/4ebd_wa.txt")
  or4 <- read.delim("OR/4ebd_or.txt")
  bc41 <- read.delim("BC/4ebd_bc1.txt")
  bc42 <- read.delim("BC/4ebd_bc2.txt")
txt <- rbind(wa4,or4,bc41,bc42)
  rm("wa4","or4","bc41","bc42")
#Species group 5
  wa5 <- read.delim("WA/5ebd_wa.txt")
  or5 <- read.delim("OR/5ebd_or.txt")
  bc51 <- read.delim("BC/5ebd_bc1.txt")
  bc52 <- read.delim("BC/5ebd_bc2.txt")
txt <- rbind(wa5,or5,bc51,bc52)
  rm("wa5","or5","bc51","bc52")
#Species group 6
  wa6 <- read.delim("WA/6ebd_wa.txt")
  or6 <- read.delim("OR/6ebd_or.txt")
  bc61 <- read.delim("BC/6ebd_bc1.txt")
  bc62 <- read.delim("BC/6ebd_bc2.txt")
txt <- rbind(wa6,or6,bc61,bc62)
  rm("wa6","or6","bc61","bc62")
#Species group 7
  wa7 <- read.delim("WA/7ebd_wa.txt")
  or7 <- read.delim("OR/7ebd_or.txt")
  bc71 <- read.delim("BC/7ebd_bc1.txt")
  bc72 <- read.delim("BC/7ebd_bc2.txt")
txt <- rbind(wa7,or7,bc71,bc72)
  rm("wa7","or7","bc71","bc72")

# CHECKLIST FILTERING 
comments <- txt[txt$SPECIES.COMMENTS != "",] # Include only checklists with comments
groups <- comments[comments$GROUP.IDENTIFIER != "",] %>% #Remove duplicate shared checklists
  distinct(GROUP.IDENTIFIER, .keep_all= TRUE)
singles <- comments[comments$GROUP.IDENTIFIER == "",] #Keep single person checklists
comments2 <- rbind(groups,singles)                    #Combine single and shared checklists

# GENERATE KEYWORD VARIABLES
words <- comments2 %>% 
  mutate(glacier = grepl("glacier|glac. ", ignore.case = TRUE, SPECIES.COMMENTS)) %>% 
  mutate(snowfield = grepl("snowfield|snow field", ignore.case = TRUE, SPECIES.COMMENTS)) %>% 
  mutate(ice = grepl(" ice|Ice", SPECIES.COMMENTS)) %>% 
  mutate(snow = grepl(" snow|Snow", SPECIES.COMMENTS)) %>% 
  mutate(ice_worm = grepl("iceworm|ice worm", ignore.case = TRUE,SPECIES.COMMENTS)) %>% 
  mutate(feed = grepl("Feed|feed| eat|Eat|forag|Forag", SPECIES.COMMENTS)) %>% 
  mutate(worm = grepl("worm", ignore.case = TRUE,SPECIES.COMMENTS)) %>% 
  mutate(heard = grepl("heard only|not seen", ignore.case = TRUE,SPECIES.COMMENTS)) %>% 
  mutate(flyover = grepl("fly over|flying over|flyover|flew over", ignore.case = TRUE,SPECIES.COMMENTS))

# Count habitat use
aggregate(words$glacier, by=list(words$COMMON.NAME), FUN=sum)
aggregate(words$snowfield, by=list(words$COMMON.NAME), FUN=sum)

# Count ice worm feeding
aggregate(words$ice_worm, by=list(words$COMMON.NAME), FUN=sum)
aggregate(words$worm, by=list(words$COMMON.NAME), FUN=sum)

# FILTER BY HABITAT USINGS (1) KEYWORDS
glacier <- subset(words, glacier != "FALSE") #subset to include only keyword
snowfield <- subset(words, snowfield != "FALSE") #subset to include only keyword
gl_sf <- rbind(glacier,snowfield) 

#Count behaviors
aggregate(gl_sf$feed, by=list(gl_sf$COMMON.NAME), FUN=sum) # count feeding behavior
aggregate(gl_sf$flyover, by=list(gl_sf$COMMON.NAME), FUN=sum) # count flyover behavior
aggregate(gl_sf$heard, by=list(gl_sf$COMMON.NAME), FUN=sum) # count heard only

#Count breeding codes
subset(gl_sf, BREEDING.CODE == "F") %>% count(COMMON.NAME) # count flyovers
subset(gl_sf, BREEDING.CODE == "CF") %>% count(COMMON.NAME) # count carrying food

# FILTER BY HABITAT USING (2) SPATIALLY FILTERED CHECKLISTS
filter <- read.csv("lists100m.csv")    # Load spatially selected data
words_filter <- left_join(filter,                 #filter text data
                        words,by = "SAMPLING.EVENT.IDENTIFIER") 

#Count behaviors
aggregate(words_filter$feed, by=list(words_filter$COMMON.NAME), FUN=sum) # count feeding behavior
aggregate(words_filter$flyover, by=list(words_filter$COMMON.NAME), FUN=sum) # count flyover behavior
aggregate(words_filter$heard, by=list(words_filter$COMMON.NAME), FUN=sum) # count heard only

#Count breeding codes
words_filter <- left_join(filter,                 #filter text data
                          txt,by = "SAMPLING.EVENT.IDENTIFIER") 
subset(words_filter, BREEDING.CODE == "F") %>% count(COMMON.NAME) # count flyovers
subset(words_filter, BREEDING.CODE == "CF") %>% count(COMMON.NAME) # count carrying food

setwd("~/Desktop/Research/Winter_vegloss_qld/Data/Version12_redo/RE_selection")

library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(tokenizers)
library(stringi)

##read in REDD table, qld floral list, and Peggys 2008 GHFF diet species list 
redd_full<- read.csv("reg_ecos_v12_expanded20210901_cl.csv", header = TRUE)

#fix any non UTF8 characters and delete them 
redd_fix<- redd_full %>%
  mutate_at(vars(veg_description), function(x){gsub('[^ -~]', '', x)})

#make character vector of fixed description
veg_fix<- as.character(redd_fix$veg_description)

#trim the whitespaces of the veg description
veg_trim<- trimws(veg_fix, which = c( "left"), whitespace = "[\n]")

#put the new trimmed descr column into the redd table 
redd_full$veg_description2<- veg_trim
redd_full[,2]<- NULL

#make re_id character type 
redd_full$re_id<- as.character(redd_full$re_id)


########################## Species extraction of all QLD flora ##############################
qld_full<- read.csv("qldplantcensus2020_fulldataset_v2.csv", header = TRUE)

#make dataframe of full scientific angiosperm names and make a 2nd species column bc some here only list subspecies
#but the redd table does not list them as subspecies (e.g. Syncarpia glomulifera is only lisited as sub sp here)
names<- qld_full[, c("Taxon_Name", "Genus", "Species")]

#make list of full taxon name (subspecies specified)
Species1<- as.data.frame (names$Taxon_Name)
colnames(Species1)[1] = "Species"
##make list of simple species listed (no subspeceis listed)
Species2<- as.data.frame(paste(names$Genus, names$Species))
colnames(Species2)[1] = "Species"


#### set up for abbreviations variations in the REDD table (sometimes genus is not spelled out)
#pull out the genera to copy for abbreviation 
genera<- as.data.frame(names$Genus)
colnames(genera)[1] = "Genus"


#pull out first letter and add a period 
genera$abbrev<- substr( genera$Genus, 1,1)
genera$abbrev<- paste(genera$abbrev,".", sep ="")

#paste together the abbreviated species for fully spelled out subspecies names 
taxonsp<- separate(names, Taxon_Name, into = c("Genus", "Species"), sep = "\\s", extra = "merge")
abbrevs1<- as.data.frame(paste(genera$abbrev, taxonsp$Species))
colnames(abbrevs1)[1] = "Species"

#paste together the abbreviated species with simple name, no subspecies specified
abbrevs2<- as.data.frame(paste(genera$abbrev, names$Species))
colnames(abbrevs2)[1] = "Species"

########################### EXTRA SPECIES DESCRIPTIONS IN REDD TABLE ##############################
#besides the full scientific species names there are extra phrases!! 

#need to make the combo of genus plus spp. bc they appear like that in REDD table
generaspp<- as.data.frame(paste(genera$Genus, "spp."))
colnames(generaspp)[1] <- "Species"

#bind the possible species list for matching to redd table
list_full<- rbind(Species1, Species2, abbrevs1, abbrevs2, generaspp)

list_full_unique<-as.data.frame(unique(list_full))

############################ Alex's fancy code, he truly is awesome #################### 
# Alex's matching function to find species and keep them in correct order
matching <- paste((list_full_unique[,1]), collapse="|")
sp_extract <- stringr::str_extract_all((redd_full$veg_description2), matching) # That will take time

#This will check the number of columns needed
max_col <- lapply(sp_extract, length) %>%
  unlist() %>%
  max()

# This will padd the lines with fewer elements
empty_col <- function(x, n){
  if(length(x)<n){
    mis <- n-length(x)
    x <- c(x, rep("", mis))
  }
  return(x)
}

# Here you have the final product
regecos_angios <- lapply(sp_extract, function(x){empty_col(x, max_col)}) %>%
  do.call("rbind", .) %>% 
  data.frame()

#rbind to the reids and original veg description
regecos_angio_df<- cbind(redd_full, regecos_angios)

#most reg ecos have been extracted but some do not have any flora species from QLD flora list described in them
#find the reg ecos that have no veg listed
empty<- subset(regecos_angio_df, regecos_angio_df$X1 == "")
#drop all the unneccessary columns (they're all empty)
empty[,c(3:47)]<- NULL

#separate out the odd reg ecos by the first period (shorten the description)
empty<- empty %>% tidyr::separate(veg_description2, into = c("primary_veg","secondary_veg", "BVG_extraveg", "extras"), sep = "[.]")

extras<- as.data.frame(empty$primary_veg)
colnames(extras)[1]<- "Species"

##### RERUN MATCHING FUNCTION TO INCLUDE THE EXTRA REG ECOS ############# 
#rbind all the posible species names combinations (full names with subspecies, full names no subspecies, abbreviations with subsp, abbrevs w/out subsp, genera + spp)
list_full2<- rbind(Species1, Species2, abbrevs1, abbrevs2, generaspp, extras)


list_full_unique2<-as.data.frame(unique(list_full2))
#write.csv(list_full_unique2, "regecos_fullpossible_veglist_unique_20210924.csv", row.names = FALSE)

extra_veg_list<- rbind(forests, forests2, extras)
#write.csv(extras, "regecos_extras_veglist_20210924.csv", row.names = FALSE)

############################ Alex's fancy code, he truly is awesome #################### 
#shortcut for starting here 
#list_full_unique2<- read.csv("regecos_fullpossible_veglist_unique_20210913.csv",  header = TRUE)

# Alex's matching function to find species and keep them in correct order
matching2 <- paste((list_full_unique2[,1]), collapse="|")
sp_extract <- stringr::str_extract_all((redd_full$veg_description2), matching2) # That will take time

#This will check the number of columns needed
max_col <- lapply(sp_extract, length) %>%
  unlist() %>%
  max()

# This will padd the lines with fewer elements
empty_col <- function(x, n){
  if(length(x)<n){
    mis <- n-length(x)
    x <- c(x, rep("", mis))
  }
  return(x)
}

# Here you have the final product
regecos_angios <- lapply(sp_extract, function(x){empty_col(x, max_col)}) %>%
  do.call("rbind", .) %>% 
  data.frame()

#rbind to the reids and original veg description
regecos_angio_df<- cbind(redd_full, regecos_angios)



#clean columns, write csv 
regecos_angio_df15<- regecos_angio_df[, - c(18:47)]
regecos_angio_df10<- regecos_angio_df[, - c(13:47)]
regecos_angio_df5<- regecos_angio_df[, - c(8:47)]
#write.csv(regecos_angio_df, "regecos_expandedspdf_V12_allsp_20210924.csv", row.names = FALSE)

###########################################################################################################
##################### Validate that all species were pulled out appropriately  ############################
###########################################################################################################


#use tokenize package to pull out all words in the REDD table descriptions 
t<-tokenize_words(veg_trim, lowercase = FALSE) 
t_df<- as.data.frame(unlist(t))
colnames(t_df)[1]<- "words"

#remove the numbers, usually from the BVGM description 
countsabc<- as.data.frame(gsub('[[:digit:]]+', "", t_df$words))
colnames(countsabc)[1]<- "words"

#find the frequency of words used 
counts_freq <- as.data.frame(table(countsabc))

counts_rare<- subset(counts_freq, counts_freq$Freq <=2)

#take out the weird punctuation and empty spaces that got pulled out
counts_rare2<- subset(counts_rare, counts_rare$countsabc != "" & counts_rare$countsabc != "..b" & counts_rare$countsabc != "..c" 
                      & counts_rare$countsabc != "..k"& counts_rare$countsabc != "..x" & counts_rare$countsabc != "..xa" & counts_rare$countsabc != "..xc"  )

###########################################################################################################
########################################### SET UP FOR FUZZY MATCHING ######################################
#heres all the possible species list that I made before 
# species<- read.csv("regecos_fullpossible_veglist_unique_20210924.csv", header = TRUE)
# species<- as.data.frame(trimws(species$Species, which = c( "left"), whitespace = "[\n]")) 
# colnames(species)[1]<- "species"

species<- list_full_unique2
#set up from https://github.com/Adamishere/Fuzzymatching/blob/master/Fuzzy%20String%20Match%20FunctionV1.R 

library(stringdist)

string1<- species$Species
string2<- counts_rare2$countsabc

dataset1<-data.frame(string1,stringsAsFactors =FALSE)
dataset2<-data.frame(string2,stringsAsFactors =FALSE)

fuzzymatch<-function(dat1,dat2,string1,string2,meth,id1,id2){
  #initialize Variables:
  matchfile <-NULL #iterate appends
  x<-nrow(dat1) #count number of rows in input, for max number of runs
  
  #Check to see if function has ID values. Allows for empty values for ID variables, simple list match
  if(missing(id1)){id1=NULL}
  if(missing(id2)){id2=NULL}
  
  #### lowercase text only
  dat1[,string1]<-as.character(tolower(unlist(dat1[,string1])))#force character, if values are factors
  dat2[,string2]<-as.character(tolower(unlist(dat2[,string2])))
  
  #Loop through dat1 dataset iteratively. This is a work around to allow for large datasets to be matched
  #Can run as long as dat2 dataset fits in memory. Avoids full Cartesian join.
  for(i in 1:x) {
    d<-merge(dat1[i,c(string1,id1), drop=FALSE],dat2[,c(string2,id2), drop=FALSE])#drop=FALSE to preserve 1var dataframe
    
    #Calculate String Distatnce based method specified "meth"
    d$dist <- stringdist(d[,string1],d[,string2], method=meth)
    
    #dedupes A_names selects on the smallest distatnce.
    d<- d[order(d[,string1], d$dist, decreasing = FALSE),]
    d<- d[!duplicated(d[,string1]),]
    
    #append demos on matched file
    matchfile <- rbind(matchfile,d)
    # print(paste(round(i/x*100,2),"% complete",sep=''))
    
  }
  return(matchfile)
}

#run the matching 
match<-fuzzymatch(dataset1, dataset2, "string1", "string2", meth = "osa")
#write.csv(match, "fuzzymatch_valid_20210927.csv", row.names = F)

#identify the matches that are only 1 character different
close_match<- subset(match, match$dist == 1)
close_match[,c(1,3)]<- NULL

#pull out the specie phrases written in the REDD table that are misspelled so they can be added for the matching funciton
#close_match_sp<- close_match[,-c(1,3)]
colnames(close_match)[1]<- "Species"
  
##### RERUN MATCHING FUNCTION TO INCLUDE THE EXTRA REG ECOS ############# 
#rbind all the posible species names combinations (full names with subspecies, full names no subspecies, abbreviations with subsp, abbrevs w/out subsp, genera + spp)
list_full3<- rbind(list_full_unique2, close_match)

#write.csv(list_full3, "list_full3_speciesformatching_20210927.csv", row.names = F)
# Alex's matching function to find species and keep them in correct order
matching3 <- paste((list_full3[,1]), collapse="|")
sp_extract <- stringr::str_extract_all((redd_full$veg_description2), matching3) # That will take time

#This will check the number of columns needed
max_col <- lapply(sp_extract, length) %>%
  unlist() %>%
  max()

# This will padd the lines with fewer elements
empty_col <- function(x, n){
  if(length(x)<n){
    mis <- n-length(x)
    x <- c(x, rep("", mis))
  }
  return(x)
}

# Here you have the final product
regecos_angios <- lapply(sp_extract, function(x){empty_col(x, max_col)}) %>%
  do.call("rbind", .) %>% 
  data.frame()

#rbind to the reids and original veg description
regecos_angio_df_valid<- cbind(redd_full, regecos_angios)



#clean columns, write csv 
regecos_angio_df15<- regecos_angio_df_valid[, - c(18:47)]
regecos_angio_df10<- regecos_angio_df_valid[, - c(13:47)]
regecos_angio_df5<- regecos_angio_df_valid[, - c(8:47)]
# write.csv(regecos_angio_df_valid, "regecos_expandedspdf_V12_allsp_valid_20210927.csv", row.names = FALSE)
# write.csv(regecos_angio_df10, "regecos_expandedspdf_V12_10sp_valid_20210927.csv", row.names = FALSE)



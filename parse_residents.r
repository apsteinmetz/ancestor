# parse schowe residents file
library(tidyverse)


# change periods in dates to dashes and deliniate
#accounts for "i" in place if "1" in some years
date_str <-"([0-9]{1,2}(\\.|:) {0,2}[0-9]{0,2}\\. {0,2}([0-9]{2,4}|i[0-9]{3}))|([0-9]{4}|i[0-9]{3})"
#date_str_2 <-"([0-9]{1,2}(\\.|:) {0,2}[0-9]{0,2}\\. {0,2}([0-9]{2,4}|i[0-9]{3}))"
make_yeardate <- Vectorize(function(orig_str){
  if (!is.na(orig_str)){
    if (str_detect(orig_str,date_str)){
      temp_str <- str_extract(orig_str,date_str)
      temp_str <- paste0("ddd",str_replace_all(temp_str,"\\. {0,2}","-"),"ddd")
      orig_str <- str_replace(orig_str,date_str,temp_str)
    }
  }
  return(orig_str)  
}
)  

# house in alt-schowe did not have house numbers but this file places an index number in front.  put in after street.
# add whether in old or new town
house_number_swap <- Vectorize(function(orig_str){
  if (!is.na(orig_str)){
    if (str_detect(orig_str,"^[0-9]+")){
      tokens <- strsplit(orig_str,split = " {1,2}-- ") %>% unlist()
      orig_str <- paste0("Alt -- ", tokens[2]," ",tokens[1])
      #    orig_str <- paste0("Alt -- ", tokens[2]," ",tokens[1]," -- ",tokens[3])
    } else   {
      orig_str <- paste0("Neu -- ",orig_str)
    }
    
  }
  return(orig_str)
}
)


raw_data <- read_lines("data/Schowe_Residents.txt",skip_empty_rows = TRUE) %>%
  enframe(name=NULL)

#expand abbreviations
raw_data_1 <- raw_data %>% 
  mutate(value=str_remove(value,"\n")) %>% 
  mutate(value = str_replace_all(value,"P {0,1}\\.","Platz")) %>% 
  mutate(value = str_replace_all(value,"Hof J {0,1}\\.","Hof J")) %>% 
  mutate(value = str_replace_all(value,"geb {0,1}\\.","nee")) %>% 
  mutate(value = str_replace_all(value,"ev {0,1}(\\.|,)","Evangelical")) %>% 
  mutate(value = str_replace_all(value,"ref {0,1}\\.","Reformed")) %>%
  mutate(value = str_replace_all(value,"gef {0,1}\\.","died")) %>%
  mutate(value = str_replace_all(value,"Krs {0,1}\\.","Krs")) %>%
  mutate(value = str_replace_all(value,"Jug {0,1}\\.","Jugoslavia")) %>%
  mutate(value = str_replace_all(value,"†","died")) %>% 
  mutate(value = str_replace_all(value,", {0,2}sen\\."," Senior")) %>% 
  mutate(value = str_replace_all(value,", {0,2}jun\\."," Junior")) %>% 
  mutate(value = str_replace_all(value,"r\\. {0,2}k\\.","Catholic")) %>% 
  {.}  

#conforms to abbreviation mods above
street_names <- c("Kuzuraer-Gasse", "Wolf-Gasse (Schiller-Gasse)", "Allee-Gasse", 
                  "Debrezin-Gasse", "Lange-Gasse", "Frosch-Gasse", "Jakobsdörfchen (Belgrader-Gasse )", 
                  "Spital", "Rappen-Gasse (Pfälzer-Gasse )", "Haupt-Gasse", "Seil-Gasse", 
                  "Elisabeth-Gasse", "Schiller-Gasse", "Fohlen-Gasse (Zagreber-Gasse)", 
                  "Seil-Gasse – Kleine-Gasse", "Hof Geyer", "Hof Platz Brücker", 
                  "Hof J Wert", "Schlagbrücke")

# extract anything that looks like an address and move it to the address column
# move_address <- function(orig_str)
trailing_punctuation <- "[^[:alnum:]]+$"
leading_punctuation <- "^[^[:alnum:]]+"
address_str <- "(\\.|\\?|;|,|:|·){1,2}[öäüß[:alnum:] \\/–\\-\\(\\)]+$"
house_number_str <- "[0-9\\/)]+[a-z]{0,1}$"

#extract addresses
raw_data_2 <- raw_data_1 %>% 
#  mutate(value=make_yeardate(value)) %>% 
  mutate(value = str_remove(value,trailing_punctuation)) %>% 
  mutate(address = str_extract(value,address_str)) %>%
  mutate(address = str_remove(address,leading_punctuation)) %>% 
  mutate(address = house_number_swap(trimws(address))) %>% 
  filter(!is.na(address)) %>% 
  separate(address,into=c("alt_oder_neu","street"),sep=" -- ") %>% 
  mutate(house=str_extract(street,house_number_str)) %>% 
  mutate(street=trimws(str_remove(street,house_number_str))) %>%
  # if we have a good street name strip address from the base string
  mutate(value = ifelse(street %in% street_names,str_remove(value,address_str),value)) %>%
  {.}

# now convert the anything that looks like a date or into birth then death date
# everthing to the left is going to be name
raw_data_3 <- raw_data_2 %>% 
  mutate(value=make_yeardate(value)) %>% 
  separate(value,into=c("name","birth_date","value"),sep="ddd") %>% 
  mutate(born=str_extract(birth_date,"[0-9]{2,4}$")) %>% 
  mutate(value=make_yeardate(value)) %>% 
  separate(value,into=c("value","death_date","value_2"),sep="ddd") %>% 
  mutate(value=trimws(str_remove(value,"^[;\\*,\\.]"))) %>% 
  mutate(died=str_extract(death_date,"[0-9]{2,4}$")) %>% 
  {.}




raw_data_4 <- raw_data_3 %>%   
 separate(name,
          into=c("last_name","first_name"),
          sep = ", {0,2}",
          extra = "merge") %>%
  mutate(first_name=str_remove(first_name,", $")) %>% 
  separate(value,into=c("faith","fate"),extra = "merge") %>% 
  {.}

  
temp <- raw_data_2 %>% filter(!(street %in% street_names))

raw_data_3 <- raw_data_2 %>% 
  mutate(first_name=ifelse(is.na(first_name),last_name,first_name)) %>% 
  mutate(birth_date=ifelse(is.na(birth_date),first_name,birth_date)) %>% 
  mutate(faith=ifelse(is.na(faith),birth_date,faith)) %>% 
  mutate(outcome=ifelse(is.na(outcome),faith,outcome)) %>% 
  mutate(address=ifelse(is.na(address),outcome,address)) %>%
  mutate(address=str_remove(address,".+(,|:|;|\\.)")) %>% 
  {.}
  
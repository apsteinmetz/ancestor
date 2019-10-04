# parse schowe residents file
library(tidyverse)


# change periods in dates to dashes and deliniate
#accounts for "i" in place if "1" in some years
# vectorize so we can use in dplyr::mutate
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
# vectorize so we can use in dplyr::mutate
house_number_swap <- Vectorize(function(orig_str){
  if (!is.na(orig_str)){
    if (str_detect(orig_str,"^[0-9]+")){
      tokens <- strsplit(orig_str,split = " {1,2}-- ") %>% unlist()
      orig_str <- paste0("AltSchowe -- ", tokens[2]," ",tokens[1])
      #    orig_str <- paste0("Alt -- ", tokens[2]," ",tokens[1]," -- ",tokens[3])
    } else   {
      orig_str <- paste0("NeuSchowe -- ",orig_str)
    }
    
  }
  return(orig_str)
}
)

# read in file and make an extra copy of raw text for later validation
raw_data <- read_lines("data/Schowe_Residents.txt",skip_empty_rows = TRUE) %>%
  enframe(name=NULL) %>% 
  mutate(value=str_remove(value,"\n")) %>% 
  mutate(raw_text = value)

#expand abbreviations
# questions: is "gef." "geßtorben" or "gefallen" (which would be "killed in action")? I translate to "died."
raw_data_1 <- raw_data %>% 
  mutate(value = str_replace_all(value,"P {0,1}\\.","Platz")) %>% 
  mutate(value = str_replace_all(value,"Hof J {0,1}\\.","Hof J")) %>% 
  mutate(value = str_replace_all(value,"geb {0,1}\\.","nee")) %>% 
  mutate(value = str_replace_all(value,"ev {0,1}(\\.|,)","Evangelical")) %>% 
  # "ret." also appears in the religion field but I am not confident enough to assume it is a typo for "ref"
  mutate(value = str_replace_all(value,"ref {0,1}\\.","Reformed")) %>%
  mutate(value = str_replace_all(value,"gef {0,1}\\.","died")) %>%
  mutate(value = str_replace_all(value,"Krs {0,1}\\.","Krs")) %>%
  mutate(value = str_replace_all(value,"Jug {0,1}\\.","Jugoslavia")) %>%
  mutate(value = str_replace_all(value,"†","died")) %>% 
  mutate(value = str_replace_all(value,", {0,2}sen\\."," Senior")) %>% 
  mutate(value = str_replace_all(value,"vermißt","missing")) %>% 
  mutate(value = str_replace_all(value,"ermordet","murdered")) %>% 
  mutate(value = str_replace_all(value,", {0,2}jun\\."," Junior")) %>% 
  mutate(value = str_replace_all(value,"r\\. {0,2}k\\.","Catholic")) %>% 
  {.}  

#conforms to abbreviation mods above
schowe_street_names <- c("Kuzuraer-Gasse", "Wolf-Gasse (Schiller-Gasse)", "Allee-Gasse", 
                  "Debrezin-Gasse", "Lange-Gasse", "Frosch-Gasse", "Jakobsdörfchen (Belgrader-Gasse )", 
                  "Spital", "Rappen-Gasse (Pfälzer-Gasse )", "Haupt-Gasse", "Seil-Gasse", 
                  "Elisabeth-Gasse", "Schiller-Gasse", "Fohlen-Gasse (Zagreber-Gasse)", 
                  "Seil-Gasse – Kleine-Gasse", "Hof Geyer", "Hof Platz Brücker", 
                  "Hof J Wert", "Schlagbrücke")

faiths <- c("Evangelical","Reformed","Catholic")
fates <- c("died","murdered","missing")

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
  separate(address,into=c("village","street"),sep=" -- ") %>% 
  mutate(house=str_extract(street,house_number_str)) %>% 
  mutate(street=trimws(str_remove(street,house_number_str))) %>%
  # if we have a good street name strip address from the base string
  mutate(value = ifelse(street %in% schowe_street_names,str_remove(value,address_str),value)) %>%
  {.}

# now convert the anything that looks like a date or into birth then death date
# everthing to the left is going to be name
raw_data_3 <- raw_data_2 %>% 
  mutate(value=make_yeardate(value)) %>% 
  separate(value,into=c("name","birth_date","value"),sep="ddd") %>% 
  mutate(born=str_extract(birth_date,"[0-9]{2,4}$")) %>% 
  mutate(value=make_yeardate(value)) %>% 
  separate(value,into=c("value","death_date","last_location"),sep="ddd") %>% 
  mutate(value=trimws(str_remove(value,leading_punctuation))) %>% 
  mutate(died=str_extract(death_date,"[0-9]{2,4}$")) %>% 
  {.}



# parse names and faith
raw_data_4 <- raw_data_3 %>%   
 separate(name,
          into=c("last_name","first_name"),
          sep = ", {0,2}",
          extra = "merge") %>%
  mutate(first_name=str_remove(first_name,", $")) %>% 
  separate(value,into=c("faith","fate"),extra = "merge") %>% 
  {.}

# Now it gets to be a tough slog.  Try to clean up some of remaining mess.  
raw_data_5 <- raw_data_4 %>% 
  mutate(fate = ifelse(!is.na(faith) & (is.na(fate) | fate ==""), faith, fate)) %>% 
  mutate(last_location = ifelse(!is.na(faith) & (is.na(last_location) | last_location ==""),fate,last_location)) %>%
  separate(last_location,into = c("last_location","last_district"),sep = " Krs ",extra = "merge") %>%
  mutate(last_location=ifelse(str_detect(last_location,"Lager"),str_extract(last_location,"Lager [a-zA-Z]+"),last_location)) %>%
  mutate(faith = ifelse(faith == "e|ev"),"Evangelical",faith) %>% 
  mutate(fate=ifelse(!(faith %in% faiths),faith,fate)) %>%
  select(-raw_text,everything())
  {.}

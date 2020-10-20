library(knitr)
library(tidyverse)
library(magrittr)
library(kableExtra)
library(ggplot2)

# read the data
ag_data <- read_csv("berries.csv", col_names = TRUE)

# look at number of unique values in each column
ag_data %>% summarize_all(n_distinct) -> aa

## make a list of the columns with only one unique value
bb <- which(aa[1,]==1)

## list the 1-unique valu column names 
cn <- colnames(ag_data)[bb]


##Data selected from the NASS database often has columns without any data or with a single repeated Values.  The berries data had only 8 out of 21 columns containing meaningful data.


# remove the 1-unique columns from the dataset
ag_data %<>% select(-all_of(bb))
aa %<>% select(-all_of(bb)) 

## State name and the State ANSI code are (sort of) redundant
## Just keep the name
ag_data %<>% select(-4)
aa %<>% select(-4) 

kable(head(ag_data)) %>%
  kable_styling(font_size=12)

#This table contains informaton about `r nberry` berries: blueberries, raspberries, and strawberries.
berry <- unique(ag_data$Commodity)
nberry <- length(berry)





# strawberries
sberry <- ag_data %>% filter((Commodity=="STRAWBERRIES") & (Period=="YEAR"))
sberry %<>% select(-c(Period, Commodity))   

## Does every Data Item begin with "
sum(str_detect(sberry$`Data Item`, "^STRAWBERRIES, ")) == length(sberry$`Data Item`)

##cope with data item
# sberry %<>% str_replace_all(sberry$`Data Item`, "STRAWBERRIES ", "STRAWBERRIES,")



## separate data item
sberry %<>% separate(`Data Item`, c("B","type", "meas", "what"), sep = ",") 
sberry %<>% select(-B)

sberry %<>% separate(type,c("b1", "b2"), sep=" - ")
sberry %<>% separate(b1,c("label1", "label2","label3"), sep=" ")
sberry %<>% select(-label1)

index_label2<- str_detect(sberry$label2, "BEARING")

f1 <- function(a,b){
  if(a){
    return(b)
  }else{
    return("")
  }
}

sberry %<>% mutate(type = unlist(map2(index_label2, sberry$label2, f1)))

f2 <- function(a,b){
  if(a){
    return("")
  }else{
    return(b)
  }
}

sberry %<>% mutate(pro = unlist(map2(index_label2, sberry$label2, f2)))

sberry %<>% select(-label2)

sberry[is.na(sberry)] <- "" 

sberry %<>% mutate(pro1=str_trim(paste(pro, label3)))
sberry %<>% mutate(production=str_trim(paste(pro1, b2)))


sberry$production<-str_trim(sberry$production,'left')

sberry %<>% select(-c(pro, label3, b2, pro1))

sberry[is.na(sberry)] <- " "  ## OK now Data Item has been split into parts

## separate domain
sberry %<>% separate(Domain, c("D_left", "D_right"), sep = ", ")
sberry[is.na(sberry)] <- " "

## separate damain category
sberry %<>% separate(`Domain Category`, c("DC_left", "DC_right"), sep = ", ")

head(sberry$DC_left %>% unique(),n=20)
head(sberry$DC_right %>% unique(), n=20)

## separate DC_left first
sberry %<>% separate(DC_left, c("DC_left_l", "DC_left_r"), sep = ": ")

## separate DC_right
sberry %<>% separate(DC_right, c("DC_right_l", "DC_right_r"), sep = ": ") 

sberry[is.na(sberry)] <- " "





# fine and remove redundant columns
paste(sberry$D_left,sberry$DC_left_l) %>% unique
sberry %<>%  select(-DC_left_l) 

sum(sberry$D_right == sberry$DC_right_l)
sberry %<>% select(-DC_right_l)


paste(sberry$D_left, sberry$D_right) %>% unique()
sberry %<>% mutate(D_left = "CHEMICAL", D_left = "")
sberry %<>% mutate(Chemical=paste(D_left, D_right)) 
sberry %<>% select(-c(D_left, D_right)) 

sberry %<>% select(Year, State, type, production, what, meas, DC_left_r, DC_right_r, Chemical, Value )






# Now the problem is that we have entries in both the "what" and "meas" columns
##  that begin  "MEASURED IN"
cnt_1 <- str_detect(sberry$what, "MEASURED IN")
sum(cnt_1)
cnt_2 <- str_detect(sberry$meas, "MEASURED IN")
sum(cnt_2)

f1 <- function(a,b){
  if(a){
    return(b)
  }else{
    return("")
  }
}

## now let's separate the "MEASURED IN" entries in the meas column
## form an index of the entries to be separated out

index_meas <- str_detect(sberry$meas, "MEASURED IN")

## verify the first six values against the dats bberry
sberry %<>% mutate(m_in_1 = unlist(map2(index_meas, sberry$meas, f1))) 

sberry %<>% mutate(meas = str_replace(sberry$meas, "MEASURED IN.*$", ""))
### Check
cnt_3 <- str_detect(sberry$meas, "MEASURED IN")
sum(cnt_3)

## merge production and meas
sberry %<>% separate(meas,c("meas1", "meas2"), sep=" - ")
sberry[is.na(sberry)] <- " "
sberry %<>% mutate(production1=paste(production, meas2))
sberry %<>% select(-c(meas1,meas2,production))

## Now we will do the same thing with the "what" column  
### index of cells to be isolated
index_what <- str_detect(sberry$what, "MEASURED IN")
sum(index_what)

### create a column of the isolated cells
sberry %<>% mutate(m_in_2 = unlist(map2(index_what, sberry$what, f1))) 

###  eliminate the isolated cells from the original column
sberry %<>% mutate(what = str_replace(sberry$what, "MEASURED IN.*$", ""))

### test that theere are no more "MEASURED IN" cells in the original column
cnt_what <- str_detect(sberry$what, "MEASURED IN")
sum(cnt_what)


## Check for overlaps
sberry %<>% mutate(units = str_trim(paste(m_in_1, m_in_2))) 
sberry$units %>% unique()




# now let's clean it up 

sberry$what %>% unique()  ## rename Avg

sberry$production1 %>% unique() ## rename production 

sberry$DC_left_r %>% unique() # rename chemical_family

tmp <- sberry$DC_right_r %>% unique() # rename materials --213

tmp <- sberry$Value %>% unique() # values

tmp <- sberry$units %>% unique() # Measures

sberry %<>% rename(Avg = what)
sberry %<>% rename(production = production1, Chem_family = DC_left_r, Materials = DC_right_r, Measures = units)

colnames(sberry)

sberry %<>% select(Year, State, type, production, 
                   Measures, Avg, Chem_family,
                   Materials, Chemical, Value )


###  these belong in one column
sberry %<>% mutate(Chemical = str_trim(paste(Chem_family, Chemical)))

sberry %<>% select(Year, State, type, production, Avg, Measures, Materials, Chemical, Value)

#write.csv(sberry,file = "sberry.csv",row.names = F)


## deal with the value column
sberry1<-read.csv(file = "sberry.csv",header=T)
sberry1$Value[sberry1$Value=="(D)"]=0
sberry1$Value[sberry1$Value=="(NA)"]=0
sberry1$Value[sberry1$Value=="(Z)"]=0
sberry1$Value<-as.numeric(sberry1$Value)

write.csv(sberry1,file = "sberry1.csv",row.names = F)



# EDA

##explore the relationship among years, state and value
read.csv(file="sberry1.csv",header=T)

measure_mean_state<-sberry1%>%
  group_by(Measures,Year,State)%>%
  summarize(
    val1=mean(Value,na.rm=TRUE)
  )

#measure_mean_state1<- pivot_wider(data=measure_mean_state,names_from =Year, values_from = val1)
#measure_mean_state1[is.na(measure_mean_state1)] <-0

#measure_mean_state2<-filter(measure_mean_state,Measures=="MEASURED IN CWT")

###MEASURED IN $
measure_mean_state_mea1<-filter(measure_mean_state,Measures=="MEASURED IN $")
ggplot(data=measure_mean_state_mea1)+
  geom_bar(
    mapping=aes(x=State,y=val1,fill=Year),stat = "identity", position = position_dodge(0.5))+
  theme(axis.text.x = element_text(angle = 70, hjust = 0.5,vjust = 0.5,color = "black",size=5))

###MEASURED IN CWT
measure_mean_state_mea2<-filter(measure_mean_state,Measures=="MEASURED IN CWT")
ggplot(data=measure_mean_state_mea2)+
  geom_bar(
    mapping=aes(x=State,y=val1,fill=Year),stat = "identity", position = position_dodge(0.5))+
  theme(axis.text.x = element_text(angle = 70, hjust = 0.5,vjust = 0.5,color = "black",size=5))


###MEASURED IN CWT / ACRE
measure_mean_state_mea3<-filter(measure_mean_state,Measures=="MEASURED IN CWT / ACRE")
ggplot(data=measure_mean_state_mea3)+
  geom_bar(
    mapping=aes(x=State,y=val1,fill=Year),stat = "identity", position = position_dodge(0.5))+
  theme(axis.text.x = element_text(angle = 70, hjust = 0.5,vjust = 0.5,color = "black",size=5))



##explore the relationship among chemical and value
measure_chemcial<-sberry1 %>% select(Measures, Year, Chemical, Value)
measure_chemcial %<>% filter(Chemical!="")

ggplot(data=measure_chemcial)+
  geom_boxplot(mapping = aes(x=Chemical,y=Value,fill=Year))+
  theme(axis.text.x = element_text(angle = 70, hjust = 0.5,vjust = 0.5,color = "black",size=8))+
  facet_wrap(~ Measures,nrow=2)+
  ylim(0,2e+06)
  
  
## explore the relationship among type and value
measure_type<-sberry1 %>% select(Measures,type, Value)
measure_type %<>% filter(Measures=="MEASURED IN $"|Measures=="MEASURED IN CWT"|Measures=="MEASURED IN CWT / ACRE")

ggplot(data=measure_type)+
  geom_boxplot(mapping = aes(x=type,y=Value))+
  facet_wrap(~ Measures,nrow=2)+
  ylim(0,500000)




        
        
        
  










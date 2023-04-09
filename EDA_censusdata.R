library(readxl)
census_data <- read_excel("D:\\GitHub\\EDA-census_data\\A-1_NO_OF_VILLAGES_TOWNS_HOUSEHOLDS_POPULATION_AND_AREA.xlsx")

library(DT)
head(census_data)

census_data <- census_data[-2,]
head(census_data)

library(dplyr)
names(census_data) #to see the names of the columns

#changed 'Number of villages' column to NV_Inhabited
census_data <- rename(census_data, NV_Inhabited = "Number of villages")
#changed '...8' column to NV_Uninhabited
census_data <- rename(census_data, NV_Uninhabited = "...8")

#changed the population super column to 3 seperate columns 
#P_Total, P_Males and P_Females
census_data <- rename(census_data, P_Total = "Population")
census_data <- rename(census_data, P_Males = "...12")
census_data <- rename(census_data, P_Females = "...13")

names(census_data)
```
## <span style="font-size: 1.15em;">Delete the redundant 1st row</span>
```{r warning=FALSE, message=FALSE}
census_data <- census_data[-1,]
head(census_data)

library(tidyverse)
str(census_data)
glimpse(census_data)
names(census_data)

library(DataExplorer)
plot_missing(census_data)

unique(census_data$`District Code`)
unique(census_data$`India/ State/ Union Territory/ District/ Sub-district`)
unique(census_data$`Total/\r\nRural/\r\nUrban`) 

census_data_filtered <- census_data %>% 
  filter(!grepl("[^0-9]",`District Code`)) 

census_data_filtered <- census_data_filtered %>% 
  filter(!`District Code` %in% c(1,2,3,4,5))

census_data_filtered <- census_data_filtered %>% 
  filter(!grepl("[^0-9]",`Sub District Code`))

options(scipen = 999)

census_data_filtered <- census_data_filtered %>% 
  filter(is.na(`Area\r\n (In sq. km)`) |
           (grepl("^\\d+\\.?\\d*$", `Area\r\n (In sq. km)`)))

head(census_data_filtered$Name) 
census_data_filtered <- census_data_filtered %>% 
  mutate(Name = str_replace_all(Name, "@&", ""))
census_data_filtered <- census_data_filtered %>% 
  mutate(Name = str_replace_all(Name, "\\$", ""))

head(census_data_filtered$Name)  

nrow(census_data)
nrow(census_data_filtered)

census_data_filtered$`State  Code` <- as.factor(census_data_filtered$`State  Code`)
census_data_filtered$`District Code` <- as.factor(census_data_filtered$`District Code`)
census_data_filtered$`Sub District Code` <- as.factor(census_data_filtered$`Sub District Code`)
census_data_filtered$`India/ State/ Union Territory/ District/ Sub-district` <- as.factor(census_data_filtered$`India/ State/ Union Territory/ District/ Sub-district`)
census_data_filtered$`Name` <- as.character(census_data_filtered$`Name`)
census_data_filtered$`Total/\r\nRural/\r\nUrban` <- as.factor(census_data_filtered$`Total/\r\nRural/\r\nUrban`)
census_data_filtered$`NV_Inhabited` <- as.numeric(census_data_filtered$`NV_Inhabited`)
census_data_filtered$`NV_Uninhabited` <- as.numeric(census_data_filtered$`NV_Uninhabited`)
census_data_filtered$`Number of towns` <- as.numeric(census_data_filtered$`Number of towns`)
census_data_filtered$`Number of households` <- as.numeric(census_data_filtered$`Number of households`)
census_data_filtered$`P_Total` <- as.numeric(census_data_filtered$`P_Total`)
census_data_filtered$`P_Males` <- as.numeric(census_data_filtered$`P_Males`)
census_data_filtered$`P_Females` <- as.numeric(census_data_filtered$`P_Females`)
census_data_filtered$`Area\r\n (In sq. km)` <- as.numeric(census_data_filtered$`Area\r\n (In sq. km)`)
census_data_filtered$`Population per sq. km.` <- as.numeric(census_data_filtered$`Population per sq. km.`)


glimpse(census_data_filtered)


variable_names <- colnames(census_data_filtered)

library(purrr)
variable_type <- map_chr(census_data_filtered, class)

description <- c("Official code of each state","Official code of each disctrict","Official code of each sub district", "Classified as State, Union Territory, District, Sub-district, or the entire country - India",
                 "Name of the State, Union Territory, District, Sub-district, or the entire country - India", "Classified as Urban area and Rural area.",
                 "Number of villages inhabited","Number of villages uninhabited","Number of towns in the area", "Number of households in the area","Total population in the area","Total male population in the area",
                 "Total female population in the area", "Total area of the region in square kilometer", "Number of people per square kilometer")

data_dict <- data.frame(variable_names,variable_type,description)
rownames(census_data_filtered) <- NULL
data_dict <- data.frame(data_dict, row.names = 1:ncol(census_data_filtered))



library(DT)
datatable(data_dict)


display_df <- census_data_filtered %>% 
  filter_all(any_vars(is.na(.)))
datatable(display_df)

display_df <- census_data_filtered %>% 
  filter_all(all_vars(is.na(.)))
datatable(display_df)


census_data_filtered <- census_data_filtered %>% 
  filter_all(any_vars(!is.na(.)))

display_df <- census_data_filtered %>% 
  filter_all(all_vars(is.na(.)))
datatable(display_df)

ggplot(data = census_data_filtered, aes(x = `India/ State/ Union Territory/ District/ Sub-district`))+
  geom_bar() 

census_data_dist <- census_data_filtered %>% 
  filter(`India/ State/ Union Territory/ District/ Sub-district` == 'DISTRICT')

hist(census_data_dist$P_Total, breaks = 250,
     main = "Total population Distribution", xlim = c(0,5000000))
boxplot(census_data_dist$`Population per sq. km.`, ylim = c(0,10000))

plot(density(census_data_dist$`Area\r\n (In sq. km)`, na.rm = TRUE), xlim = c(-2000,30000), main = "Area")

census_data_imputed <- census_data_dist
summary(census_data_imputed$`Area\r\n (In sq. km)`)


min_val <- min(census_data_imputed$`Area\r\n (In sq. km)`, na.rm = TRUE)
max_val <- max(census_data_imputed$`Area\r\n (In sq. km)`, na.rm = TRUE)

imputed_vals <- runif(sum(is.na(census_data_imputed$`Area\r\n (In sq. km)`)), min = min_val, max = max_val)

census_data_imputed$`Area\r\n (In sq. km)`[which(is.na(census_data_imputed$`Area\r\n (In sq. km)`))] <- imputed_vals


summary(census_data_imputed$`Area\r\n (In sq. km)`)


summary(census_data_imputed$`Population per sq. km.`)

census_data_imputed$`Population per sq. km.`[which(is.na(census_data_imputed$`Population per sq. km.`))]


pd_imputed_vals <-  census_data_imputed$`Area\r\n (In sq. km)`[which(is.na(census_data_imputed$`Population per sq. km.`))]/census_data_imputed$P_Total[which(is.na(census_data_imputed$`Population per sq. km.`))]


census_data_imputed$`Population per sq. km.`[which(is.na(census_data_imputed$`Population per sq. km.`))] <- pd_imputed_vals

summary(census_data_imputed$`Population per sq. km.`)


boxplot(census_data_dist$`Population per sq. km.`,
        main = "Total population Density\n(Before)", ylim = c(0,6000))

boxplot(census_data_imputed$`Population per sq. km.`,
        main = "Total Population Density\n(After)", ylim = c(0,6000))


plot(density(census_data_dist$`Area\r\n (In sq. km)`, na.rm = TRUE), xlim = c(-2000,30000), main = "Area\n(Before)")
plot(density(census_data_imputed$`Area\r\n (In sq. km)`), xlim = c(-2000,30000), main = "Area\n(After)")


cor(census_data_imputed[7:15])

plot_correlation(census_data_imputed[7:15])

ggplot(census_data_imputed,aes(x=P_Total,y=NV_Inhabited))+
  geom_point()


boxplot(census_data_imputed$`Population per sq. km.`,
        main = "Total Population Density\n(After)", ylim = c(6000,50000))

Recommended_list <- census_data_imputed %>% 
  filter(`Population per sq. km.` > 6000) %>% 
  filter(!duplicated(Name)) %>% 
  pull(Name)


Recommended_list <- Recommended_list[c(-9,-10,-11,-12,-13,-14,-15,-16)]  

Recommended_list <- c(Recommended_list, "Delhi") 
print(Recommended_list)

phase1 <- c("Chandigarh", "Hyderabad", "Mumbai", "Pune", "Chennai", "Delhi")

phase2 <- c("Ludhiana", "Amritsar", "Karnal", "Panipat", "Faridabad", "Saharanpur", "Muzaffarnagar", 
            "Meerut", "Ghaziabad", "Agra", "Lucknow", "Kanpur Nagar", "Allahabad", "Varanasi", "Patna", 
            "Ahmadabad", "Surat", "Nagpur", "Thane", "Mumbai Suburban", "Bangalore")

phase3 <- c("Kargil", "Kishtwar", "Bijnor", "Moradabad", "Jyotiba Phule Nagar", "Firozabad", 
            "Pilibhit", "Shahjahanpur", "Farrukhabad", "Etawah", "Hamirpur", "Mahoba", "Bahraich", 
            "Shrawasti", "Etah", "Kanshiram Nagar", "Saharsa", "Muzaffarpur", "Siwan", "Vaishali", 
            "Samastipur", "Khagaria", "Bhagalpur", "Munger", "Buxar", "Gaya", "Chandel", "Dhubri", 
            "Dakshin Dinajpur", "Maldah", "North Twenty Four Parganas", "Hugli", "Haora", 
            "Kolkata", "Purbi Singhbhum", "Kendrapara", "Morena", "Datia", "Indore", "Akola", 
            "Koppal", "Ramanagara", "Puducherry","Mahamaya Nagar", "Latur")

print(phase1)
print(phase2)
print(phase3)
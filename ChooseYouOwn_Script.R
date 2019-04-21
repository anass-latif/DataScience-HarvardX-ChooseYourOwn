# =============================================================================
# File      :  ChooseYouOwn_Script.R
# TYpe      :  R Script
# Project   :  HarvardX - Data Science Professional Certificate
# Release   :  1.0.0
# Purpose   :  R Script to implement all the code related to
#              Capstone Project - Choose You Own -
#              ==> Chocolat Bar Rating
#              ==> NOTE:   The script execution could take 10-15 minutes to run
#                          Depending on the machine configuration
#
# =============================================================================
# Revision History
#
#  -------------------------------------------------------------------------
#  Version  Date        Author         Comments
#  -------------------------------------------------------------------------
#  1.0.0    2019.04.20  Anass Latif    Initial Creation
#  -------------------------------------------------------------------------
#
# =============================================================================



# =============================================================================
#  BEGIN OF SCRIPT
# =============================================================================



# =============================================================================
#     Step 001 - Configure all variables and session information
# =============================================================================

# Start the chronometer to get the script start time in milliseconds
script_start_time <- proc.time()
script_start_time

# Display the working directory
getwd()

# Display session information for reproductibility
sessionInfo()

# Use s seed number for reproductibility
set.seed(1111)

# Setup precision for decimals
options(digits = 3)



# =============================================================================
#     Step 002 - Install all necessary packages if do not exist
# =============================================================================

# Initialize the list of packages in a vector
package_list <- c("tidyverse",
                  "caret",
                  "lubridate",
                  "gridExtra",
                  "ggplot2",
                  "ggrepel",
                  "ggthemes",
                  "knitr",
                  "kableExtra",
                  "rworldmap",
                  "rattle",
                  "rpart",
                  "rpart.plot",
                  "kernlab",
                  "gbm",
                  "doMC")

# Install the list of packages defined previously
for (pkg in package_list) {
   if(!require(pkg, character.only = TRUE)) install.packages(pkg)
}



# =============================================================================
#     Step 003 - Load all necessary libraries
# =============================================================================

# Load libraries from the list of packages defined previously
for (pkg in package_list) {
   library(pkg, character.only = TRUE)
}



# =============================================================================
#     Step 004 - Download and Generate the Dataset
# =============================================================================

# 004-01 Define the URL for the dataset
chocolate_dataset_url <- "https://raw.githubusercontent.com/anass-latif/DataScience-HarvardX-ChooseYourOwn/master/data/flavors_of_cacao.csv"

# 004-02 Download and read file into raw table while removing non printable characters
chocolate_data <- read_csv(gsub("[^[:print:]]","",chocolate_dataset_url),
                           na = c(""," ","NA"))

# 004-03 Create a Country-Region mapping based on rworldmap package
country_region_data <- countryRegions %>%
   mutate(CountryName = ADMIN,
          CountryCode = ISO3,
          GeoRegion = GEO3) %>%
   filter(!is.na(GeoRegion)) %>%
   select(CountryName,
          CountryCode,
          GeoRegion)



# =============================================================================
#     Step 005 - Understand the original data structure
# =============================================================================

# 005-01 chocolate_data Dataset Structure
str(chocolate_data)

# 005-02 Number of columns in chocolate_data Dataset
num_columns <- ncol(chocolate_data)

num_columns

# 005-03 Number of rows in chocolate_data Dataset
num_rows <- nrow(chocolate_data)

num_rows

# 005-04 Number of missing values in chocolate_data Dataset
num_missings <- sum(is.na(chocolate_data))

num_missings

# 005-05 Rename Columns for more consistency
column_names <- c("CompanyName",
                  "ChocolateBarName",
                  "Reference",
                  "ReviewYear",
                  "CocoaPercentage",
                  "CompanyCountry",
                  "Rating",
                  "BeanType",
                  "BeanOrigin")

names(chocolate_data) <- column_names

# 005-06 Check missing values for each column in chocolate_data Dataset
dataset_missing_Values <- tibble("Column_Name" = c("CompanyName",
                                                   "ChocolateBarName",
                                                   "Reference",
                                                   "ReviewYear",
                                                   "CocoaPercentage",
                                                   "CompanyCountry",
                                                   "Rating",
                                                   "BeanType",
                                                   "BeanOrigin"),
                                 "Missing_Values" = c(sum(is.na(chocolate_data$CompanyName)),
                                                      sum(is.na(chocolate_data$ChocolateBarName)),
                                                      sum(is.na(chocolate_data$Reference)),
                                                      sum(is.na(chocolate_data$ReviewYear)),
                                                      sum(is.na(chocolate_data$CocoaPercentage)),
                                                      sum(is.na(chocolate_data$CompanyCountry)),
                                                      sum(is.na(chocolate_data$Rating)),
                                                      sum(is.na(chocolate_data$BeanType)),
                                                      sum(is.na(chocolate_data$BeanOrigin))))

# 005-07 Display Missing Value counts
dataset_missing_Values %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE)

# 005-08 Dataset Example
head(chocolate_data) %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE,
                 latex_options = "scale_down")



# =============================================================================
#     Step 006 - Preprocess the datasets
# =============================================================================

# 006-01 Standardize the [BeanOrigin] column data based on Country or Sub-Region ISO-3166 standards
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Africa, Carribean, C. Am."] <- "Western Africa|Caribbean|Meso-America"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Burma"] <- "Myanmar"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Carribean"] <- "Caribbean"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Carribean(DR/Jam/Tri)"] <- "Dominican Republic|Jamaica|Trinidad and Tobago"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Central and S. America"] <- "Meso-America|South America"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Colombia, Ecuador"] <- "Colombia|Ecuador"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Congo"] <- "Republic of the Congo"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Cost Rica, Ven"] <- "Costa Rica|Venezuela"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Dom. Rep., Madagascar"] <- "Dominican Republic|Madagascar"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Dominican Rep., Bali"] <- "Dominican Republic|Indonesia"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "DR, Ecuador, Peru"] <- "Dominican Republic|Ecuador|Peru"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Ecuador, Costa Rica"] <- "Ecuador|Costa Rica"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Ecuador, Mad., PNG"] <- "Ecuador|Madagascar|Papua New Guinea"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Ghana & Madagascar"] <- "Ghana|Madagascar"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Ghana, Domin. Rep"] <- "Ghana|Dominican Republic"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Ghana, Panama, Ecuador"] <- "Ghana|Panama|Ecuador"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Gre., PNG, Haw., Haiti, Mad"] <- "Grenada|Papua New Guinea|South Pacific|Haiti|Madagascar"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Guat., D.R., Peru, Mad., PNG"] <- "Guatemala|Dominican Republic|Peru|Madagascar|Papua New Guinea"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Indonesia, Ghana"] <- "Indonesia|Ghana"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Mad., Java, PNG"] <- "Madagascar|Indonesia|Papua New Guinea"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Madagascar & Ecuador"] <- "Madagascar|Ecuador"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Peru(SMartin,Pangoa,nacional)"] <- "Peru"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Peru, Belize"] <- "Peru|Belize"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Peru, Dom. Rep"] <- "Peru|Dominican Republic"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Peru, Ecuador"] <- "Peru|Ecuador"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Peru, Ecuador, Venezuela"] <- "Peru|Ecuador|Venezuela"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Peru, Mad., Dom. Rep."] <- "Peru|Madagascar|Dominican Republic"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Peru, Madagascar"] <- "Peru|Madagascar"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "PNG, Vanuatu, Mad"] <- "Papua New Guinea|Vanuatu|Madagascar"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Trinidad, Ecuador"] <- "Trinidad and Tobago|Ecuador"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Ven, Bolivia, D.R."] <- "Venezuela|Bolivia|Dominican Republic"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Ven, Trinidad, Ecuador"] <- "Venezuela|Trinidad and Tobago|Ecuador"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Ven., Indonesia, Ecuad."] <- "Venezuela|Indonesia|Ecuador"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Ven., Trinidad, Mad."] <- "Venezuela|Trinidad and Tobago|Madagascar"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Ven.,Ecu.,Peru,Nic."] <- "Venezuela|Ecuador|Peru|Nicaragua"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Venez,Africa,Brasil,Peru,Mex"] <- "Venezuela|Western Africa|Brazil|Peru|Mexico"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Venezuela, Carribean"] <- "Venezuela|Caribbean"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Venezuela, Dom. Rep."] <- "Venezuela|Dominican Republic"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Venezuela, Ghana"] <- "Venezuela|Ghana"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Venezuela, Java"] <- "Venezuela|Indonesia"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Venezuela, Trinidad"] <- "Venezuela|Trinidad and Tobago"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Venezuela/ Ghana"] <- "Venezuela|Ghana"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Domincan Republic"] <- "Dominican Republic"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Hawaii"] <- "South Pacific"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Principe"] <- "Sao Tome and Principe"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Sao Tome"] <- "Sao Tome and Principe"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Sao Tome & Principe"] <- "Sao Tome and Principe"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "St. Lucia"] <- "Saint Lucia"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "South America, Africa"] <- "South America|Western Africa"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Tanzania"] <- "United Republic of Tanzania"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Tobago"] <- "Trinidad and Tobago"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Trinidad"] <- "Trinidad and Tobago"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Trinidad, Tobago"] <- "Trinidad and Tobago"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Trinidad-Tobago"] <- "Trinidad and Tobago"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Venezuela"] <- "Venezuela"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Vietnam"] <- "Vietnam"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "Martinique"] <- "Caribbean"
chocolate_data$BeanOrigin[chocolate_data$BeanOrigin == "West Africa"] <- "Western Africa"

# 006-02 Correct the Misspellings in [CompanyCountry] column to be compliant with ISO Codes
chocolate_data$CompanyCountry[chocolate_data$CompanyCountry == "Amsterdam"] <- "Netherlands"
chocolate_data$CompanyCountry[chocolate_data$CompanyCountry == "Domincan Republic"] <- "Dominican Republic"
chocolate_data$CompanyCountry[chocolate_data$CompanyCountry == "Eucador"] <- "Ecuador"
chocolate_data$CompanyCountry[chocolate_data$CompanyCountry == "Niacragua"] <- "Nicaragua"
chocolate_data$CompanyCountry[chocolate_data$CompanyCountry == "St. Lucia"] <- "Saint Lucia"
chocolate_data$CompanyCountry[chocolate_data$CompanyCountry == "Sao Tome"] <- "Sao Tome and Principe"
chocolate_data$CompanyCountry[chocolate_data$CompanyCountry == "Scotland"] <- "United Kingdom"
chocolate_data$CompanyCountry[chocolate_data$CompanyCountry == "U.K."] <- "United Kingdom"
chocolate_data$CompanyCountry[chocolate_data$CompanyCountry == "U.S.A."] <- "United States of America"
chocolate_data$CompanyCountry[chocolate_data$CompanyCountry == "Venezuela"] <- "Venezuela"
chocolate_data$CompanyCountry[chocolate_data$CompanyCountry == "Wales"] <- "United Kingdom"
chocolate_data$CompanyCountry[chocolate_data$CompanyCountry == "Martinique"] <- "France"

# 006-03 Group the [BeanType] by bean species
group_criollo <- c("Criollo",
                   "Criollo (Amarru)",
                   "Criollo (Ocumare)",
                   "Criollo (Ocumare 61)",
                   "Criollo (Ocumare 67)",
                   "Criollo (Ocumare 77)",
                   "Criollo (Porcelana)",
                   "Criollo (Wild)")

group_forastero <- c("Forastero",
                     "Forastero (Amelonado)",
                     "Forastero (Arriba)",
                     "Forastero (Arriba) ASS",
                     "Forastero (Arriba) ASSS",
                     "Forastero (Catongo)",
                     "Forastero (Parazinho)")

group_nacional <- c("Forastero (Nacional)",
                    "Nacional",
                    "Nacional (Arriba)")

group_trinitario <- c("Trinitario",
                      "Trinitario (Amelonado)",
                      "Trinitario (Scavina)")

group_blend <- c("Amazon",
                 "Amazon mix",
                 "Amazon, ICS",
                 "Blend",
                 "Blend-Forastero,Criollo",
                 "Criollo, +",
                 "Criollo, Forastero",
                 "Criollo, Trinitario",
                 "Forastero, Trinitario",
                 "Forastero(Arriba, CCN)",
                 "Trinitario (85% Criollo)",
                 "Trinitario, Criollo",
                 "Trinitario, Forastero",
                 "Trinitario, Nacional",
                 "Trinitario, TCGA",
                 "Beniano",
                 "CCN51",
                 "EET",
                 "Matina")

chocolate_data$BeanType[which(chocolate_data$BeanType %in% group_criollo)] <- "Criollo"
chocolate_data$BeanType[which(chocolate_data$BeanType %in% group_forastero)] <- "Forastero"
chocolate_data$BeanType[which(chocolate_data$BeanType %in% group_nacional)] <- "Nacional"
chocolate_data$BeanType[which(chocolate_data$BeanType %in% group_trinitario)] <- "Trinitario"
chocolate_data$BeanType[which(chocolate_data$BeanType %in% group_blend)] <- "Blend"

# 006-04 Replace missing values for [BeanType] to [Blend] if
#     - Multiple countries for [BeanOrigin] AND [BeanType] is na
#     - [ChocolateBarName] contains [Blend] or [blend] or [,] AND [BeanType] is na
chocolate_data$BeanType[is.na(chocolate_data$BeanType) & str_detect(chocolate_data$BeanOrigin, "\\|")] <- "Blend"
chocolate_data$BeanType[is.na(chocolate_data$BeanType) & str_detect(chocolate_data$ChocolateBarName, "blend")] <- "Blend"
chocolate_data$BeanType[is.na(chocolate_data$BeanType) & str_detect(chocolate_data$ChocolateBarName, "Blend")] <- "Blend"
chocolate_data$BeanType[is.na(chocolate_data$BeanType) & str_detect(chocolate_data$ChocolateBarName, "\\,")] <- "Blend"

# 006-05 Create a new column [RatingClass] to be used as a Visualization
chocolate_data <- chocolate_data %>%
   mutate(RatingClass = case_when(Rating >= 1.00 & Rating <= 1.75  ~ "10-Unpleasant",
                                  Rating >= 2.00 & Rating <= 2.75  ~ "20-Disappointing",
                                  Rating >= 3.00 & Rating <= 3.75  ~ "30-Satisfactory",
                                  Rating >= 4.00 & Rating <= 4.75  ~ "40-Premium",
                                  Rating > 4.75  ~ "50-Elite"))

# 006-06 separate [BeanOrigin] into rows using pipe-separator
chocolate_data <- chocolate_data %>%
   separate_rows(BeanOrigin,
                 sep = "\\|",
                 convert = FALSE)

# 006-07 Create a new column [BeanOriginGeoRegion] providing the Geo-region based on the [BeanOrigin]
chocolate_data <- chocolate_data %>%
   left_join(country_region_data, by = c("BeanOrigin" = "CountryName")) %>%
   mutate(BeanOriginGeoRegion = if_else(condition = is.na(GeoRegion),
                                        true = BeanOrigin,
                                        false = GeoRegion,
                                        missing = BeanOrigin)) %>%
   select(- CountryCode,
          - GeoRegion)

# 006-08 Create a new column [CompanyGeoRegion] providing the Geo-region based on the [CompanyCountry]
chocolate_data <- chocolate_data %>%
   left_join(country_region_data, by = c("CompanyCountry" = "CountryName")) %>%
   mutate(CompanyGeoRegion = if_else(condition = is.na(GeoRegion),
                                        true = CompanyCountry,
                                        false = GeoRegion,
                                        missing = CompanyCountry)) %>%
   select(- CountryCode,
          - GeoRegion)

# 006-09 Convert [CocoaPercentage] column to Numeric by removing % sign and rounding to the nearest integer
chocolate_data$CocoaPercentage <- as.numeric(sub("%", "", chocolate_data$CocoaPercentage, fixed = TRUE))
chocolate_data$CocoaPercentage <- round(chocolate_data$CocoaPercentage, digits = 0)

# 006-10 Convert [Rating] column to Numeric
chocolate_data$Rating <- as.numeric(chocolate_data$Rating)

# 006-11 Convert all other columns to Factor
chocolate_data$CompanyName <- as.factor(chocolate_data$CompanyName)
chocolate_data$CompanyCountry <- as.factor(chocolate_data$CompanyCountry)
chocolate_data$CompanyGeoRegion <- as.factor(chocolate_data$CompanyGeoRegion)
chocolate_data$BeanType <- as.factor(chocolate_data$BeanType)
chocolate_data$BeanOrigin <- as.factor(chocolate_data$BeanOrigin)
chocolate_data$BeanOriginGeoRegion <- as.factor(chocolate_data$BeanOriginGeoRegion)
chocolate_data$ReviewYear <- as.factor(chocolate_data$ReviewYear)
chocolate_data$RatingClass <- as.factor(chocolate_data$RatingClass)

# 006-12 Generate a new clean dataset by ordering the columns and removing unused ones [Reference]
chocolate_data_clean <- chocolate_data %>%
   select(ChocolateBarName,
          CompanyName,
          CompanyCountry,
          CompanyGeoRegion,
          BeanType,
          BeanOrigin,
          BeanOriginGeoRegion,
          CocoaPercentage,
          ReviewYear,
          Rating,
          RatingClass)

# 006-13 chocolate_data Dataset Example after preprocessing
head(chocolate_data_clean) %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE,
                 latex_options = "scale_down")



# =============================================================================
#     Step 007 - Understand the final data structure
# =============================================================================

# 007-01 chocolate_data_clean Dataset Structure
str(chocolate_data_clean)

# 007-02 Number of columns in chocolate_data_clean Dataset
num_columns <- ncol(chocolate_data_clean)

num_columns

# 007-03 Number of rows in chocolate_data_clean Dataset
num_rows <- nrow(chocolate_data_clean)

num_rows

# 007-04 Check chocolate_data_clean Dataset structure
dataset_structure <- tibble("Feature_Name" = c("ChocolateBarName",
                                               "CompanyName",
                                               "CompanyCountry",
                                               "CompanyGeoRegion",
                                               "BeanType",
                                               "BeanOrigin",
                                               "BeanOriginGeoRegion",
                                               "CocoaPercentage",
                                               "ReviewYear",
                                               "Rating",
                                               "RatingClass"),
                            "Data_Type" = c(class(chocolate_data_clean$ChocolateBarName),
                                            class(chocolate_data_clean$CompanyName),
                                            class(chocolate_data_clean$CompanyCountry),
                                            class(chocolate_data_clean$CompanyGeoRegion),
                                            class(chocolate_data_clean$BeanType),
                                            class(chocolate_data_clean$BeanOrigin),
                                            class(chocolate_data_clean$BeanOriginGeoRegion),
                                            class(chocolate_data_clean$CocoaPercentage),
                                            class(chocolate_data_clean$ReviewYear),
                                            class(chocolate_data_clean$Rating),
                                            class(chocolate_data_clean$RatingClass)),
                            "Distinct_Values" = c(n_distinct(chocolate_data_clean$ChocolateBarName),
                                                  n_distinct(chocolate_data_clean$CompanyName),
                                                  n_distinct(chocolate_data_clean$CompanyCountry),
                                                  n_distinct(chocolate_data_clean$CompanyGeoRegion),
                                                  n_distinct(chocolate_data_clean$BeanType),
                                                  n_distinct(chocolate_data_clean$BeanOrigin),
                                                  n_distinct(chocolate_data_clean$BeanOriginGeoRegion),
                                                  n_distinct(chocolate_data_clean$CocoaPercentage),
                                                  n_distinct(chocolate_data_clean$ReviewYear),
                                                  n_distinct(chocolate_data_clean$Rating),
                                                  n_distinct(chocolate_data_clean$RatingClass)),
                            "Missing_Values" = c(sum(is.na(chocolate_data_clean$ChocolateBarName)),
                                                 sum(is.na(chocolate_data_clean$CompanyName)),
                                                 sum(is.na(chocolate_data_clean$CompanyCountry)),
                                                 sum(is.na(chocolate_data_clean$CompanyGeoRegion)),
                                                 sum(is.na(chocolate_data_clean$BeanType)),
                                                 sum(is.na(chocolate_data_clean$BeanOrigin)),
                                                 sum(is.na(chocolate_data_clean$BeanOriginGeoRegion)),
                                                 sum(is.na(chocolate_data_clean$CocoaPercentage)),
                                                 sum(is.na(chocolate_data_clean$ReviewYear)),
                                                 sum(is.na(chocolate_data_clean$Rating)),
                                                 sum(is.na(chocolate_data_clean$RatingClass))))

# 007-05 Display Missing Value Counts
dataset_structure %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE)



# =============================================================================
#     Step 008 - Explore and visualize the data
# =============================================================================

# 008-01 What is the overall Chocolate Bar Ratings distribution?
ratings_mu <- mean(chocolate_data_clean$Rating)

chocolate_data_clean %>%
   ggplot(aes(Rating)) +
   geom_histogram(color = "darkblue",
                  fill = "lightblue",
                  binwidth = 0.25) +
   geom_vline(xintercept = ratings_mu,
              col = "red",
              linetype = "dashed") +
   labs(title = "Overall Chocolate Bar Rating Distribution",
        x = "Rating",
        y = "Frequency") +
   theme_economist() +
   scale_color_economist() +
   theme(plot.title = element_text(size = 11, color = "darkblue", hjust = 0.5))

# 008-02 What is the Chocolate Bar Rating Class distribution?
chocolate_data_clean %>%
   ggplot(aes(RatingClass)) +
   geom_bar(color = "darkblue",
            fill = "lightblue") +
   coord_flip() +
   labs(title = "Chocolate Bar Rating Class Distribution",
        x = "Rating Scale",
        y = "Frequency") +
   theme_economist() +
   scale_color_economist() +
   theme(plot.title = element_text(size = 11, color = "darkblue", hjust = 0.5),
         axis.text.x = element_text(size = 8, angle = 90, hjust = 1),
         legend.position="none")

# 008-03 What is the Chocolate Bar Ratings distribution by Cocoa Percentage?
cocoa_pct_mu <- mean(chocolate_data_clean$CocoaPercentage)

chocolate_data_clean %>%
   ggplot(aes(CocoaPercentage)) +
   geom_histogram(color = "darkblue",
                  fill = "lightblue",
                  binwidth = 1) +
   geom_vline(xintercept = cocoa_pct_mu,
              col = "red",
              linetype = "dashed") +
   labs(title = "Chocolate Bar Rating Distribution By Cocoa Percentage",
        x = "Cocoa Percentage",
        y = "Frequency") +
   theme_economist() +
   scale_color_economist() +
   theme(plot.title = element_text(size = 11, color = "darkblue", hjust = 0.5))

# 008-04 Is there any correlation between Chocolate Bar Ratings and Cocoa Percentage?
chocolate_data_clean %>%
   ggplot(aes(y = Rating, x = CocoaPercentage)) +
   geom_jitter(color = "darkblue") +
   geom_smooth(method = "lm") +
   labs(title = "Chocolate Bar Rating VS Cocoa Percentage",
        x = "Cocoa Percentage",
        y = "Rating") +
   theme_economist() +
   scale_color_economist() +
   theme(plot.title = element_text(size = 11, color = "darkblue", hjust = 0.5))

# 008-05 What is the Chocolate Bar Ratings distribution by Bean Type?
chocolate_data_clean %>%
   filter(!is.na(BeanType)) %>%
   group_by(BeanType) %>%
   summarize(Rating_Count = n(),
             Rating_Average = mean(Rating)) %>%
   ggplot(aes(x = reorder(BeanType, Rating_Count),
              y = Rating_Count)) +
   geom_bar(stat = "identity",
            color = "darkblue",
            fill = "lightblue") +
   labs(title = "Chocolate Bar Rating Distribution By Bean Type",
        x = "Bean Type",
        y = "Frequency") +
   theme_economist() +
   scale_color_economist() +
   theme(plot.title = element_text(size = 11, color = "darkblue", hjust = 0.5),
         axis.text.x = element_text(size = 8, angle = 90, hjust = 1),
         legend.position="none")

# 008-06 What is the Chocolate Bar Ratings distribution by Bean Type?
chocolate_data_clean %>%
   filter(!is.na(BeanType)) %>%
   ggplot(aes(x = BeanType,
              y = Rating)) +
   geom_boxplot(color = "darkblue",
                fill = "lightblue") +
   labs(title = "Chocolate Bar Rating Distribution By Bean Type",
        x = "Bean Type",
        y = "Rating") +
   theme_economist() +
   scale_color_economist() +
   theme(plot.title = element_text(size = 11, color = "darkblue", hjust = 0.5))

# 008-07 What Is The Chocolate Bar Rating Average By Bean Origin (Country)
BeanOriginMap <- chocolate_data_clean %>%
   filter(!is.na(BeanOrigin)) %>%
   group_by(BeanOrigin) %>%
   summarize(Rating_Count = n(),
             Rating_Average = mean(Rating)) %>%
   joinCountryData2Map(joinCode="NAME",
                       nameJoinColumn="BeanOrigin",
                       verbose = FALSE)

mapCountryData(mapToPlot = BeanOriginMap,
               nameColumnToPlot="Rating_Average",
               missingCountryCol = 'lightgrey',
               oceanCol = 'lightblue',
               borderCol = 'darkgrey',
               mapTitle = "Chocolate Bar Rating Average By Bean Origin (Country)",
               colourPalette = "heat",
               catMethod = "fixedWidth")

# 008-08 What is the top 10 ranking of the Chocolate Bar Rating Average by Bean Origin (Country)?
chocolate_data_clean %>%
   filter(!is.na(BeanOrigin)) %>%
   group_by(BeanOrigin) %>%
   summarize(Rating_Count = n(),
             Rating_Average = mean(Rating)) %>%
   filter(Rating_Count >= 10) %>%
   arrange(desc(Rating_Average)) %>%
   head(10) %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE)

# 008-09 What is the Chocolate Bar Ratings distribution by Review Year?
chocolate_data_clean %>%
   filter(!is.na(ReviewYear)) %>%
   ggplot(aes(x = ReviewYear,
              y = Rating)) +
   geom_boxplot(color = "darkblue",
                fill = "lightblue") +
   labs(title = "Chocolate Bar Rating Distribution By Review Year",
        x = "Review Year",
        y = "Rating") +
   theme_economist() +
   scale_color_economist() +
   theme(plot.title = element_text(size = 11, color = "darkblue", hjust = 0.5))

# 008-10 What are the top 10 Chocolate Bar Rating Average by Company Name (at least 10 ratings)?
chocolate_data_clean %>%
   group_by(CompanyName, CompanyCountry) %>%
   summarize(Rating_Count = n(),
             Rating_Average = mean(Rating)) %>%
   filter(Rating_Count >= 10) %>%
   arrange(desc(Rating_Average)) %>%
   head(10) %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE)

# 008-11 What is the Chocolate Bar Rating Average Distribution By Company Location (Country)?
chocolate_data_clean %>%
   group_by(CompanyCountry) %>%
   summarise(Rating_Count = n(),
             Rating_Average = mean(Rating)) %>%
   arrange(desc(Rating_Average)) %>%
   ggplot(aes(y = Rating_Average,
              x = reorder(CompanyCountry, Rating_Average))) +
   geom_point(aes(size = Rating_Count,
                  colour = factor(Rating_Average)),
              alpha = 0.5) +
   labs(title = "Chocolate Bar Rating Average Distribution By Company Location (Country)",
        x = "Country",
        y = "Rating Average") +
   theme_economist() +
   theme(plot.title = element_text(size = 11, color = "darkblue", hjust = 0.5),
         axis.text.x = element_text(size = 7, angle = 90, hjust = 1),
         legend.position="none")

# 008-12 What is the Chocolate Bar Ratings Average Distribution by Company Location (Country)?
CompanyCountryMap <- chocolate_data_clean %>%
   group_by(CompanyCountry) %>%
   summarise(Rating_Count = n(),
             Rating_Average = mean(Rating)) %>%
   joinCountryData2Map(joinCode="NAME",
                       nameJoinColumn="CompanyCountry",
                       verbose = FALSE)

mapCountryData(mapToPlot = CompanyCountryMap,
               nameColumnToPlot="Rating_Average",
               missingCountryCol = 'lightgrey',
               oceanCol = 'lightblue',
               borderCol = 'darkgrey',
               mapTitle = "Chocolate Bar Rating Distribution By Company Location (Country)",
               colourPalette = "diverging",
               catMethod = "fixedWidth")

# 008-13 What is the top 10 ranking of the Chocolate Bar Rating Average by Company Geo-Region (at least 10 ratings)?
chocolate_data_clean %>%
   filter(!is.na(CompanyCountry)) %>%
   group_by(CompanyCountry) %>%
   summarize(Rating_Count = n(),
             Rating_Average = mean(Rating)) %>%
   filter(Rating_Count >= 10) %>%
   arrange(desc(Rating_Average)) %>%
   head(10) %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE)



# =============================================================================
#     Step 009 - Select only the feature that are has less variability and
#                 Filter the dataset by removing all [NAs] rows
# =============================================================================

# 009-01 Create final Dataset
chocolate_final_ds <- chocolate_data_clean %>%
   select(CocoaPercentage,
          BeanType,
          BeanOrigin,
          CompanyCountry,
          ReviewYear,
          RatingClass) %>%
   drop_na()



# =============================================================================
#     Step 010 - Split the dataset into
#                 -  training_set   70%
#                 -  validation_set 30%
# =============================================================================

# 010-01 Create Data Partition Index
set.seed(1111)
sampling_index <- createDataPartition(y = chocolate_final_ds$RatingClass,
                                      times = 1,
                                      p = 0.7,
                                      list = FALSE)

# 010-02 Create training_set
training_set <- chocolate_final_ds[sampling_index, ]

# 010-03 Create validation_set
validation_set <- chocolate_final_ds[-sampling_index, ]

# 010-04 Clean unecessary data frames
rm(chocolate_data,
   sampling_index)



# =============================================================================
#     Step 011 - Model Building, Training and Validation
# =============================================================================

# 011-01 Configure the number of K-folds for cross validation (Repeated CV)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)



# =============================================================================
#     Step 011.01 - Model Building, Training and Validation
#           Model 1 - Support Vector Machine (SVM)
# =============================================================================

# Step 01: Identify the Model
model_1_id <- "SVM"
model_1_desc <- "Support Vector Machine"

# Step 02: Train the Model on the training_set
set.seed(1111)
model_1_fit <- train(RatingClass ~ .,
                     data = training_set,
                     trControl = control,
                     method = "svmRadial")

# Step 03: Get and display the results of the model on the training_set
model_1_fit_results <- model_1_fit$results

model_1_fit_results %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE)

# Step 04: Get the best Accuracy Measure from the Model on the training_set
model_1_fit_accuracy <- max(model_1_fit$results["Accuracy"])

# Step 05: Predict the Model on the validation_set
model_1_predict <- predict(model_1_fit,
                           newdata = validation_set)

# Step 06: Define Confusion Matrix
model_1_confusion_matrix <- confusionMatrix(model_1_predict, validation_set$RatingClass)

# Step 07: Get and display the results of the final model on the validation_set
model_1_predict_results <- model_1_confusion_matrix$overall

model_1_predict_results %>%
   kable(col.names = c("Measure_Value")) %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE)

# Step 08: Get the best Accuracy Measure from the Model on the validation_set
model_1_predict_accuracy <- model_1_predict_results["Accuracy"]

# Step 09: Generate a table to record our approaches and the Measure for the model
model_1_result <- tibble(Model_Id = model_1_id,
                         Model_Method = model_1_desc,
                         Accuracy_On_Training = model_1_fit_accuracy,
                         Accuracy_On_Validation = model_1_predict_accuracy)

# Step 10: Generate an overall table to summarize all the results from all the models.
result_summary <- model_1_result

# Step 11: Display the result Summary
result_summary %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE) %>%
   column_spec(1, width = "5em") %>%
   column_spec(2, width = "20em") %>%
   column_spec(4, bold = TRUE)



# =============================================================================
#     Step 011.02 - Model Building, Training and Validation
#           Model 2 - K-Nearest Neighbors (KNN)
# =============================================================================

# Step 01: Identify the Model
model_2_id <- "KNN"
model_2_desc <- "K-Nearest Neighbors"

# Step 02: Train the Model on the training_set
set.seed(1111)
model_2_fit <- train(RatingClass ~ .,
                     data = training_set,
                     trControl = control,
                     method = "knn")

# Step 03: Get and display the results of the model on the training_set
model_2_fit_results <- model_2_fit$results

model_2_fit_results %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE)

# Step 04: Get the best Accuracy Measure from the Model on the training_set
model_2_fit_accuracy <- max(model_2_fit$results["Accuracy"])

# Step 05: Predict the Model on the validation_set
model_2_predict <- predict(model_2_fit,
                           newdata = validation_set)

# Step 06: Define Confusion Matrix
model_2_confusion_matrix <- confusionMatrix(model_2_predict, validation_set$RatingClass)

# Step 07: Get and display the results of the final model on the validation_set
model_2_predict_results <- model_2_confusion_matrix$overall

model_2_predict_results %>%
   kable(col.names = c("Measure_Value")) %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE)

# Step 08: Get the best Accuracy Measure from the Model on the validation_set
model_2_predict_accuracy <- model_2_predict_results["Accuracy"]

# Step 09: Generate a table to record our approaches and the Measure for the model
model_2_result <- tibble(Model_Id = model_2_id,
                         Model_Method = model_2_desc,
                         Accuracy_On_Training = model_2_fit_accuracy,
                         Accuracy_On_Validation = model_2_predict_accuracy)

# Step 10: Generate an overall table to summarize all the results from all the models.
result_summary <- bind_rows(result_summary,
                            model_2_result)

# Step 11: Display the result Summary
result_summary %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE) %>%
   column_spec(1, width = "5em") %>%
   column_spec(2, width = "20em") %>%
   column_spec(4, bold = TRUE)



# =============================================================================
#     Step 011.03 - Model Building, Training and Validation
#           Model 3 - Random Forest (RF)
# =============================================================================

# Step 01: Identify the Model
model_3_id <- "RF"
model_3_desc <- "Random Forest"

# Step 02: Train the Model on the training_set
set.seed(1111)
model_3_fit <- train(RatingClass ~ .,
                     data = training_set,
                     trControl = control,
                     method = "rf")

# Step 03: Get and display the results of the model on the training_set
model_3_fit_results <- model_3_fit$results

model_3_fit_results %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE)

# Step 04: Get the best Accuracy Measure from the Model on the training_set
model_3_fit_accuracy <- max(model_3_fit$results["Accuracy"])

# Step 05: Predict the Model on the validation_set
model_3_predict <- predict(model_3_fit,
                           newdata = validation_set)

# Step 06: Define Confusion Matrix
model_3_confusion_matrix <- confusionMatrix(model_3_predict, validation_set$RatingClass)

# Step 07: Get and display the results of the final model on the validation_set
model_3_predict_results <- model_3_confusion_matrix$overall

model_3_predict_results %>%
   kable(col.names = c("Measure_Value")) %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE)

# Step 08: Get the best Accuracy Measure from the Model on the validation_set
model_3_predict_accuracy <- model_3_predict_results["Accuracy"]

# Step 09: Generate a table to record our approaches and the Measure for the model
model_3_result <- tibble(Model_Id = model_3_id,
                         Model_Method = model_3_desc,
                         Accuracy_On_Training = model_3_fit_accuracy,
                         Accuracy_On_Validation = model_3_predict_accuracy)

# Step 10: Generate an overall table to summarize all the results from all the models.
result_summary <- bind_rows(result_summary,
                            model_3_result)

# Step 11: Display the result Summary
result_summary %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE) %>%
   column_spec(1, width = "5em") %>%
   column_spec(2, width = "20em") %>%
   column_spec(4, bold = TRUE)



# =============================================================================
#     Step 011.04 - Model Building, Training and Validation
#           Model 4 - Learning Vector Quantization (LVQ)
# =============================================================================

# Step 01: Identify the Model
model_4_id <- "LVQ"
model_4_desc <- "Learning Vector Quantization"

# Step 02: Train the Model on the training_set
set.seed(1111)
model_4_fit <- train(RatingClass ~ .,
                     data = training_set,
                     trControl = control,
                     method = "lvq")

# Step 03: Get and display the results of the model on the training_set
model_4_fit_results <- model_4_fit$results

model_4_fit_results %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE)

# Step 04: Get the best Accuracy Measure from the Model on the training_set
model_4_fit_accuracy <- max(model_4_fit$results["Accuracy"])

# Step 05: Predict the Model on the validation_set
model_4_predict <- predict(model_4_fit,
                           newdata = validation_set)

# Step 06: Define Confusion Matrix
model_4_confusion_matrix <- confusionMatrix(model_4_predict, validation_set$RatingClass)

# Step 07: Get and display the results of the final model on the validation_set
model_4_predict_results <- model_4_confusion_matrix$overall

model_4_predict_results %>%
   kable(col.names = c("Measure_Value")) %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE)

# Step 08: Get the best Accuracy Measure from the Model on the validation_set
model_4_predict_accuracy <- model_4_predict_results["Accuracy"]

# Step 09: Generate a table to record our approaches and the Measure for the model
model_4_result <- tibble(Model_Id = model_4_id,
                         Model_Method = model_4_desc,
                         Accuracy_On_Training = model_4_fit_accuracy,
                         Accuracy_On_Validation = model_4_predict_accuracy)

# Step 10: Generate an overall table to summarize all the results from all the models.
result_summary <- bind_rows(result_summary,
                            model_4_result)

# Step 11: Display the result Summary
result_summary %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE) %>%
   column_spec(1, width = "5em") %>%
   column_spec(2, width = "20em") %>%
   column_spec(4, bold = TRUE)



# =============================================================================
#     Step 011.05 - Model Building, Training and Validation
#           Model 5 - Stochastic Gradient Boosting Machine (GBM)
# =============================================================================

# Step 01: Identify the Model
model_5_id <- "GBM"
model_5_desc <- "Stochastic Gradient Boosting Machine"

# Step 02: Train the Model on the training_set
set.seed(1111)
model_5_fit <- train(RatingClass ~ .,
                     data = training_set,
                     trControl = control,
                     method = "gbm",
                     verbose = FALSE)

# Step 03: Get and display the results of the model on the training_set
model_5_fit_results <- model_5_fit$results

model_5_fit_results %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE)

# Step 04: Get the best Accuracy Measure from the Model on the training_set
model_5_fit_accuracy <- max(model_5_fit$results["Accuracy"])

# Step 05: Predict the Model on the validation_set
model_5_predict <- predict(model_5_fit,
                           newdata = validation_set)

# Step 06: Define Confusion Matrix
model_5_confusion_matrix <- confusionMatrix(model_5_predict, validation_set$RatingClass)

# Step 07: Get and display the results of the final model on the validation_set
model_5_predict_results <- model_5_confusion_matrix$overall

model_5_predict_results %>%
   kable(col.names = c("Measure_Value")) %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE)

# Step 08: Get the best Accuracy Measure from the Model on the validation_set
model_5_predict_accuracy <- model_5_predict_results["Accuracy"]

# Step 09: Generate a table to record our approaches and the Measure for the model
model_5_result <- tibble(Model_Id = model_5_id,
                         Model_Method = model_5_desc,
                         Accuracy_On_Training = model_5_fit_accuracy,
                         Accuracy_On_Validation = model_5_predict_accuracy)

# Step 10: Generate an overall table to summarize all the results from all the models.
result_summary <- bind_rows(result_summary,
                            model_5_result)

# Step 11: Display the result Summary
result_summary %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE) %>%
   column_spec(1, width = "5em") %>%
   column_spec(2, width = "20em") %>%
   column_spec(4, bold = TRUE)



# =============================================================================
#     Step 011.06 - Model Building, Training and Validation
#           Model 6 - Classification And Regression Tree (CART)
# =============================================================================

# Step 01: Identify the Model
model_6_id <- "CART"
model_6_desc <- "Classification And Regression Tree"

# Step 02: Train the Model on the training_set
set.seed(1111)
model_6_fit <- train(RatingClass ~ .,
                     data = training_set,
                     trControl = control,
                     method = "rpart")

# Step 03: Get and display the results of the model on the training_set
model_6_fit_results <- model_6_fit$results

model_6_fit_results %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE)

# Step 04: Get the best Accuracy Measure from the Model on the training_set
model_6_fit_accuracy <- max(model_6_fit$results["Accuracy"])

# Step 05: Predict the Model on the validation_set
model_6_predict <- predict(model_6_fit,
                           newdata = validation_set)

# Step 06: Define Confusion Matrix
model_6_confusion_matrix <- confusionMatrix(model_6_predict, validation_set$RatingClass)

# Step 07: Get and display the results of the final model on the validation_set
model_6_predict_results <- model_6_confusion_matrix$overall

model_6_predict_results %>%
   kable(col.names = c("Measure_Value")) %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE)

# Step 08: Get the best Accuracy Measure from the Model on the validation_set
model_6_predict_accuracy <- model_6_predict_results["Accuracy"]

# Step 09: Generate a table to record our approaches and the Measure for the model
model_6_result <- tibble(Model_Id = model_6_id,
                         Model_Method = model_6_desc,
                         Accuracy_On_Training = model_6_fit_accuracy,
                         Accuracy_On_Validation = model_6_predict_accuracy)

# Step 10: Generate an overall table to summarize all the results from all the models.
result_summary <- bind_rows(result_summary,
                            model_6_result)

# Step 11: Display the result Summary
result_summary %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE) %>%
   column_spec(1, width = "5em") %>%
   column_spec(2, width = "20em") %>%
   column_spec(4, bold = TRUE)



# =============================================================================
#     Step 012 - Summary results on all the models
# =============================================================================

# 012 Display the result Summary
result_summary %>%
   arrange(desc(Accuracy_On_Validation), desc(Accuracy_On_Training)) %>%
   kable() %>%
   kable_styling(bootstrap_options = c("striped","hoved","condensed"),
                 position = "center",
                 font_size = 10,
                 full_width = FALSE) %>%
   column_spec(1, width = "5em") %>%
   column_spec(2, width = "20em") %>%
   column_spec(4, bold = TRUE)



# =============================================================================
#     Step 013 - Model comparison
# =============================================================================

# 013.01 Summarise the model results
definitive_results <- resamples(list(SVM = model_1_fit,
                                     KNN = model_2_fit,
                                     RF  = model_3_fit,
                                     LVQ = model_4_fit,
                                     GBM = model_5_fit,
                                     CART= model_6_fit))

# 013.02 Box-Plot of the result summary
bwplot(definitive_results)

# 013.03 Dot-Plot of the result summary
dotplot(definitive_results)



# =============================================================================
#     Step 014 - Configure all variables and session information
# =============================================================================

# Start the chronometer to get the script end time in milliseconds
script_end_time <- proc.time()

script_duration <- script_end_time - script_start_time

script_duration



# =============================================================================
#  END OF SCRIPT
# =============================================================================

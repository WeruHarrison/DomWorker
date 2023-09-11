library(tidyverse)
library(e1071)

df <- read.csv("C:/Users/user/Desktop/RealEstateModelling/HousingValuationTest-V2.csv")
view(df)
head(df)
str(df)

numerical_vars <- select(df, "YearBuilt", "FullBath", "HalfBath", "BedroomAbvGr", "KitchenAbvGr", "TotalRmsAbvGrd", 
                         "Fireplaces", "GarageCars", "MoSold", "YrSold", "LotArea", "TotalBSF", "LowQualFinSF", 
                         "LivingArea", "PoolArea", "OpenPorchSF", "SalePrice")

categorical_vars <- select(df, "LotShape", "LandContour", "Utilities", "LotConfig", "Slope", "DwellClass",
                           "CentralAir", "GarageType", "PavedDrive", "OverallQuality", "OverallCondition",
                           "ExteriorCondition", "BasementCondition", "KitchenQuality")

# Numerical variables summary statistics
summary_stats <- summary(numerical_vars, na.rm = TRUE)
print(summary_stats)

numerical_sd <- round(sapply(numerical_vars, sd), 4)
print(numerical_sd)
# Calculating the standard deviation for TotalBSF with NA removal
std_dev_TotalBSF <- sd(df$TotalBSF, na.rm = TRUE)
# Calculating the standard deviation for LivingArea with NA removal
std_dev_LivingArea <- sd(df$LivingArea, na.rm = TRUE)

# Numerical variables skewness
numerical_skewness <- round(sapply(numerical_vars, skewness), 4)
# Calculating the skewness for TotalBSF with NA removal
skewness_TotalBSF <- round(skewness(df$TotalBSF, na.rm = TRUE), 4)
# Calculating the skewness for LivingArea with NA removal
skewness_LivingArea <- round(skewness(df$LivingArea, na.rm = TRUE), 4)


# categorical variables frequencies
frequency_tables <- lapply(categorical_vars, table)
print(frequency_tables)

# Histograms of numerical variables
hist_YearBuilt <- ggplot(numerical_vars, aes(x = YearBuilt)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "YearBuilt Histogram", x = "YearBuilt") +
  theme_minimal()

hist_FullBath <- ggplot(numerical_vars, aes(x = FullBath)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "FullBath Histogram", x = "FullBath") +
  theme_minimal()

hist_HalfBath <- ggplot(numerical_vars, aes(x = HalfBath)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "HalfBath Histogram", x = "HalfBath") +
  theme_minimal()

hist_BedroomAbvGr <- ggplot(numerical_vars, aes(x = BedroomAbvGr)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "BedroomAbvGr Histogram", x = "BedroomAbvGr") +
  theme_minimal()

hist_KitchenAbvGr <- ggplot(numerical_vars, aes(x = KitchenAbvGr)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "KitchenAbvGr Histogram", x = "KitchenAbvGr") +
  theme_minimal()

hist_TotalRmsAbvGrd <- ggplot(numerical_vars, aes(x = TotalRmsAbvGrd)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "TotalRmsAbvGrd Histogram", x = "TotalRmsAbvGrd") +
  theme_minimal()

hist_Fireplaces <- ggplot(numerical_vars, aes(x = Fireplaces)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Fireplaces Histogram", x = "Fireplaces") +
  theme_minimal()

hist_GarageCars <- ggplot(numerical_vars, aes(x = GarageCars)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "GarageCars Histogram", x = "GarageCars") +
  theme_minimal()

hist_MoSold <- ggplot(numerical_vars, aes(x = MoSold)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "MoSold Histogram", x = "MoSold") +
  theme_minimal()

hist_YrSold <- ggplot(numerical_vars, aes(x = YrSold)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "YrSold Histogram", x = "YrSold") +
  theme_minimal()

hist_LotArea <- ggplot(numerical_vars, aes(x = LotArea)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  labs(title = "LotArea Histogram", x = "LotArea") +
  theme_minimal()

hist_TotalBSF <- ggplot(numerical_vars, aes(x = TotalBSF)) +
  geom_histogram(binwidth = 500, fill = "blue", color = "black") +
  labs(title = "TotalBSF Histogram", x = "TotalBSF") +
  theme_minimal()

hist_LowQualFinSF <- ggplot(numerical_vars, aes(x = LowQualFinSF)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "LowQualFinSF Histogram", x = "LowQualFinSF") +
  theme_minimal()

hist_LivingArea <- ggplot(numerical_vars, aes(x = LivingArea)) +
  geom_histogram(binwidth = 500, fill = "blue", color = "black") +
  labs(title = "LivingArea Histogram", x = "LivingArea") +
  theme_minimal()

hist_PoolArea <- ggplot(numerical_vars, aes(x = PoolArea)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black") +
  labs(title = "PoolArea Histogram", x = "PoolArea") +
  theme_minimal()

hist_OpenPorchSF <- ggplot(numerical_vars, aes(x = OpenPorchSF)) +
  geom_histogram(binwidth = 20, fill = "blue", color = "black") +
  labs(title = "OpenPorchSF Histogram", x = "OpenPorchSF") +
  theme_minimal()

hist_SalePrice <- ggplot(numerical_vars, aes(x = SalePrice)) +
  geom_histogram(binwidth = 10000, fill = "blue", color = "black") +
  labs(title = "SalePrice Histogram", x = "SalePrice") +
  theme_minimal()

# Creating box plots for each numerical variable
boxplot_YearBuilt <- ggplot(numerical_vars, aes(x = "", y = YearBuilt)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "YearBuilt Box Plot", x = "", y = "YearBuilt") +
  theme_minimal()

boxplot_FullBath <- ggplot(numerical_vars, aes(x = "", y = FullBath)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "FullBath Box Plot", x = "", y = "FullBath") +
  theme_minimal()

boxplot_HalfBath <- ggplot(numerical_vars, aes(x = "", y = HalfBath)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "HalfBath Box Plot", x = "", y = "HalfBath") +
  theme_minimal()

boxplot_BedroomAbvGr <- ggplot(numerical_vars, aes(x = "", y = BedroomAbvGr)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "BedroomAbvGr Box Plot", x = "", y = "BedroomAbvGr") +
  theme_minimal()

boxplot_KitchenAbvGr <- ggplot(numerical_vars, aes(x = "", y = KitchenAbvGr)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "KitchenAbvGr Box Plot", x = "", y = "KitchenAbvGr") +
  theme_minimal()

boxplot_TotalRmsAbvGrd <- ggplot(numerical_vars, aes(x = "", y = TotalRmsAbvGrd)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "TotalRmsAbvGrd Box Plot", x = "", y = "TotalRmsAbvGrd") +
  theme_minimal()

boxplot_Fireplaces <- ggplot(numerical_vars, aes(x = "", y = Fireplaces)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Fireplaces Box Plot", x = "", y = "Fireplaces") +
  theme_minimal()

boxplot_GarageCars <- ggplot(numerical_vars, aes(x = "", y = GarageCars)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "GarageCars Box Plot", x = "", y = "GarageCars") +
  theme_minimal()

boxplot_MoSold <- ggplot(numerical_vars, aes(x = "", y = MoSold)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "MoSold Box Plot", x = "", y = "MoSold") +
  theme_minimal()

boxplot_YrSold <- ggplot(numerical_vars, aes(x = "", y = YrSold)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "YrSold Box Plot", x = "", y = "YrSold") +
  theme_minimal()

boxplot_LotArea <- ggplot(numerical_vars, aes(x = "", y = LotArea)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "LotArea Box Plot", x = "", y = "LotArea") +
  theme_minimal()

boxplot_TotalBSF <- ggplot(numerical_vars, aes(x = "", y = TotalBSF)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "TotalBSF Box Plot", x = "", y = "TotalBSF") +
  theme_minimal()

boxplot_LowQualFinSF <- ggplot(numerical_vars, aes(x = "", y = LowQualFinSF)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "LowQualFinSF Box Plot", x = "", y = "LowQualFinSF") +
  theme_minimal()

boxplot_LivingArea <- ggplot(numerical_vars, aes(x = "", y = LivingArea)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "LivingArea Box Plot", x = "", y = "LivingArea") +
  theme_minimal()

boxplot_PoolArea <- ggplot(numerical_vars, aes(x = "", y = PoolArea)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "PoolArea Box Plot", x = "", y = "PoolArea") +
  theme_minimal()

boxplot_OpenPorchSF <- ggplot(numerical_vars, aes(x = "", y = OpenPorchSF)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "OpenPorchSF Box Plot", x = "", y = "OpenPorchSF") +
  theme_minimal()

boxplot_SalePrice <- ggplot(numerical_vars, aes(x = "", y = SalePrice)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "SalePrice Box Plot", x = "", y = "SalePrice") +
  theme_minimal()


# Identifying missing values
cols <- colnames(df)
count_missing_values <- sapply(df, function(x) sum(is.na(x)))
missing_values <- data.frame(Variable = cols, 
                             NumberOfMissingValues = count_missing_values)
print(missing_values)

# Handling missing values with Method 1: Imputation
df_method1 <- df
# Impute missing values in TotalBSF with the mean of the non-missing values
mean_TotalBSF <- mean(df_method1$TotalBSF, na.rm = TRUE)
print(mean_TotalBSF)
df_method1$TotalBSF[is.na(df_method1$TotalBSF)] <- mean_TotalBSF
# Impute missing values in TotalBSF with the mean of the non-missing values
mean_LivingArea <- mean(df_method1$LivingArea, na.rm = TRUE)
print(mean_LivingArea)
df_method1$LivingArea[is.na(df_method1$LivingArea)] <- mean_LivingArea
# Impute missing values in GarageType with the mode of the non-missing values
garage_table <- c("2Types" = 6, "Attchd" = 870, "Basment" = 19, "BuiltIn" = 88, 
                  "CarPort" = 9, "Detchd" = 384)
garage_mode <- names(which.max(garage_table))
print(garage_mode)
df_method1$GarageType[is.na(df_method1$GarageType)] <- garage_mode
# Identifying missing values after method 1
cols1 <- colnames(df_method1)
count_missing_values_method1 <- sapply(df_method1, function(x) sum(is.na(x)))
missing_values_method1 <- data.frame(Variable = cols1, 
                                     NumberOfMissingValues = count_missing_values_method1)
print(missing_values_method1)

# Handling missing values with Method 2: Deleting missing values
df_method2 <- df
rows_with_na <- rowSums(is.na(df_method2[c("TotalBSF", "LivingArea", "GarageType")])) > 0
df_method2 <- df_method2[!rows_with_na, ]

# Identifying missing values after method 2
cols2 <- colnames(df_method2)
count_missing_values_method2 <- sapply(df_method2, function(x) sum(is.na(x)))
missing_values_method2 <- data.frame(Variable = cols2, 
                                     NumberOfMissingValues = count_missing_values_method2)
print(missing_values_method2)

# Handling missing values with Method 3: Replacing the missing values with a specific value
df_method3 <- df
# Replacing missing values in TotalBSF with "0"
df_method3$TotalBSF[is.na(df_method3$TotalBSF)] <- 0
# Replacing missing values in LivingArea with "0"
df_method3$LivingArea[is.na(df_method3$LivingArea)] <- 0
# Replacing missing values in GarageType with "Unknown"
df_method3$GarageType[is.na(df_method3$GarageType)] <- "Unknown"
# Identifying missing values after method 1
cols3 <- colnames(df_method3)
count_missing_values_method3 <- sapply(df_method3, function(x) sum(is.na(x)))
missing_values_method3 <- data.frame(Variable = cols3, 
                                     NumberOfMissingValues = count_missing_values_method3)
print(missing_values_method3)


# Summary statistics
summary(df$TotalBSF) 
summary(df$LivingArea)
summary(df_method1$TotalBSF) 
summary(df_method1$LivingArea)
summary(df_method2$TotalBSF) 
summary(df_method2$LivingArea)
summary(df_method3$TotalBSF) 
summary(df_method3$LivingArea)


# Transforming right skewed variables using log method
right_skewed_vars <- select(df, "LowQualFinSF", "PoolArea")
right_skewed_vars$LowQualFinSF <- log10(right_skewed_vars$LowQualFinSF + 1) 
right_skewed_vars$PoolArea <- log10(right_skewed_vars$PoolArea + 1)

# Boxplots before transform
ggplot(df) + 
  geom_boxplot(aes(y = LowQualFinSF))+
  labs(title = "LowQualFinSF Box Plot Before Transform", x = "", y = "LowQualFinSF") +
  theme_minimal()

ggplot(df) +
  geom_boxplot(aes(y = PoolArea))+
  labs(title = "PoolArea Box Plot Before Transform", x = "", y = "PoolArea") +
  theme_minimal()

# Boxplots after transform  
ggplot(right_skewed_vars) +
  geom_boxplot(aes(y = LowQualFinSF))+
  labs(title = "LowQualFinSF Box Plot After Transform", x = "", y = "LowQualFinSF") +
  theme_minimal()

ggplot(right_skewed_vars) +
  geom_boxplot(aes(y = PoolArea)) +
  labs(title = "PoolArea Box Plot After Transform", x = "", y = "PoolArea") +
  theme_minimal()

# Calculating before and after skewness
skewness(df$LowQualFinSF)
skewness(right_skewed_vars$LowQualFinSF) 
skewness(df$PoolArea)
skewness(right_skewed_vars$PoolArea)


# Transforming nominal categorical variables
df_method1_with_dummies <- df_method1 %>%
  mutate(
    LandContour_HLS = if_else(LandContour == "HLS", 1, 0),
    LandContour_Low = if_else(LandContour == "Low", 1, 0),
    LandContour_Lv1 = if_else(LandContour == "Lv1", 1, 0),
    Utilities_Allpub = if_else(Utilities == "Allpub", 1, 0),
    LotConfig_CulDSac = if_else(LotConfig == "CulDSac", 1, 0),
    LotConfig_FR2 = if_else(LotConfig == "FR2", 1, 0),
    LotConfig_FR3 = if_else(LotConfig == "FR3", 1, 0),
    LotConfig_Inside = if_else(LotConfig == "Inside", 1, 0),
    Slope_Mod = if_else(Slope == "Mod", 1, 0),
    Slope_Sev = if_else(Slope == "Sev", 1, 0),
    DwellClass_2fmCon = if_else(DwellClass == "2fmCon", 1, 0),
    DwellClass_Duplex = if_else(DwellClass == "Duplex", 1, 0),
    DwellClass_Twnhs = if_else(DwellClass == "Twnhs", 1, 0),
    DwellClass_TwnhsE = if_else(DwellClass == "TwnhsE", 1, 0),
    CentralAir_Y = if_else(CentralAir == "Y", 1, 0),
    GarageType_Attchd = if_else(GarageType == "Attchd", 1, 0),
    GarageType_Basment = if_else(GarageType == "Basment", 1, 0),
    GarageType_BuiltIn = if_else(GarageType == "BuiltIn", 1, 0),
    GarageType_CarPort = if_else(GarageType == "CarPort", 1, 0),
    GarageType_Detchd = if_else(GarageType == "Detchd", 1, 0),
  )

# Transforming ordinal categorical variables as follows:
## ExteriorCondition_Code: 1 = "Gd", 2 = "TA", and 3 = "Fa"
## BasementCondition_Code: 2 = "TA", 1 = "Gd", 3 = "Fa", and 4 = "NB"
## KitchenQuality_Code: 2 = "Gd"3 ="TA" 1 = "Ex" 4 = "Fa"
## PavedDrive_Code: 1 = "Y", 2 = "P", and 3 = "N"
df_method1_with_dummies <- df_method1_with_dummies %>%
  mutate(ExteriorCondition_Code = case_when(
    ExteriorCondition == "Gd" ~ 1,
    ExteriorCondition == "TA" ~ 2,
    ExteriorCondition == "Fa" ~ 3)) %>%
  
  mutate(BasementCondition_Code = case_when(
    BasementCondition == "Gd" ~ 1,
    BasementCondition == "TA" ~ 2,
    BasementCondition == "Fa" ~ 3,
    BasementCondition == "NB" ~ 4)) %>%
  
  mutate(KitchenQuality_Code = case_when(
    KitchenQuality == "Ex" ~ 1,
    KitchenQuality == "Gd" ~ 2,
    KitchenQuality == "TA" ~ 3, 
    KitchenQuality == "Fa" ~ 4)) %>%
  
  mutate(PavedDrive_Code = case_when(
    PavedDrive == "Y" ~ 1,
    PavedDrive == "P" ~ 2,
    PavedDrive == "N" ~ 3))

not_missing_numerical_vars <- select(df_method1_with_dummies, "YearBuilt", "FullBath", "HalfBath", "BedroomAbvGr", "KitchenAbvGr", "TotalRmsAbvGrd", 
                         "Fireplaces", "GarageCars", "MoSold", "YrSold", "LotArea", "TotalBSF", "LowQualFinSF", 
                         "LivingArea", "PoolArea", "OpenPorchSF", "SalePrice")

# Correlation analysis
correlation_matrix <- round(cor(not_missing_numerical_vars), 4)
# Filter correlations exceeding a threshold of 0.6
threshold <- 0.6
high_correlation <- which(correlation_matrix > threshold, arr.ind = TRUE)

# Diagrams for high correlation variables
ggplot(data = not_missing_numerical_vars, aes(x = factor(FullBath), y = LivingArea)) +
  geom_boxplot() +
  labs(title = "Box Plot of LivingArea by FullBath Count",
       x = "FullBath",
       y = "LivingArea")

ggplot(data = not_missing_numerical_vars, aes(x = factor(BedroomAbvGr), y = TotalRmsAbvGrd)) +
  geom_boxplot() +
  labs(title = "Box Plot of TotalRmsAbvGrd by BedroomAbvGr",
       x = "BedroomAbvGr",
       y = "TotalRmsAbvGrd")

ggplot(data = not_missing_numerical_vars, aes(x = factor(TotalRmsAbvGrd), y = LivingArea)) +
  geom_boxplot() +
  labs(title = "Box Plot of LivingArea by TotalRmsAbvGrd",
       x = "TotalRmsAbvGrd",
       y = "LivingArea")

ggplot(data = not_missing_numerical_vars, aes(x = factor(GarageCars), y = SalePrice)) +
  geom_boxplot() +
  labs(title = "Box Plot of SalePrice by GarageCars",
       x = "GarageCars",
       y = "SalePrice")

ggplot(data = not_missing_numerical_vars, aes(x = SalePrice, y = TotalBSF)) +
  geom_point() +
  labs(title = "Scatter Plot of SalePrice and TotalBSF",
       x = "SalePrice",
       y = "TotalBSF")

ggplot(data = not_missing_numerical_vars, aes(x = LivingArea, y = SalePrice)) +
  geom_point() +
  labs(title = "Scatter Plot of LivingArea and SalePrice",
       x = "LivingArea",
       y = "SalePrice")

# Dimensionality reduction
df_method1_with_dummies <- df_method1_with_dummies[, !names(df_method1_with_dummies) == "TotalRmsAbvGrd"]
df_method1_with_dummies <- df_method1_with_dummies[, !names(df_method1_with_dummies) == "X"]
df_method1_with_dummies <- df_method1_with_dummies[, !names(df_method1_with_dummies) == "X.1"]

# Regression modelling
set.seed(123)
train_index <- sample(1:nrow(df_method1_with_dummies), size = nrow(df_method1_with_dummies)*0.7)
train <- df_method1_with_dummies[train_index, ] 
test <- df_method1_with_dummies[-train_index, ]

lm_model <- lm(SalePrice ~ LotArea + LotShape + LandContour_HLS + LandContour_Low + LandContour_Lv1 +
                 Utilities_Allpub + LotConfig_CulDSac + LotConfig_FR2 + LotConfig_FR3 + LotConfig_Inside +
                 Slope_Mod + Slope_Sev + DwellClass_2fmCon + DwellClass_Duplex + DwellClass_Twnhs + DwellClass_TwnhsE +
                 OverallQuality + OverallCondition + YearBuilt + ExteriorCondition_Code + BasementCondition_Code +
                 TotalBSF + CentralAir_Y + LowQualFinSF + LivingArea + FullBath + HalfBath + BedroomAbvGr +
                 KitchenQuality_Code + KitchenAbvGr + Fireplaces + GarageType_Attchd + GarageType_Basment +
                 GarageType_BuiltIn + GarageType_CarPort + GarageType_Detchd + GarageCars + PavedDrive_Code + 
                 PoolArea + OpenPorchSF + MoSold + YrSold, data = train)
summary(lm_model)

# Creating interaction terms
train$Garage_Attchd_Cars <- train$GarageType_Attchd * train$GarageCars
train$Garage_Detchd_Cars <- train$GarageType_Detchd * train$GarageCars
train$Garage_BuiltIn_Cars <- train$GarageType_BuiltIn * train$GarageCars
train$Garage_Basment_Cars <- train$GarageType_Basment * train$GarageCars
train$OverallQuality_YearBuilt <- train$OverallQuality * train$YearBuilt
train$TotalBSF_BasementCondition <- train$TotalBSF * train$BasementCondition_Code
train$KitchenQuality_KitchenAbvGr <- train$KitchenQuality_Code * train$KitchenAbvGr
train$ExteriorCondition_YearBuilt <- train$ExteriorCondition_Code * train$YearBuilt

lm_model2 <- lm(SalePrice ~ LotArea + LotShape + LandContour_HLS + LandContour_Low + LandContour_Lv1 +
                  LotConfig_CulDSac + LotConfig_FR2 + LotConfig_FR3 + LotConfig_Inside +
                  Slope_Mod + Slope_Sev + DwellClass_2fmCon + DwellClass_Duplex + DwellClass_Twnhs + DwellClass_TwnhsE +
                  OverallQuality + OverallCondition + YearBuilt + ExteriorCondition_Code + BasementCondition_Code +
                  TotalBSF + CentralAir_Y + LowQualFinSF + LivingArea + BedroomAbvGr +
                  KitchenQuality_Code + Fireplaces + GarageType_Attchd + GarageType_Basment +
                  GarageType_BuiltIn + GarageType_CarPort + GarageType_Detchd + GarageCars + PavedDrive_Code + 
                  PoolArea + OpenPorchSF + MoSold + YrSold + Garage_Attchd_Cars + Garage_Detchd_Cars +
                  Garage_BuiltIn_Cars + Garage_Basment_Cars + OverallQuality_YearBuilt + TotalBSF_BasementCondition +
                  KitchenQuality_KitchenAbvGr + ExteriorCondition_YearBuilt, data = train)
summary(lm_model2)

lm_model3 <- lm(SalePrice ~ LotArea + LotShape + LandContour_HLS + LandContour_Low + LandContour_Lv1 +
                  LotConfig_CulDSac + LotConfig_FR2 + LotConfig_FR3 + LotConfig_Inside +
                  Slope_Mod + Slope_Sev + DwellClass_2fmCon + DwellClass_Duplex + DwellClass_Twnhs + DwellClass_TwnhsE +
                  OverallQuality + OverallCondition + YearBuilt + ExteriorCondition_Code + BasementCondition_Code +
                  TotalBSF + CentralAir_Y + LowQualFinSF + LivingArea + FullBath + HalfBath + BedroomAbvGr +
                  KitchenQuality_Code + KitchenAbvGr + Fireplaces + GarageType_Attchd + GarageType_Basment +
                  GarageType_BuiltIn + GarageType_CarPort + GarageType_Detchd + GarageCars + PavedDrive_Code + 
                  OpenPorchSF + MoSold + YrSold, data = train)
summary(lm_model3)


































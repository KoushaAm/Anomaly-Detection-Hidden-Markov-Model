## R code for Term Project GROUP 15
## Fall 2024 - CMPT 318


required_packages <- c(
  "dplyr", "ggplot2", "depmixS4", "lubridate", "data.table",
  "devtools", "factoextra", "zoo", "car", "usethis",
  "doParallel", "foreach", "tidyr"
)

install_if_missing <- function(packages) {
  installed <- rownames(installed.packages())
  for (p in packages) {
    if (!(p %in% installed)) {
      install.packages(p, dependencies = TRUE)
    }
  }
}

install_if_missing(required_packages)

# libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)
library(usethis)
library(devtools)
library(depmixS4)
library(factoextra)
library(zoo)
library(car)
library(doParallel)
library(foreach)
library(tidyr)    

file_path <- "/Users/koushaamouzesh/Desktop/Fall 2024/318/term project/group_project/TermProjectData.txt"
df <- fread(file_path, header = TRUE, sep = ",", na.strings = "NA", stringsAsFactors = FALSE)
df <- as.data.frame(df)

cat("First 10 rows of the dataframe:\n")
head(df, 10)
cat("Column Names:\n")
colnames(df)

# **************************************************************
# Combining Date and Time into DateTime and convert to POSIXct
# **************************************************************

df$DateTime <- paste(df$Date, df$Time)
df$DateTime <- as.POSIXct(df$DateTime, format = "%d/%m/%Y %H:%M:%S", tz = "UTC")

cat("DateTime conversion completed.\n")
str(df$DateTime)

# ******************************
# Extracting time window on Monday (09:00 AM to 12:00 PM)
# ******************************

# Function to extract the time window
extract_time_window <- function(dataframe) {
  df_monday_9am_to_12pm <- dataframe %>%
    filter(
      weekdays(DateTime) == "Monday" &
        hour(DateTime) >= 9 & hour(DateTime) < 12
    )
  return(df_monday_9am_to_12pm)
}

print(head(df))

# ******************************
# Converting columns to numeric
# ******************************

numeric_cols <- c(
  "Global_active_power", "Global_reactive_power", "Voltage",
  "Global_intensity", "Sub_metering_1", "Sub_metering_2",
  "Sub_metering_3"
)

df[numeric_cols] <- lapply(df[numeric_cols], function(x) as.numeric(x))

# checking conversion success
if (any(sapply(df[numeric_cols], function(x) any(is.na(x))))) {
  cat("Warning: Some numeric columns have NA values after conversion.\n")
} else {
  cat("All numeric columns converted successfully.\n")
}

# ******************************
# Handling the Missing Values
# ******************************

# checking for missing values
missing_values <- sapply(df[numeric_cols], function(x) sum(is.na(x)))
cat("Missing Values in Each Numeric Column:\n")
print(missing_values)

# NA values approximation
fill_na <- function(x) {
  # for interpolation
  x <- na.approx(x, na.rm = FALSE)
  # to handle leading NAs
  x <- na.locf(x, na.rm = FALSE)
  # to handle trailing NAs
  x <- na.locf(x, na.rm = FALSE, fromLast = TRUE)
  return(x)
}

df[numeric_cols] <- lapply(df[numeric_cols], fill_na)

missing_values_after <- sapply(df[numeric_cols], function(x) sum(is.na(x)))
cat("Missing Values in Each Numeric Column (After Interpolation):\n")
print(missing_values_after)

df_clean <- df

# ******************************
# Feature Engineering
# ******************************

df_clean$Hour <- as.integer(format(df_clean$DateTime, "%H"))
df_clean$DayOfWeek <- as.factor(weekdays(df_clean$DateTime))
df_clean$Month <- as.factor(format(df_clean$DateTime, "%m"))

df_clean <- df_clean[complete.cases(df_clean), ]

numeric_cols <- c(
  'Global_active_power', 'Global_reactive_power', 'Voltage', 'Global_intensity',
  'Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'
)

# ************************************
# Feature Scaling (Standardization)
# ************************************

df_scaled <- df_clean
df_scaled[numeric_cols] <- scale(df_scaled[numeric_cols])

# checking the scaling results making sure Mean = 0
cat("Summary of Scaled Variables:\n")
print(summary(df_scaled[numeric_cols]))

# making sure SD = 1
col_sds <- sapply(df_scaled[numeric_cols], sd, na.rm = TRUE)

# display the standard deviations
cat("Standard Deviations of All Columns:\n")
print(col_sds)

# ************************************
# Principal Component Analysis (PCA)
# ************************************

# preparing data for PCA
pca_data <- df_scaled[numeric_cols]
# performing PCA
pca_result <- prcomp(pca_data, center = FALSE, scale. = FALSE)
# summary of PCA results
cat("PCA Summary:\n")
print(summary(pca_result))

# Variance percentages of all PCs
pca_var <- pca_result$sdev^2
pca_var_perc <- pca_var / sum(pca_var) * 100

# Printing variance percentages of all PCs
cat("Variance percentages of all Principal Components:\n")
for (i in 1:length(pca_var_perc)) {
  cat(paste0("PC", i, ": ", round(pca_var_perc[i], 2), "% "))
}

fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50)) +
  labs(
    title = "Variance Percentage vs Principal Component",
    x = "Principal Components",
    y = "Percentage of Variance Explained"
  )

# adding PCA scores to the dataframe
df_scaled$PC1 <- pca_result$x[, 1]
df_scaled$PC2 <- pca_result$x[, 2]

# ************************************
# Visualizations with PCA Components
# ***********************************

# correlation Plot of Original Variables
cor_matrix <- cor(df_scaled[, numeric_cols])

# convert to data frame and add row names as a column
cor_df <- as.data.frame(cor_matrix)
cor_df$Var1 <- rownames(cor_df)

# use pivot_longer to reshape the data
melted_cor <- pivot_longer(
  cor_df,
  cols = -Var1,
  names_to = "Var2",
  values_to = "value"
)

ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Correlation"
  ) +
  theme_minimal() +
  labs(title = "Correlation Matrix of Variables", x = "", y = "") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1,
                               size = 10, hjust = 1)
  )

# Extracting the loadings (rotation matrix) from the PCA results
loadings <- pca_result$rotation

# Scaling the loadings for better visualization
scale_factor <- 5
loadings_scaled <- loadings[, 1:2] * scale_factor

# Preparing a data frame for the loadings (arrows)
arrow_data <- data.frame(
  Feature = rownames(loadings_scaled),
  PC1 = loadings_scaled[, 1],
  PC2 = loadings_scaled[, 2]
)

# PCA plot PC1 vs PC2
ggplot(df_scaled, aes(x = PC1, y = PC2, color = "green2")) +
  geom_jitter(alpha = 0.5, size = 2, width = 0.2, height = 0.2) +
  scale_color_brewer() +
  geom_segment(
    data = arrow_data, aes(x = 0, y = 0, xend = PC1, yend = PC2),
    arrow = arrow(type = "closed", length = unit(0.2, "cm")),
    color = "blue", linewidth = 1
  ) +
  geom_text(
    data = arrow_data, aes(x = PC1, y = PC2, label = Feature),
    hjust = 1.2, vjust = 1.2, size = 5, color = "black"
  ) +
  labs(
    title = "PCA Scatter Plot: Feature Contribution",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14)
  )

pca_result <- prcomp(df_scaled[numeric_cols])
loadings <- pca_result$rotation
print(loadings)

# ************************************
# Splitting train and test data
# ************************************
df_scaled$Year <- year(df_scaled$DateTime)

df_scaled <- extract_time_window(df_scaled)

train_data <- df_scaled %>% filter(Year <= 2008)
test_data <- df_scaled %>% filter(Year == 2009)

train_features <- train_data[, c("Global_intensity", "Voltage")]
test_features <- test_data[, c("Global_intensity", "Voltage")]

# saving test_data for injecting anomalies
test_data_injected_anomalies <- test_features

# ************************************
# Model Training Optimizations
# ************************************

# reducing the number of states to try
states_list <- c(4, 6, 7, 8, 10, 12, 13)

# adjusting EM algorithm control parameters
em_ctrl <- em.control(maxit = 1000, tol = 1e-5)

# initializing lists to store results
log_likelihoods <- list()
bics <- list()
models <- list()

# parallelized model training (requires doParallel and foreach packages)

# setting up parallel backend to use multiple processors
num_cores <- detectCores()
cl <- makeCluster(num_cores)
registerDoParallel(cl)

results <- foreach(num_states = states_list, .packages = 'depmixS4') %dopar% {
  suppressMessages({
    hmm_model <- depmix(
      response = list(Global_intensity ~ 1, Voltage ~ 1),
      data = train_features,
      nstates = num_states,
      family = list(gaussian(), gaussian())
    )
    
    set.seed(42)
    print(paste0("Train model state = ", num_states))
    fitted_model <- fit(hmm_model, ntimes = 10, verbose = FALSE, emcontrol = em_ctrl)
    
    log_likelihood <- logLik(fitted_model)
    bic_value <- BIC(fitted_model)
    
    list(
      num_states = num_states,
      log_likelihood = log_likelihood,
      bic_value = bic_value,
      model = fitted_model
    )
  })
}

stopCluster(cl)

# collecting results from the parallel computations
for (res in results) {
  num_states <- res$num_states
  log_likelihoods[[as.character(num_states)]] <- res$log_likelihood
  bics[[as.character(num_states)]] <- res$bic_value
  models[[as.character(num_states)]] <- res$model
  cat("Log-Likelihood for", num_states, "states:", res$log_likelihood, "\n")
  cat("BIC for", num_states, "states:", res$bic_value, "\n")
}

# selecting the best model based on the lowest BIC
best_num_states <- 7  # based on experiments
best_model <- models[[as.character(best_num_states)]]

# saving the best model
saveRDS(best_model, file = "training_model.rds")

cat("\nTraining Results Summary:\n")
result_df <- data.frame(
  States = states_list,
  LogLikelihood = unlist(log_likelihoods),
  BIC = unlist(bics)
)
print(result_df)

# Plotting BIC and log-likelihood
ggplot(result_df, aes(x = States)) +
  geom_line(aes(y = BIC, color = "BIC"), linewidth = 1) +
  geom_point(aes(y = BIC, color = "BIC"), size = 3) +
  geom_line(aes(y = LogLikelihood, color = "Log-Likelihood"), linewidth = 1) +
  geom_point(aes(y = LogLikelihood, color = "Log-Likelihood"), size = 3) +
  labs(
    title = "BIC and Log Likelihood for Different Number of States",
    x = "Number of States",
    y = "Value",
    color = "Metric"
  ) +
  scale_color_manual(values = c("BIC" = "blue", "Log-Likelihood" = "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top",
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  )

# Plot of BIC vs number of states
ggplot(result_df, aes(x = States, y = BIC)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 3) +
  labs(
    title = "BIC vs. Number of States",
    x = "Number of States",
    y = "BIC Value"
  ) +
  theme_minimal()

# Plot of log-likelihood vs number of states
ggplot(result_df, aes(x = States, y = LogLikelihood)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 3) +
  labs(
    title = "Log-Likelihood vs. Number of States",
    x = "Number of States",
    y = "Log-Likelihood"
  ) +
  theme_minimal()

# ************************************
# Performance on test data
# ************************************

# Using the best model parameters to predict on test data
test_model <- depmix(
  response = list(Global_intensity ~ 1, Voltage ~ 1),
  data = test_features,
  nstates = best_num_states,
  family = list(gaussian(), gaussian())
)

# Setting parameters from the best model
test_fitted <- setpars(test_model, getpars(best_model))

# Computing the log-likelihood without re-fitting
fb_test <- forwardbackward(test_fitted)
test_log_likelihood <- fb_test$logLike
cat("Log-Likelihood on Test Data:", test_log_likelihood, "\n")

# ******************************
# Anomaly detection
# ******************************

# Partitioning into 10 roughly equal-sized subsets
test_data_partition <- test_data %>%
  mutate(week_group = ntile(row_number(), 10))

weekly_subsets <- test_data_partition %>%
  group_split(week_group)

# Data frame to store results
subset_data_frame <- data.frame(
  week_group = 1:10,
  LogLikelihood = numeric(10),
  avg_loglikelihood = numeric(10)
)

# Anomaly detection loop without using setdata
for (i in 1:10) {
  subset_data <- weekly_subsets[[i]]
  subset_features <- subset_data[, c("Global_intensity", "Voltage")]
  
  # Creating a new model with the subset data
  hmm_model_subset <- depmix(
    response = list(Global_intensity ~ 1, Voltage ~ 1),
    data = subset_features,
    nstates = best_num_states,  # Stick to the best model params
    family = list(gaussian(), gaussian())
  )
  
  # Choosing the parameters from the best model
  hmm_model_subset <- setpars(hmm_model_subset, getpars(best_model))
  
  # Computing the log-likelihood without re-fitting
  fb <- forwardbackward(hmm_model_subset)
  loglikelihood_subset <- fb$logLike
  normalize_loglikelihood_subset <- loglikelihood_subset / nrow(subset_features)
  
  subset_data_frame$LogLikelihood[i] <- loglikelihood_subset
  subset_data_frame$avg_loglikelihood[i] <- normalize_loglikelihood_subset
}

# Calculating deviations and threshold
train_log_likelihood <- forwardbackward(best_model)$logLike / nrow(train_features)
subset_data_frame$Deviation <- subset_data_frame$avg_loglikelihood - train_log_likelihood
threshold <- max(abs(subset_data_frame$Deviation))
cat("Threshold for the acceptable deviation of any unseen observations:", threshold, "\n")
print(subset_data_frame)

# ************************************
# Log-Likelihood for training data
# ************************************

# The best model fitted on the training dataset
train_fitted <- setpars(models[[as.character(best_num_states)]], getpars(models[[as.character(best_num_states)]]))

# Log-likelihood for training data using the forward-backward algorithm
fb_train <- forwardbackward(train_fitted)
train_log_likelihood <- fb_train$logLik

cat("Log-Likelihood for Training Data: ", train_log_likelihood, "\n")

# ************************************
# Log-Likelihood for Test Data
# ************************************

# the best model fitted on the test dataset
test_fitted <- setpars(test_model, getpars(models[[as.character(best_num_states)]]))

# log-likelihood for test data using the forward-backward algorithm
fb_test <- forwardbackward(test_fitted)
test_log_likelihood <- fb_test$logLik

cat("Log-Likelihood for Test Data: ", test_log_likelihood, "\n")

# ************************************
# Normalized Log-Likelihood
# ************************************

# normalizing the log-likelihood by dividing by the number of observations
train_log_likelihood_normalized <- train_log_likelihood / nrow(train_data)
test_log_likelihood_normalized <- test_log_likelihood / nrow(test_data)

cat("Normalized Log-Likelihood for Training Data: ", train_log_likelihood_normalized, "\n")
cat("Normalized Log-Likelihood for Test Data: ", test_log_likelihood_normalized, "\n")

# ************************************
# Comparison plot
# ************************************

comparison_df <- data.frame(
  Data = c("Training", "Test"),
  LogLikelihood = c(train_log_likelihood_normalized, test_log_likelihood_normalized)
)

ggplot(comparison_df, aes(x = Data, y = LogLikelihood, fill = Data)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Normalized Log-Likelihood Comparison: Training vs Test",
    x = "Dataset", y = "Normalized Log-Likelihood"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# ************************************
# Anomalies injection
# ************************************

subset_size <- nrow(test_data_injected_anomalies) / 3

# splitting the dataset into 3 subsets for testing
subsets <- split(test_data_injected_anomalies, ceiling(seq_along(1:nrow(test_data_injected_anomalies)) / subset_size))

# function to inject anomalies
inject_anomalies <- function(df, anomalies_per_subset = 100) {
  set.seed(42)
  anomaly_indices <- sample(1:nrow(df), anomalies_per_subset, replace = FALSE)
  
  df[anomaly_indices, "Global_intensity"] <- runif(anomalies_per_subset, -10, 10)
  df[anomaly_indices, "Voltage"] <- runif(anomalies_per_subset, -5, 5)
  
  return(df)
}

# injecting anomalies into the subsets
anomalous_subsets <- lapply(subsets, inject_anomalies)

# function that flags anomalies in subsets
flag_anomalous_subsets <- function(subset, threshold) {
  # creating a new model with the subset data
  hmm_model_subset <- depmix(
    response = list(Global_intensity ~ 1, Voltage ~ 1),
    data = subset,
    nstates = best_num_states,  # Stick to the best model params
    family = list(gaussian(), gaussian())
  )
  
  # choosing the parameters from the best model
  hmm_model_subset <- setpars(hmm_model_subset, getpars(best_model))
  
  # computing the log-likelihood without re-fitting
  fb <- forwardbackward(hmm_model_subset)
  normalize_loglikelihood_subset <- fb$logLike / nrow(subset_features)
  # cat("normalized anamoly for subset: ", normalize_loglikelihood_subset)
  
  # calculate the deviation from the training model
  deviation <- normalize_loglikelihood_subset - train_log_likelihood_normalized
  
  if (abs(deviation) > threshold) {
    cat("Anomaly detected, the data deviates from the threshold by:", abs(deviation), "\n")
  } else {
    cat("Normal, no anomalies detected.\n")
  }
}


# ************************************
# Testing the model's ability to detect anomalies
# ************************************
cat("Testing Anomalous Subsets:\n")
for (i in 1:3) {
  cat("Subset", i, ": ")
  flag_anomalous_subsets(anomalous_subsets[[i]], threshold)
}

cat("\nTesting Original Subsets:\n")
for (i in 1:3) {
  cat("Subset", i, ": ")
  flag_anomalous_subsets(subsets[[i]], threshold)
}

# boxplot comparison for each subset before and after anomalies
plot_subset_comparison <- function(subsets, anomalous_subsets, variable, subset_number) {
  plot_list <- list()
  
  for (i in 1:length(subsets)) {
    original <- subsets[[i]]
    anomalous <- anomalous_subsets[[i]]
    
    original$Type <- "Original"
    anomalous$Type <- "Anomalous"
    
    combined <- rbind(
      data.frame(original[, c("Type", variable)]),
      data.frame(anomalous[, c("Type", variable)])
    )
    
    # uses all_of(variable) to avoid the warning
    melted <- pivot_longer(
      combined,
      cols = all_of(variable),
      names_to = "Variable",
      values_to = "Value"
    )
    
    p <- ggplot(melted, aes(x = Type, y = Value, color = Type, fill = Type)) +
      geom_boxplot(alpha = 0.6) +
      facet_wrap(~Variable, scales = "free") +
      labs(
        title = paste("Subset", subset_number, ": Original vs Anomalous Data"),
        x = "Type of Data", y = "Value"
      ) +
      theme_minimal()+
      theme(
        plot.title = element_text(size = 16, face = "bold"), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 12),    
        strip.text = element_text(size = 14)    
      )
    
    plot_list[[i]] <- p
  }
  
  return(plot_list)
}
plot_subset_comparison(subsets[1], anomalous_subsets[1], c("Voltage", "Global_intensity"), 1)
plot_subset_comparison(subsets[2], anomalous_subsets[2], c("Voltage", "Global_intensity"), 2)
plot_subset_comparison(subsets[3], anomalous_subsets[3], c("Voltage", "Global_intensity"), 3)

# THE END

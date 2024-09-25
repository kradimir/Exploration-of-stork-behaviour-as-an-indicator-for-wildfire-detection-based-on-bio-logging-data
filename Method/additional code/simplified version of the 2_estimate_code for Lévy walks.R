#simplified version of the 2_estimate_code for Lévy walks (post line 218)
# Load required libraries
library(VGAM)
library(dplyr)


# Function to remove outliers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs = c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  return(y)
}

# Filter data based on speed and distance
Movements_all <- Movements_all[Movements_all$Speed >= 0 & Movements_all$Speed < 70, ]
Movements_all <- Movements_all[Movements_all$Distance >= 0, ]
Movements_all <- Movements_all[Movements_all$Distance >= 0.03 & Movements_all$Distance < 325, ]

# Compute day numbers for each individual
Inds <- unique(Movements_all$Individual)
Movements_all$day_cur <- NA
for (i in seq_along(Inds)) {
  Movements_all$day_cur[Movements_all$Individual == Inds[i]] <- 1 + as.numeric(Movements_all$Start.jdate[Movements_all$Individual == Inds[i]] - min(Movements_all$Start.jdate[Movements_all$Individual == Inds[i]], na.rm = TRUE))
}

# Initialize an empty data frame for storing results
Out <- data.frame()

# Loop over each individual and each day to compute alpha values
for (i in seq_along(Inds)) {
  gc()
  cat('individual', i, '\n')
  Movements_cur <- Movements_all[Movements_all$Individual == Inds[i], ]
  Days_after <- 6:(max(Movements_cur$day_cur, na.rm = TRUE) - 5)
  
  cur_alphas <- sapply(Days_after, function(Day) {
    input_data_cur <- na.omit(Movements_cur$Distance[(Movements_cur$day_cur) >= (Day - 5) & (Movements_cur$day_cur) <= (Day + 5)] * 1000)
    
    # Remove outliers
    input_data_cur <- remove_outliers(input_data_cur)
    input_data_cur <- na.omit(input_data_cur)
    
    if (length(input_data_cur) > 50) {
      cat("Day:", Day, "Number of data points:", length(input_data_cur), "\n")
      print(summary(input_data_cur))
      
      fit_result <- tryCatch({
        fit <- vglm(input_data_cur ~ 1, truncpareto(xmin - (1e-10), xmax + (1e-10), lshape = 'loglink'), trace = TRUE)
        1 + exp(fit@coefficients)
      }, error = function(e) {
        cat("Error fitting model on Day:", Day, "\n")
        print(e)
        NA
      })
      return(fit_result)
    } else {
      return(NA)
    }
  })
  
  Out <- rbind(Out, data.frame(Individual = Inds[i], day_cur = Days_after, cur_alphas = cur_alphas))
}

# Save results to a file
all_alphas <- Out
save(all_alphas, file = 'all_alphas_07_08_2024.RData')

# Remove existing Alpha column if it exists
if ("Alpha" %in% names(Movements_all)) {
  Movements_all <- Movements_all %>% select(-Alpha)
}

# Join the new alpha values with the original data
Movements_all <- Movements_all %>% left_join(all_alphas, by = c("Individual", "day_cur"))

# Rename the alpha column
names(Movements_all)[names(Movements_all) == "cur_alphas"] <- "Alpha"

# Save the final data frame with the alpha values
save(Movements_all, file = 'Movements_all_with_Alpha_27_05_2024.RData')
















#version we necessary added script from Eldar which I don't have at the moment
# Initial data summaries and visualizations
summary(Movements_all$Speed)
hist(log(Movements_all$Speed), breaks=500)
hist(Movements_all$Distance / as.numeric(Movements_all$Duration) * 60, breaks=500)
quantile(Movements_all$Speed, c(0.975, 0.99, 0.995, 0.999, 0.9995, 0.9996, 0.9997, 0.9998, 0.9999))

# Filter data based on speed
Movements_all <- Movements_all[Movements_all$Speed >= 0 & Movements_all$Speed < 70, ]

# Remove rows with negative distance values
Movements_all <- Movements_all[Movements_all$Distance >= 0, ]

# Further filter data based on distance
summary(Movements_all$Distance)
xmin <- 30
xmax <- 325000
Movements_all <- Movements_all[Movements_all$Distance >= 0.03 & Movements_all$Distance < 325, ]

# If additional processing steps are needed
if (FALSE) {
  # Source external scripts and load additional libraries
  #source('d:\\Powerlaw\\Power_law_ER.R')
  library(Rcpp)
  #sourceCpp("d:\\Powerlaw\\KolmogorovSmirnovDist.cpp")
  library(prospectr)
  
  # Estimate upper boundary using power-law fitting
  input_data_cur <- na.omit(Movements_all$Distance * 1000)
  res_cur <- plfit_upper_trunc(input_data_cur, fit.vglm = TRUE, print = FALSE)
  
  # Filter input data and fit truncated Pareto distribution
  input_data_cur_trunc <- input_data_cur[input_data_cur > xmin & input_data_cur < xmax]
  cur.fit <- vglm(input_data_cur_trunc ~ 1, truncpareto(xmin - (1e-10), xmax + (1e-10), lshape = 'loge'))
  Alpha <- 1 + exp(cur.fit@coefficients)
}

# Compute day numbers for each individual
Inds <- unique(Movements_all$Individual)
Movements_all$day_cur <- NA
for (i in seq_along(Inds)) {
  Movements_all$day_cur[Movements_all$Individual == Inds[i]] <- 1 + as.numeric(Movements_all$Start.jdate[Movements_all$Individual == Inds[i]] - min(Movements_all$Start.jdate[Movements_all$Individual == Inds[i]], na.rm = TRUE))
}

# Initialize an empty data frame for storing results
Out <- data.frame()

# Loop over each individual and each day to compute alpha values
for (i in seq_along(Inds)) {
  gc()
  cat('individual', i, '\n')
  Movements_cur <- Movements_all[Movements_all$Individual == Inds[i], ]
  Days_after <- 6:(max(Movements_cur$day_cur, na.rm = TRUE) - 5)
  
  cur_alphas <- sapply(Days_after, function(Day) {
    input_data_cur <- na.omit(Movements_cur$Distance[(Movements_cur$day_cur) >= (Day - 5) & (Movements_cur$day_cur) <= (Day + 5)] * 1000)
    if (length(input_data_cur) > 50) {
      fit_result <- tryCatch({
        fit <- vglm(input_data_cur ~ 1, truncpareto(xmin - (1e-10), xmax + (1e-10), lshape = 'loglink'))
        1 + exp(fit@coefficients)
      }, error = function(e) {
        NA
      })
      return(fit_result)
    } else {
      return(NA)
    }
  })
  
  Out <- rbind(Out, data.frame(Individual = Inds[i], day_cur = Days_after, cur_alphas = cur_alphas))
}

# Save results to a file
all_alphas <- Out
save(all_alphas, file = 'all_alphas_07_08_2024.RData')

# Remove existing Alpha column if it exists
if ("Alpha" %in% names(Movements_all)) {
  Movements_all <- Movements_all %>% select(-Alpha)
}

# Join the new alpha values with the original data
Movements_all <- Movements_all %>% left_join(all_alphas, by = c("Individual", "day_cur"))

# Rename the alpha column
names(Movements_all)[names(Movements_all) == "cur_alphas"] <- "Alpha"

# Save the final data frame with the alpha values
save(Movements_all, file = 'Movements_all_with_Alpha_27_05_2024.RData')

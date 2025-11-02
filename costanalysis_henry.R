library(gert)

library(credentials)

credentials::set_github_pat()

# this will prompt a popup that asks you to enter your GitHub Personal Access Token.

gert::git_pull() # pull most recent changes from GitHub

gert::git_add(dir(all.files = TRUE)) # select any and all new files created or edited to be 'staged'

# 'staged' files are to be saved anew on GitHub 

### Get Functions and Packages

# Get process control functions

#install.packages("dplyr")
#install.packages("readr")
#install.packages("ggplot2")
#install.packages("ggpubr")
#install.packages("moments")

library(dplyr)
library(readr)
library(ggplot2)
library(ggpubr)
library(moments)
### Get Functions and Packages

# Get process control functions
source("functions_process_control.R")


### Known Data

# Approximate Fatigue Curve
# EVA @ 0.23 g/cm^3
# 0.304 (1) -> 0.270 (200000)
# slope = -0.00000017
# E = (-0.8*log(N)+9)/1.6*rho
# EVA @ 0.17 g/cm^3
# 0.304 -> 0.255
# slope = -0.000000245
# E = (-0.54*log(N)+7.1)/1.6*rho
# PEBA @ 0.09 g/cm^3
#
# E = (-0.5*log(N)+6.2)/1.6*rho

get_cycles = function(rho,R_E){
  slope = 0.000000075*(rho-0.17)/0.06-0.000000245
  y_int = 0.304
  log_N = y_int*(R_E-1)/slope
  cycles = log_N
  return(cycles)
}

# Density
E_eva = 18 # MPa
E_peba = 20 # MPa

# Stride Length
stride = 1.25 # m


### Import Data

# Get Experimental Data
df_shoes = read.csv("Shoe_Data - Experiment.csv")

# Get Source Data
df_ref = read.csv("Shoe_Data - RunRepeat (2).csv")


### Make Data Workable

# Keep only shoes with measured data
df_shoes = df_shoes[complete.cases(df_shoes[, c("Avg_Retired_SA")]), ]
df_ref = df_ref[complete.cases(df_ref[, c("New_SA")]), ]

# Restructure data frame into x an y vectors
cols <- c("Retired_SA_1", "Retired_SA_2", "Retired_SA_3", "Retired_SA_4", "Retired_SA_5")
df <- df_shoes[, cols, drop = FALSE]
vec = as.vector(t(as.matrix(df))) 
vec_names <- rep(rownames(df), each = ncol(df)) %>% as.numeric()
df_SA <- data.frame(specimen = vec_names,shore_hardness = vec)

### Analyze

# Percent Change of SA
diff_SA = (df_shoes$Avg_Retired_SA-df_ref$New_SA)/df_ref$New_SA*100
R_SA = df_shoes$Avg_Retired_SA/df_ref$New_SA

### Calculate

# Approximate Midsole Elastic Modulus
Eold = 10^(0.0235*df_shoes$Avg_Retired_SA-0.6403)
Enew = 10^(0.0235*df_ref$New_SA-0.6403)

# Percent Change of E
diff_E = (Eold-Enew)/Enew*100
R_E = Eold/Enew

# Approximate Density
df_shoes = df_shoes %>% mutate(
  rho_shoes = sqrt(Enew/E_eva))

df_shoes = df_shoes %>% mutate(
  miles = get_cycles(df_shoes$rho_shoes,R_E)*2*stride/1609
)

### Fit mileage data to gamma distribution
#install.packages("fitdistrplus")
library(fitdistrplus)

fit_gamma <- fitdist(abs(df_shoes$miles), distr = 'gamma', method = 'mle')

plot(fit_gamma)

### shape and rate factors of gamma distribution
k = unname(fit_gamma$estimate[1])
lambda = unname(fit_gamma$estimate[2])

### Make annual cost curve
annualcost = 354.1

### Define failure function for gamma distribution
F = function(t, k, lambda){
  n = seq(from = 0, to = k - 1)
  1 - sum( (lambda*t)^n / factorial(n)  * exp(-lambda*t) )
}

### calculate mean time (mileage) to fail
mttf = k / lambda 

### Probability of getting extra mileage
P_extra = 1 - F(mttf, k, lambda)

### Convert mileage to time

gert::git_commit_all("my first commit") # save your record of file edits - called a commit

gert::git_push() # push your commit to GitHub


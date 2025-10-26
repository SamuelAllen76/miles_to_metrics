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
df_ref = read.csv("Shoe_Data - RunRepeat (1).csv")


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

# Percent Change of E
diff_E = (Eold-Enew)/Enew*100
R_E = Eold/Enew


### Calculate

# Approximate Midsole Elastic Modulus
Eold = 10^(0.0235*df_shoes$Avg_Retired_SA-0.6403)
Enew = 10^(0.0235*df_ref$New_SA-0.6403)

# Approximate Density
df_shoes = df_shoes %>% mutate(
  rho_shoes = sqrt(Enew/E_eva))

df_shoes = df_shoes %>% mutate(
  miles = get_cycles(df_shoes$rho_shoes,R_E)*2*stride/1609
)


### Visualize

# Create standard deviation chart
ggs(df_SA$specimen,df_SA$shore_hardness,xlab="Specimen")

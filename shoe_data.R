# Get Experimental Data
df_shoes = read.csv("Shoe_Data - Experimental.csv")

# Keep only shoes with measured data
df_shoes = df_shoes[complete.cases(df_shoes[, c("Avg_Retired_SA")]), ]

# Get process control functions
source("functions_process_control.R")

# Restructure data frame into x an y vectors
cols <- c("Retired_SA_1", "Retired_SA_2", "Retired_SA_3", "Retired_SA_4", "Retired_SA_5")
df <- df_shoes[, cols, drop = FALSE]
vec = as.vector(t(as.matrix(df))) 
vec_names <- rep(rownames(df), each = ncol(df)) %>% as.numeric()
df_SA <- data.frame(specimen = vec_names,shore_hardness = vec)

# Create standard deviation chart
ggs(df_SA$specimen,df_SA$shore_hardness,xlab="Specimen")

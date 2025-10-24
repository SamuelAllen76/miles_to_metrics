# Get Experimental Data
df_shoes = read.csv("Shoe_Data - Experiment.csv")

# Get Source Data
df_ref = read.csv("Shoe_Data - RunRepeat (1).csv")

# Keep only shoes with measured data
df_shoes = df_shoes[complete.cases(df_shoes[, c("Avg_Retired_SA")]), ]
df_ref = df_ref[complete.cases(df_ref[, c("New_SA")]), ]

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

# Percent Change of SA
RSA = (df_ref$New_SA-df_shoes$Avg_Retired_SA)/df_ref$New_SA*100

# Save to Github
credentials::set_github_pat()

gert::git_add(dir(all.files = TRUE)) # select any and all new files created or edited to be 'staged'

# 'staged' files are to be saved anew on GitHub 

gert::git_commit_all("my first commit") # save your record of file edits - called a commit

gert::git_push() # push your commit to GitHub

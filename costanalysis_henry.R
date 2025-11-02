library(gert)

library(credentials)

credentials::set_github_pat()

# this will prompt a popup that asks you to enter your GitHub Personal Access Token.

gert::git_pull() # pull most recent changes from GitHub

gert::git_add(dir(all.files = TRUE)) # select any and all new files created or edited to be 'staged'

# 'staged' files are to be saved anew on GitHub 

gert::git_commit_all("my first commit") # save your record of file edits - called a commit

gert::git_push() # push your commit to GitHub

### Get Functions and Packages

# Get process control functions

install.packages("dplyr")
install.packages("readr")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("moments")

library(dplyr)
library(readr)
source("functions_process_control.R")

# Get Experimental Data
df_shoes = read.csv("Shoe_Data - Experiment.csv")

# Get Source Data
df_ref = read.csv("Shoe_Data - RunRepeat (1).csv")


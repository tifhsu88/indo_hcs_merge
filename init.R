#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Tiffany Hsu, Robert Heilmayr
# Project: HCS georeferencing/digitization integration
# Date: 9/18/22
# Purpose: Creates a symlink from code directory to a directory storing project data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(R.utils)

# Define the path to your local code directory
code_dir <- "D:/dev/Indonesia/indo_hcs_merge/"

# Define the path to your local google drive Treeconomics\\Data directory 
data_dir <- "D:/cloud/Dropbox/collaborations/SNAPP_HCS/HCSproject/data/"

createLink(paste0(code_dir, 'remote'), data_dir, overwrite = FALSE)


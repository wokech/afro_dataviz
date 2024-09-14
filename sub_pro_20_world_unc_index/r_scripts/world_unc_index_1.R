# The World Uncertainty Index

# The World Uncertainty Index is a measure that tracks uncertainty across the 
# globe by text mining the country reports of the Economist Intelligence Unit. 
# The index is available for 143 countries.

# The dataset is updated as follows: the data for the first quarter in 
# late April, second quarter in late July, third quarter in late October, 
# and fourth quarter in late January.

# Note: The WUI is computed by counting the percent of word “uncertain” 
# (or its variant) in the Economist Intelligence Unit country reports. The WUI 
# is then rescaled by multiplying by 1,000,000. A higher number means higher 
# uncertainty and vice versa. For example, an index of 200 corresponds to the 
# word uncertainty accounting for 0.02 percent of all words, which—given the EIU 
# reports are on average about 10,000 words long—means about 2 words per report.

# A) Load the required libraries

library(tidyverse)
library(readxl)

# B) Load the datasets

# Overall Uncertainty

# List available sheets in the Excel file

excel_sheets("sub_pro_20_world_unc_index/datasets/WUI_Data.xlsx")

# Explain each sheet name

# T0 - 
# F1 - 
# F2 - 
# T1 - 
# T2 - 
# T3 - 
# T4 - 
# T5 - 
# T6 - 
# T7 - 
# T8 - 
# T9 - 

# Pandemic Uncertainty

# List available sheets in the Excel file

excel_sheets("sub_pro_20_world_unc_index/datasets/WPUI_Data.xlsx")

# Explain each sheet name

# T0 - 
# F1 - 
# F2 - 
# T1 - 
# T2 - 
# T3 - 

# Read data from specific sheets by name

# sheet1_data <- read_excel("example.xlsx", sheet = "Sheet1")
# sheet2_data <- read_excel("example.xlsx", sheet = "Sheet2")

# C) EDA / Plot Generation


# DBnomics R client

# https://cran.r-project.org/web/packages/rdbnomics/vignettes/rdbnomics.html

# 1) Fetch time series by ids

# A series identifier (ids) is defined by three values, formatted like 
# this: provider_code/dataset_code/series_code

# Fetch one series from dataset ‘Unemployment rate’ (ZUTN) of AMECO provider

library(data.table)
library(rdbnomics)

# Euro Zone Data
df_1 <- rdb(ids = "AMECO/ZUTN/EA19.1.0.0.0.ZUTN")
df_1 <- df_1[!is.na(value)]

# Fetch two series from dataset ‘Unemployment rate’ (ZUTN) of AMECO provider

# Euro Zone Data combined with Denmark

df_2 <- rdb(ids = c("AMECO/ZUTN/EA19.1.0.0.0.ZUTN", "AMECO/ZUTN/DNK.1.0.0.0.ZUTN"))
df_2 <- df_2[!is.na(value)]

# Fetch two series from different datasets of different providers

# Two different providers

df_3 <- rdb(ids = c("AMECO/ZUTN/EA19.1.0.0.0.ZUTN", "Eurostat/une_rt_q/Q.SA.Y15-24.PC_ACT.T.EA19"))
df_3 <- df_3[!is.na(value)]

# Fetch one series from dataset ‘Balance of Payments’ (BOP) of IMF

df_4 <- rdb("IMF", "BOP", mask = "A.FR.BCA_BP6_EUR")
df_4 <- df_4[!is.na(value)]

# Fetch two series from dataset ‘Balance of Payments’ (BOP) of IMF

df_5 <- rdb("IMF", "BOP", mask = "A.FR+ES.BCA_BP6_EUR")
df_5 <- df_5[!is.na(value)]

# Fetch all series along one dimension from dataset ‘Balance of Payments’ (BOP) of IMF

df_6 <- rdb("IMF", "BOP", mask = "A..BCA_BP6_EUR")
df_6 <- df_6[!is.na(value)]
df_6 <- df_6[order(-period, REF_AREA)]
df_6 <- head(df_6, 100)

# Fetch series along multiple dimensions from dataset ‘Balance of Payments’ (BOP) of IMF

df_7 <- rdb("IMF", "BOP", mask = "A.FR.BCA_BP6_EUR+IA_BP6_EUR")
df_7 <- df_7[!is.na(value)]
df_7 <- df_7[order(period), head(.SD, 50), by = INDICATOR]

# Fetch time series by dimensions

# Fetch one value of one dimension from dataset ‘Unemployment rate’ (ZUTN) of AMECO provider

df_8 <- rdb("AMECO", "ZUTN", dimensions = list(geo = "ea19"))
df_8 <- df_8[!is.na(value)]

# Fetch two values of one dimension from dataset ‘Unemployment rate’ (ZUTN) of AMECO provider

df_9 <- rdb("AMECO", "ZUTN", dimensions = list(geo = c("ea19", "dnk")))
df_9 <- df_9[!is.na(value)]

# Fetch several values of several dimensions from dataset ‘Doing business’ (DB) of World Bank

df_10 <- rdb("WB", "DB", dimensions = list(country = c("DZ", "PE"), indicator = c("ENF.CONT.COEN.COST.ZS", "IC.REG.COST.PC.FE.ZS")))
df_10 <- df_10[!is.na(value)]

# Fetch time series with a query

# Fetch one series from dataset ‘WEO by countries (2019-10 release)’ (WEO:2019-10) of IMF

df_11 <- rdb("IMF", "WEO:2019-10", query = "France current account balance percent")
df_11 <- df_11[!is.na(value)]

# Fetch series from dataset ‘WEO by countries (2019-10 release)’ (WEO:2019-10) of IMF

df_12 <- rdb("IMF", "WEO:2019-10", query = "current account balance percent")
df_12 <- df_12[!is.na(value)]

# Fetch time series found on the web site

df_13 <- rdb(api_link = "https://api.db.nomics.world/v22/series/WB/DB?dimensions=%7B%22country%22%3A%5B%22FR%22%2C%22IT%22%2C%22ES%22%5D%7D&q=IC.REG.PROC.FE.NO&observations=1&format=json&align_periods=1&offset=0&facets=0")
df_13 <- df_13[!is.na(value)]

# Fetch time series from the cart

df_14 <- rdb(api_link = "https://api.db.nomics.world/v22/series?observations=1&series_ids=BOE/6008/RPMTDDC,BOE/6231/RPMTBVE")
df_14 <- df_14[!is.na(value)]

# Fetch the available datasets of a provider

df_imf <- data.frame(rdb_datasets(provider_code = "IMF"))

rdb_datasets(provider_code = c("IMF", "BDF"))

rdb_datasets(provider_code = "IMF", simplify = TRUE)


options(rdbnomics.progress_bar_datasets = TRUE)
options(rdbnomics.progress_bar_datasets = FALSE)

datasets_all <- rdb_datasets()
datasets_all <- data.frame(datasets_all)

# Fetch the possible dimensions of available datasets of a provider

rdb_dimensions(provider_code = "IMF", dataset_code = "WEO:2019-10")

rdb_dimensions(provider_code = "IMF", dataset_code = "WEO:2019-10", simplify = TRUE)

# Fetch the series codes and names of available datasets of a provider

rdb_series(provider_code = "IMF", dataset_code = "WEO:2019-10", simplify = TRUE)

rdb_series(provider_code = "IMF", dataset_code = "WEO:2019-10", dimensions = list(`weo-subject` = "NGDP_RPCH"), simplify = TRUE)

rdb_series(provider_code = "IMF", dataset_code = c("WEO:2019-10", "WEOAGG:2019-10"), query = "NGDP_RPCH")




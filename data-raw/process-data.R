# Project:   mihd.results
# Objective: Prepare and deploy results for app
# Author:    Edoardo Costantini
# Created:   2023-05-02
# Modified:  2023-05-16
# Notes: 

library(stringr)

# Load data --------------------------------------------------------------------

# Load experiment 1 results
res_exp_1 <- readRDS("./data-raw/exp1_simOut_20230403_1631_res.rds")

# Load simulation study 1 time
res_exp_1_time <- readRDS("./data-raw/exp1_simOut_time.rds")

# Load slim mids objects
res_exp_1_mids <- readRDS("./data-raw/exp1_conv_20200731_1652_shiny.rds")

# Load results for simulation study 1.2
res_exp_1_2 <- readRDS("./data-raw/exp1_2_simOut_main_results_res.rds")

# Load simulation study 1.2 time
res_exp_1_2_time <- readRDS("./data-raw/exp1_2_simOut_time.rds")

# Load resampling study results
res_exp_4 <- readRDS("./data-raw/exp4_simOut_20230323_1551_res.rds")

# Simulation study: results ----------------------------------------------------

# Change names of methods
levels(res_exp_1$methods) <- str_replace(levels(res_exp_1$methods), "-la", "")
levels(res_exp_1$methods) <- str_replace(levels(res_exp_1$methods), "blasso", "BLasso")
levels(res_exp_1$methods) <- str_replace(levels(res_exp_1$methods), "bridge", "BRidge")
levels(res_exp_1$methods) <- str_replace(levels(res_exp_1$methods), "MI-qp", "MI-QP")
levels(res_exp_1$methods) <- str_replace(levels(res_exp_1$methods), "MI-am", "MI-AM")
levels(res_exp_1$methods) <- str_replace(levels(res_exp_1$methods), "MI-OP", "MI-OR")
levels(res_exp_1$methods) <- str_replace(levels(res_exp_1$methods), "stepFor", "MI-SF")

# Fix methods order
res_exp_1$methods <- factor(res_exp_1$methods,
    levels = levels(res_exp_1$methods)[c(1:7, 13, 11:12, 8, 9, 10)]
)

# Make analysis a factor
res_exp_1$analysis <- factor(
    x = res_exp_1$analysis,
    levels = unique(res_exp_1$analysis),
    labels = c("PRB", "CIC", "CIW")
)

# Use the data
usethis::use_data(res_exp_1, overwrite = TRUE)

# Simulation study: time -------------------------------------------------------

# Change names of methods
levels(res_exp_1_time$variable) <- str_replace(levels(res_exp_1_time$variable), "_la", "")
levels(res_exp_1_time$variable) <- str_replace(levels(res_exp_1_time$variable), "blasso", "BLasso")
levels(res_exp_1_time$variable) <- str_replace(levels(res_exp_1_time$variable), "bridge", "BRidge")
levels(res_exp_1_time$variable) <- str_replace(levels(res_exp_1_time$variable), "_", "-")
levels(res_exp_1_time$variable) <- str_replace(levels(res_exp_1_time$variable), "MI-qp", "MI-QP")
levels(res_exp_1_time$variable) <- str_replace(levels(res_exp_1_time$variable), "MI-am", "MI-AM")
levels(res_exp_1_time$variable) <- str_replace(levels(res_exp_1_time$variable), "MI-OP", "MI-OR")
levels(res_exp_1_time$variable) <- str_replace(levels(res_exp_1_time$variable), "stepFor", "MI-SF")

# Fix methods order
res_exp_1_time$variable <- factor(res_exp_1_time$variable,
    levels = levels(res_exp_1_time$variable)[c(1:7, 11, 9, 10, 8)]
)

# Add columns to match the shape of other inputs for plotting function
res_exp_1_time$collinearity <- 0
res_exp_1_time$n <- 200

# Use the data
usethis::use_data(res_exp_1_time, overwrite = TRUE)

# Simulation study mids --------------------------------------------------------

# Use the data
usethis::use_data(res_exp_1_mids, overwrite = TRUE)

# Simulation study 1.2: results ------------------------------------------------

# Change names of methods if required
levels(res_exp_1_2$methods) <- gsub("-la", "", levels(res_exp_1_2$methods))
levels(res_exp_1_2$methods) <- str_replace(levels(res_exp_1_2$methods), "blasso", "BLasso")
levels(res_exp_1_2$methods) <- str_replace(levels(res_exp_1_2$methods), "bridge", "BRidge")
levels(res_exp_1_2$methods) <- str_replace(levels(res_exp_1_2$methods), "MI-qp", "MI-QP")
levels(res_exp_1_2$methods) <- str_replace(levels(res_exp_1_2$methods), "MI-am", "MI-AM")
levels(res_exp_1_2$methods) <- str_replace(levels(res_exp_1_2$methods), "MI-OP", "MI-OR")
levels(res_exp_1_2$methods) <- str_replace(levels(res_exp_1_2$methods), "stepFor", "MI-SF")

# Replace NA in conditions with 0
res_exp_1_2$cond <- gsub("NA", "0", res_exp_1_2$cond)
res_exp_1_2$collinearity[is.na(res_exp_1_2$collinearity)] <- 0

# Make analysis a factor
res_exp_1_2$analysis <- factor(
    x = res_exp_1_2$analysis,
    levels = unique(res_exp_1_2$analysis),
    labels = c("PRB", "CIC", "CIW")
)

# Use the data
usethis::use_data(res_exp_1_2, overwrite = TRUE)

# Simulation study 1.2: time -------------------------------------------------------

# Change names of methods
levels(res_exp_1_2_time$variable) <- gsub("_", "-", levels(res_exp_1_2_time$variable))
levels(res_exp_1_2_time$variable) <- str_replace(levels(res_exp_1_2_time$variable), "_la", "")
levels(res_exp_1_2_time$variable) <- str_replace(levels(res_exp_1_2_time$variable), "blasso", "BLasso")
levels(res_exp_1_2_time$variable) <- str_replace(levels(res_exp_1_2_time$variable), "bridge", "BRidge")
levels(res_exp_1_2_time$variable) <- str_replace(levels(res_exp_1_2_time$variable), "_", "-")
levels(res_exp_1_2_time$variable) <- str_replace(levels(res_exp_1_2_time$variable), "MI-qp", "MI-QP")
levels(res_exp_1_2_time$variable) <- str_replace(levels(res_exp_1_2_time$variable), "MI-am", "MI-AM")
levels(res_exp_1_2_time$variable) <- str_replace(levels(res_exp_1_2_time$variable), "MI-OP", "MI-OR")
levels(res_exp_1_2_time$variable) <- str_replace(levels(res_exp_1_2_time$variable), "stepFor", "MI-SF")

# Replace NA in conditions with 0
res_exp_1_2_time$cond <- gsub("NA", "0", res_exp_1_2_time$cond)
res_exp_1_2_time$collinearity[is.na(res_exp_1_2_time$collinearity)] <- 0

# Add columns to match the shape of other inputs for plotting function
res_exp_1_2_time$n <- 200

# Use the data
usethis::use_data(res_exp_1_2_time, overwrite = TRUE)

# Resampling study: results ----------------------------------------------------

# Use the data
usethis::use_data(res_exp_4, overwrite = TRUE)

# Resampling study: time -------------------------------------------------------

# Produce data for plot
res_exp_4_time <- do.call(rbind, res_exp_4$out_time)

# Collect data
res_exp_4_time <- data.frame(
    cond = c("p = 243, n = 1000", "p = 243, n = 300"),
    p = c(243, 243),
    n = c(1000, 300),
    res_exp_4_time
)

# Melt
res_exp_4_time <- reshape2::melt(
    res_exp_4_time,
    id.var = c("cond", "p", "n")
)

# Change names of methods
levels(res_exp_4_time$variable) <- str_replace(levels(res_exp_4_time$variable), "_la", "")
levels(res_exp_4_time$variable) <- str_replace(levels(res_exp_4_time$variable), "blasso", "BLasso")
levels(res_exp_4_time$variable) <- str_replace(levels(res_exp_4_time$variable), "bridge", "BRidge")
levels(res_exp_4_time$variable) <- str_replace(levels(res_exp_4_time$variable), "_", "-")
levels(res_exp_4_time$variable) <- str_replace(levels(res_exp_4_time$variable), "MI-qp", "MI-QP")
levels(res_exp_4_time$variable) <- str_replace(levels(res_exp_4_time$variable), "MI-am", "MI-AM")
levels(res_exp_4_time$variable) <- str_replace(levels(res_exp_4_time$variable), "MI-OP", "MI-OR")
levels(res_exp_4_time$variable) <- str_replace(levels(res_exp_4_time$variable), "stepFor", "MI-SF")

# Add empty columns to match the shape of other inputs for plotting function
res_exp_4_time$collinearity <- 0 # corresponds to not considered
res_exp_4_time$pm <- 0 # corresponds to not considered

# Use the data
usethis::use_data(res_exp_4_time, overwrite = TRUE)
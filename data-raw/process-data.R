# Project:   mihd.results
# Objective: Prepare and deploy results for app
# Author:    Edoardo Costantini
# Created:   2023-05-02
# Modified:  2023-05-02
# Notes: 

library(stringr)

# Load data --------------------------------------------------------------------

# Load experiment 1 results
res_exp_1 <- readRDS("./data-raw/exp1_simOut_20230403_1631_res.rds")

# Load simulation study 1 time
res_exp_1_time <- readRDS("./data-raw/exp1_simOut_time.rds")

# Load results for simulation study 1.2
res_exp_1_2 <- readRDS("./data-raw/exp1_2_simOut_20230503_0838_res.rds")

# Load simulation study 1.2 time
res_exp_1_2_time <- readRDS("./data-raw/exp1_2_simOut_time.rds")

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

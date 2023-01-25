#==============================================================================#
                                # PREAMBLE #
#==============================================================================#
# Written by: Brian Prescott
# Last edited: 2022-12-18

# Purpose: This script compares uniform distribution estimates to the baseline
# estimates for the paper "Cash Payments and the Penny Policy Debate" by
# Brian Prescott and Oz Shy. The baseline estimates are those provided by the
# empirical distribution of cash transactions from 2015 to 2019.

# Cleaning out the environment before running the script
rm(list = ls())

last_solve_date <- "2023-01-24"

#==============================================================================#
                                # PACKAGES #
#==============================================================================#
library(tidyverse)
library(xtable)

#==============================================================================#
                                # FUNCITON #
#==============================================================================#
counter_results <- function(df1, df2) {
    output <- list(df1, df2) %>%
        purrr::map(
            .x = .,
            .f = function(df) {
                output <- df %>%
                    dplyr::group_by(only_20note) %>%
                    dplyr::summarize(
                        counter_burden = round(
                            x = mean(
                                x = n_tokens_counter,
                                na.rm = TRUE,
                                digits = 4
                            ),
                            digits = 4
                        )
                    ) %>%
                    data.frame()
                return(output)
            }
        )
    names(output) <- c("Symmetric", "Asymmetric")

    print(output)

}

#==============================================================================#
                                # PARAMETERS #
#==============================================================================#
# General directory for more robust exporting
directory <- stringr::str_remove(Sys.getenv("PWD"), "code")
# Insert the path to the dataset here
setwd(paste0(directory, "data/"))

load(paste0("./uniform-dist-baseline-results_", last_solve_date, ".RData"))

avg_burdens_list <- names(optimal_tokens_list) %>%
    as.list() %>%
    purrr::map(
        .x = .,
        .f = function(i) {
            output <- optimal_tokens_list[[i]] %>%
                dplyr::group_by(only_20note) %>%
                dplyr::summarize(
                    avg_burden = mean(x = n_tokens, na.rm = TRUE)
                ) %>%
                dplyr::mutate(upper_bound = i)

            return(output)
        }
    )
avg_burdens_df <- dplyr::bind_rows(avg_burdens_list)

# Comparing unrestricted average burden to the empirical distribution
avg_burdens_df %>%
    dplyr::filter(only_20note == "All coins and notes") %>%
    dplyr::mutate(change = (avg_burden - 3.14) / 3.14 * 100)

# Comparing restricted average burden to the empirical distribution
avg_burdens_df %>%
    dplyr::filter(only_20note == "Only $20 note") %>%
    dplyr::mutate(change = (avg_burden - 4.96) / 4.96 * 100)

# Now we are going to look at the counterfactual results
# Loading the data back in
load(
  file = stringr::str_c(
    "./uniform-policy-results-amnt_mean_", last_solve_date, ".RData"
  )
)
cat("First, we will look at the results when bar{a} = $mean\n")
counter_results(symmetric_pol_df, asymmetric_pol_df)
rm(symmetric_pol_df, asymmetric_pol_df)

cat("Now, we will look at the results when bar{a} = $100\n")
load(
  file = stringr::str_c(
    "./uniform-policy-results-amnt_100_", last_solve_date, ".RData"
  )
)
counter_results(symmetric_pol_df, asymmetric_pol_df)
rm(symmetric_pol_df, asymmetric_pol_df)

#==============================================================================#
                                # PREAMBLE #
#==============================================================================#
# Written by: Brian Prescott
# Last edited: 2022-12-17

# Purpose: This script solves the optimal currency denomination problem with a
# uniform distribution for the paper "Cash Payments and the Penny Policy Debate"
# by Brian Prescott and Oz Shy. The baseline economy is our present economy
# wherein all coins and notes are present.

# Notes: If this is your initial use of the code (i.e. if initial_run == TRUE),
# then be warned that the optimization routines take approximately 10 hours
# combined to run if using Windows. If on Windows, I suggest using the already
# generated model solutions datasets provided on GitHub to review the results.
# If you are running macOS or Linux, running the script from scratch is
# feasible. It should only take around 10 minutes thanks to parallel computing.
# It should be noted that the uasid variable reported is not actually the
# "uasid" variable  provided by USC but rather is the "id" variable created by
# the Atlanta Fed. The variable was simply renamed for the purposes of the
# script. This script reflects a major revision from the original draft.

# Cleaning out the environment before running the script
rm(list = ls())

#==============================================================================#
                                # LIBRARIES #
#==============================================================================#
library(tidyverse)
library(lpSolve)
library(xtable)
library(boot)

#==============================================================================#
                                # PARAMETERS #
#==============================================================================#
# General directory for more robust exporting
directory <- stringr::str_remove(Sys.getenv("PWD"), "code")
# Insert the path to the dataset here
setwd(paste0(directory, "data/"))

# This is seed we will use for replication purposes.
set.seed(1995)

#==============================================================================#
                                # FUNCTIONS #
#==============================================================================#
calc_optimal_tokens <- function(amounts_list, using_tokens, only_20note) {
  compute_optimal_tokens <- function(a) {
    amount <<- a #nolint
    tokens <- using_tokens
    n_tokens <- length(tokens)

    theta_p <- tokens # theta_p refers to the notation used in the paper
    if (only_20note == TRUE) {
      # Coercing all of the non-$20 positions to 0.
      theta_p[theta_p != 20] <- 0

    }

    f_obj <- rep(1, n_tokens * 2)
    f_constraint <- c(
      rep(1, n_tokens * 2), # row 1
      theta_p, rep(0, n_tokens), # row 2
      tokens, -tokens, # row 3
      diag(n_tokens * 2) # rows 4 - 27
    ) %>%
      matrix(
        data = .,
        nrow = 3 + (n_tokens * 2),
        byrow = TRUE
      )

    # The equality ensures that the payment and change equal the amount. The
    # 24 >= ensure that no negative token quantities occur.
    f_direction <- c(">=", ">=", "==", rep(">=", n_tokens * 2))
    f_rhs <- c(1, amount, amount, rep(0, n_tokens * 2)) #nolint

    solve_optimal_tokens <- lpSolve::lp(
      direction = "min",
      objective.in = f_obj,
      const.mat = f_constraint,
      const.dir = f_direction,
      const.rhs = f_rhs,
      int.vec = seq_along(f_obj),
      all.bin = FALSE
    )

    optimal_number_tokens <- round(x = solve_optimal_tokens$objval, digits = 0)

    optimal_tokens_df <- optimal_number_tokens %>%
      data.frame(
        n_tokens = .,
        a = amount
      ) %>%
      data.frame()

    return(optimal_tokens_df)
  }

  # Adding this line in for the progress bar
  suppressWarnings(
    progress_bar <- progress_estimated(length(amounts_list))
  )

  optimal_tokens_calculations <- amounts_list %>%
    purrr::map(
      .x = .,
      .f = compute_optimal_tokens
    ) %>%
    bind_rows()

  return(optimal_tokens_calculations)
}

#==============================================================================#
factor2numeric <- function(x)
  return(as.numeric(as.character(x)))

#==============================================================================#
                                # DATA IMPORT #
#==============================================================================#
# Loading in the dataset for cash only transactions
load(
  file = "change-burden-dfs_2021-09-10.RData"
)

# This is equivalent to the cash_transactions.rds in the "data" folder.
tran_df <- change_burden_dfs$cash_transactions %>%
  dplyr::rename(uasid = id)

#==============================================================================#
                                # MODEL SIMULATION #
#==============================================================================#
n_draws <- 20000
unif_bounds <- c(min, mean, median) %>%
  purrr::map(
    .x = .,
    .f = function(g) {
      output <- g(x = tran_df$amnt, na.rm = TRUE)
      return(output)
    }
  )
names(unif_bounds) <- c("min", "mean", "median")

num_cores <- 6

optimal_tokens_list <- c(unif_bounds$median, unif_bounds$mean, 60, 100) %>%
  as.list() %>%
  purrr::map(
    .x = .,
    .f = function(upper_bound) {
      cat("Uniform distribution upper bound: ", upper_bound, "\n")

      # Solving the model if we haven't already solved it
      amnt_list <- upper_bound %>%
        runif(n = n_draws, min = unif_bounds$min, max = .) %>%
        round(x = ., digits = 2)

      denoms <- c(
        0.01, 0.05, 0.10, 0.25, 0.50, # Coins
        1, 5, 10, 20, 50, 100 # Notes
      )

      optimal_tokens_baseline <- amnt_list %>%
        parallel::mclapply(
          X = .,
          FUN = calc_optimal_tokens,
          using_tokens = denoms[which(denoms <= upper_bound)],
          only_20note = FALSE,
          mc.cores = num_cores,
          mc.preschedule = TRUE,
          mc.cleanup = TRUE
        )

      optimal_tokens_only_20note <- amnt_list %>%
        parallel::mclapply(
          X = .,
          FUN = calc_optimal_tokens,
          using_tokens = denoms[which(denoms <= upper_bound)],
          only_20note = TRUE,
          mc.cores = num_cores,
          mc.preschedule = TRUE,
          mc.cleanup = TRUE
        )

      optimal_tokens_df <- optimal_tokens_baseline %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(only_20note = "All coins and notes") %>%
        dplyr::bind_rows(
          x = .,
          y = optimal_tokens_only_20note %>%
            dplyr::bind_rows() %>%
            dplyr::mutate(only_20note = "Only $20 note")
        ) %>%
        tibble::as_tibble()

      return(optimal_tokens_df)
    }
  )
names(optimal_tokens_list) <- c(
  "amnt_median", "amnt_mean", "amnt_60", "amnt_100"
)
save(
  list = c("optimal_tokens_list"),
  file = str_c("uniform-dist-baseline-results_", Sys.Date(), ".RData")
)
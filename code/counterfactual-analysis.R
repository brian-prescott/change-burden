#==============================================================================#
                                # PREAMBLE #
#==============================================================================#
# Written by: Brian Prescott
# Last edited: 2022-07-15

# Purpose: This script solves the optimal currency denomination problem in the
# counterfactual economy where pennies have been eliminated. The simulation is
# conducted for the paper "Cash Payments and the Penny Policy Debate" by
# Brian Prescott and Oz Shy.

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

# THESE SHOULD BE CONSIDERED BEFORE EVERY RUN.
initial_run <- TRUE
run_stylized <- TRUE
last_base_run <- "2022-07-16"
last_counter_run <- "2022-07-16"

#==============================================================================#
                                # LIBRARIES #
#==============================================================================#
library(tidyverse)
library(stringi)
library(lpSolve)
library(parallel)

#==============================================================================#
                                # PARAMETERS #
#==============================================================================#
# insert the path to your datasets here
# General directory for more robust exporting
directory <- stringr::str_remove(Sys.getenv("PWD"), "code")
# Insert the path to the dataset here
setwd(paste0(directory, "/data/"))

# Setting the seed for replication
set.seed(1995)

#==============================================================================#
                                  # DATA IMPORT #
#==============================================================================#
# This date is fixed in time because I made the datasets on this date.
load(
  file = "change-burden-dfs_2021-09-10.RData"
)

# This is equivalent to the cash_transactions.rds in the "data" folder.
tran_df <- change_burden_dfs$cash_transactions %>%
  dplyr::rename(uasid = id)

# The all transactions dataset is the in-person transactions.
in_person_df <- change_burden_dfs$all_transactions %>%
  dplyr::rename(uasid = id)

#==============================================================================#
                                # FUNCTIONS #
#==============================================================================#
# This is the simple symmetric rounding policy.
symmetric_rounding <- function(amount) {
  counterfactual_amount <- round(amount / 0.05) * 0.05

  return(counterfactual_amount)
}

#==============================================================================#
# This rounding policy asserts that firms will only round their prices up to
# the nearest 5 cents or they will leave their prices alone. The only deviation
# from this is when an item is priced at, say, $10.99 in which case they would
# round prices down to $10.95 so as not to lose out on the marketing angle of
# the salience of the first two numbers rather than the last two.
asymmetric_rounding <- function(amount) {
  rounded_down_lower <- as.character(1:2)
  rounded_down_upper <- as.character(6:7)
  rounded_down <- c(rounded_down_lower, rounded_down_upper)

 if (stringr::str_sub(stringi::stri_reverse(amount), 1, 1) %in% rounded_down &
              !(amount %in% c(seq(1, 1000000, 10), seq(6, 1000000, 10),
                              seq(2, 1000000, 10), seq(7, 1000000, 10))) &
              stringr::str_sub(stringi::stri_reverse(amount), 1, 2) != "1." &
              stringr::str_sub(stringi::stri_reverse(amount), 1, 2) != "2." &
              stringr::str_sub(stringi::stri_reverse(amount), 1, 2) != "6." &
              stringr::str_sub(stringi::stri_reverse(amount), 1, 2) != "7.") {
    new_amount <- stringr::str_c(
        ifelse(
          test = stringr::str_sub(
            stringi::stri_reverse(amount), 1, 1
          ) %in% rounded_down_lower,
          yes = "5",
          no = "8"
        ),
        stringr::str_sub(stringi::stri_reverse(amount), 2)
      ) %>%
      stri_reverse() %>%
      as.numeric()
    counter_amount <- round(new_amount / 0.05) * 0.05

  } else {
    counter_amount <- round(amount / 0.05) * 0.05

  }

  return(counter_amount)
}

#==============================================================================#
calc_optimal_tokens <- function(amounts_list, using_tokens, only_20note) {
  compute_optimal_tokens <- function(a) {
    amount <<- a #nolint
    tokens <- using_tokens
    n_tokens <- length(tokens)

    theta_p <- tokens # theta_p refers to the notation used in the paper
    if (only_20note == TRUE) {
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

    optimal_number_tokens <- round(solve_optimal_tokens$objval, digits = 0)

    optimal_tokens_df <- optimal_number_tokens %>%
      data.frame(
        n_tokens = .,
        a = amount
      ) %>%
      data.frame()

    return(optimal_tokens_df)
  }

  optimal_tokens_calculations <- amounts_list %>%
    purrr::map(
      .x = .,
      .f = compute_optimal_tokens
    ) %>%
    dplyr::bind_rows()

  return(optimal_tokens_calculations)
}

#==============================================================================#
counterfactual_sim <- function(rounding_policy) {
  if (rounding_policy == "symmetric") {
    counterfactual_tran_df <- tran_df %>%
      dplyr::mutate_at(
        .vars = dplyr::vars(amnt),
        .funs = symmetric_rounding
      )

  } else if (rounding_policy == "asymmetric") {
    counter_amnts <- tran_df$amnt %>%
      as.list() %>%
      purrr::map(
        .x = .,
        .f = asymmetric_rounding
      ) %>%
      unlist() %>%
      tibble::tibble(amnt = .)
    counterfactual_tran_df <- tran_df %>%
      dplyr::select(-amnt) %>%
      dplyr::bind_cols(
        x = .,
        y = counter_amnts
      )

  } else {
    stop(paste0(
      "This rounding policy is not supported. Please choose either",
      "'symmetric' or 'asymmetric'"
    ))

  }

  if (initial_run == TRUE) {
      if (run_stylized == TRUE) {
        stylized_exercise_2 <- calc_optimal_tokens(
          amounts_list = as.list(seq(4, 5, 0.05)),
          using_tokens = c(
            0.05, 0.10, 0.25, 0.50,
            1, 5, 10, 20, 50, 100
          ),
          only_20note = FALSE
        )

      }

    counterfactual_amnt_list <- as.list(counterfactual_tran_df$amnt)
    num_cores <- 6

    optimal_tokens_baseline <- counterfactual_amnt_list %>%
      parallel::mclapply(
        X = .,
        FUN = calc_optimal_tokens,
        using_tokens = c(0.05, 0.10, 0.25, 0.50, 1, 5, 10, 20, 50, 100),
        only_20note = FALSE,
        mc.cores = num_cores,
        mc.preschedule = TRUE,
        mc.cleanup = TRUE
      ) %>%
      dplyr::bind_rows()

    optimal_tokens_only_20note <- counterfactual_amnt_list %>%
      parallel::mclapply(
        X = .,
        FUN = calc_optimal_tokens,
        using_tokens = c(0.05, 0.10, 0.25, 0.50, 1, 5, 10, 20, 50, 100),
        only_20note = TRUE,
        mc.cores = num_cores,
        mc.preschedule = TRUE,
        mc.cleanup = TRUE
      ) %>%
      dplyr::bind_rows()

    counter_optimal_tokens_df <- optimal_tokens_baseline %>%
      dplyr::mutate(only_20note = "All coins and notes") %>%
      dplyr::bind_rows(
        x = .,
        y = optimal_tokens_only_20note %>%
          dplyr::mutate(only_20note = "Only $20 note")
      )

    save(
      list = c("counter_optimal_tokens_df", "stylized_exercise_2"),
      file = stringr::str_c("counterfactual-results_", Sys.Date(), ".RData")
    )

  } else {
    counter_path <- stringr::str_c(
      "counterfactual-results_", last_counter_run, ".RData"
    )
    load(counter_path)

  }

  # We will need this data regardless
  baseline_path <- stringr::str_c(
    "baseline-results_", last_base_run, ".RData"
  )
  load(baseline_path)

  postcounter_df <- counter_optimal_tokens_df %>%
    dplyr::bind_cols(
      x = .,
      y = dplyr::bind_rows(
        x = counterfactual_tran_df,
        y = counterfactual_tran_df
      )
    ) %>%
    dplyr::mutate(alpha_tilde = a) %>%
    dplyr::rename(n_tokens_counter = n_tokens) %>%
    dplyr::bind_cols(
      x = .,
      y = optimal_tokens_df %>%
        dplyr::select(n_tokens)
    ) %>%
    dplyr::inner_join(
      x = .,
      y = counter_optimal_tokens_df %>%
        dplyr::bind_cols(., dplyr::bind_rows(tran_df, tran_df)) %>%
        dplyr::rename(alpha = amnt) %>%
        dplyr::select(
          uasid, date, diary_day, tran, alpha, only_20note
        ),
      by = c("uasid", "date", "diary_day", "tran", "only_20note")
    ) %>%
    dplyr::mutate(
      Delta_alpha = round(alpha_tilde, 2) - round(alpha, digits = 2),
      delta_penny = abs(Delta_alpha),
      Delta_burden = round(n_tokens_counter, 0) - round(n_tokens, digits = 0),
      payee = factor(
        x = payee,
        levels = c(1:8),
        labels = c(
          "Financial services provider",
          "Education provider",
          "Hospital, doctor or dentist",
          "Government",
          "Nonprofit, charity or religion",
          "A person",
          "Retail store or online retailer",
          "Business that primarily sells services"
        )
      )
    ) %>%
    tibble::as_tibble()

  return(postcounter_df)
}

#==============================================================================#
        # COUNTERFACTUAL SIMULATION: ELIMINATION OF THE PENNY #
#==============================================================================#
# If you have already done the initial solving then you can skip the first
# part of the conditional. Note, solving the model under both policies takes
# approximately 20 minutes with 6 cores.
if (initial_run == TRUE) {
  # Re-solving the model under the different rounding policies
  symmetric_pol_df <- counterfactual_sim(rounding_policy = "symmetric")
  asymmetric_pol_df <- counterfactual_sim(rounding_policy = "asymmetric")

  save(
    list = stringr::str_c(c("asymmetric", "symmetric"), "_pol_df"),
    file = stringr::str_c("policy-results_", Sys.Date(), ".RData")
  )

} else {
  load(
    file = stringr::str_c("policy-results_", last_counter_run, ".RData")
  )

}
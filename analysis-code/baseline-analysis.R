#===============================================================================
# PREAMBLE #
#===============================================================================

# Written by: Brian Prescott
# Last edited: 2021-05-06

# Purpose: This script generates all of the figures and tables, solves the
# optimal currency denomination problem and conducts the counterfactual policy
# simulation for the paper "The Burden of Change and the Penny Policy Debate" by
# Brian Prescott and Oz Shy.

# Notes: If this is your initial use of the code (i.e. if initial_run == TRUE),
# then be warned that the optimization routines take approximately 10 hours
# combined to run if using Windows. I suggest using the already generated model
# solutions datasets provided on GitHub to review the results. Outside of that,
# the window containing the script should be extended to the edge of the dashed
# lines for optimal readibility. The window is approximately 80 lines long. It
# should be noted that I the uasid variable reported is not actually the "uasid"
# variable  provided by USC but rather is the "id" variable created by the
# Atlanta Fed. The variable was simply renamed for the purposes of the script.
# This script reflects a major revision from the original draft.

# Cleaning out the environment before running the script
rm(list = ls())

# THESE SHOULD BE CONSIDERED BEFORE EVERY RUN
run_stylized <- TRUE

#===============================================================================
# LIBRARIES #
#===============================================================================
library(tidyverse)
library(haven)
library(lpSolve)
library(stargazer)
library(EnvStats)
library(data.table)
library(grDevices)
library(graphics)
library(xtable)
library(boot)

#===============================================================================
# PARAMETERS #
#===============================================================================
# General directory for more robust exporting
directory <- stringr::str_remove(Sys.getenv("PWD"), "analysis-code")
# Insert the path to the dataset here
setwd(paste0(directory, "data/"))

set.seed(1995)

#===============================================================================
# FUNCTIONS #
#===============================================================================
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
    f_rhs <- c(1, amount, amount, rep(0, n_tokens * 2))

    solve_optimal_tokens <- lpSolve::lp(
      direction = "min",
      objective.in = f_obj,
      const.mat = f_constraint,
      const.dir = f_direction,
      const.rhs = f_rhs,
      int.vec = 1:length(f_obj),
      all.bin = FALSE
    )

    optimal_number_tokens <- round(solve_optimal_tokens$objval, digits = 0)

    optimal_tokens_df <- optimal_number_tokens %>%
      data.frame(
        n_tokens = .,
        a = amount
      ) %>%
      data.frame()

    # Adding in a progress bar to keep track of the optimization
    # progress_bar$tick()$print()

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

#===============================================================================
factor2numeric <- function(x)
  return(as.numeric(as.character(x)))

#===============================================================================
                                  # DATA IMPORT #
#===============================================================================
tran_df <- read_rds("cash_transactions_df.rds") %>%
  dplyr::rename(uasid = id)

#===============================================================================
                            # DESCRIPTIVE ANALYSIS #
#===============================================================================
# Number of consumers
tran_df$uasid %>%
  unique() %>%
  length()

# Number of transactions
nrow(tran_df)

# Merchant type summary statistics
merch_sumStats <- list(mean, sd, median, min, max) %>%
  purrr::map(
    .x = .,
    .f = function(h) {
      sumS_df <- tran_df %>%
        mutate(payee = factor(
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
      )

      setDT(sumS_df)[, c(levels(sumS_df$payee), "payee") :=
                       c(lapply(levels(payee), function(x)
                         as.integer(x == payee)), .(NULL))]

      table <- sumS_df[, (ncol(sumS_df) - 7):ncol(sumS_df)] %>%
        summarise_all(.tbl = .,
                      .funs = ~ h(x = ., na.rm = TRUE)) %>%
        round(2) %>%
        t() %>%
        data.frame() %>%
        rownames_to_column(.data = .,
                           var = "Covariate") %>%
        arrange(Covariate) %>%
        dplyr::rename(h = ".")
    }
  ) %>%
  purrr::reduce(
    .x = .,
    .f = inner_join,
    by = "Covariate"
  )

# Summary statistics generation
sumStat_table <- list(mean, sd, median, min, max) %>%
  purrr::map(
    .x = .,
    .f = function(h) {
      sumS_df <- tran_df

      setDT(sumS_df)[, c(levels(sumS_df$income_), "income_") :=
                       c(lapply(levels(income_), function(x)
                         as.integer(x == income_)), .(NULL))]

      setDT(sumS_df)[, c(levels(sumS_df$age_cohort_), "age_cohort_") :=
                       c(lapply(levels(age_cohort_), function(x)
                         as.integer(x == age_cohort_)), .(NULL))]

      setDT(sumS_df)[, c(levels(sumS_df$education_), "education_") :=
                       c(lapply(levels(education_), function(x)
                         as.integer(x == education_)), .(NULL))]

      table <- sumS_df %>%
        dplyr::select(
          -c(
            ends_with(".y"),
            hispaniclatino_group,
            birthYear,
            payment,
            income_hh,
            year,
            date,
            tran,
            diary_day,
            pi,
            merch,
            highest_education,
            payee,
            marital_status,
            used_cash,
            banked,
            in_person,
            uasid
          )
        ) %>%
        mutate(unbanked = as.numeric(bnk_acnt_adopt == 0)) %>%
        dplyr::select(-bnk_acnt_adopt) %>%
        summarise_all(.tbl = .,
                      .funs = ~ h(x = ., na.rm = TRUE)) %>%
        t() %>%
        data.frame() %>%
        rownames_to_column(.data = .,
                           var = "Covariate") %>%
        arrange(Covariate) %>%
        dplyr::rename(h = ".")

      return(table)

    }
  ) %>%
  purrr::reduce(
    .x = .,
    .f = inner_join,
    by = "Covariate"
  ) %>%
  dplyr::rename(
    mean = h.x,
    sd = h.y,
    median = h
  )
sumStat_table %>%
  bind_rows(
    x = .,
    y = merch_sumStats %>%
      dplyr::rename(
        mean = h.x,
        sd = h.y,
        median = h)
  ) %>%
  mutate_at(
    .vars = vars(-Covariate),
    .funs = ~ round(x = ., digits = 2)
  ) %>%
  dplyr::rename(max = median) %>%
  dplyr::rename(
    median = h.x.x,
    min = h.y.y
  ) %>%
  xtable::xtable() %>%
  print(include.rownames = FALSE)

#===============================================================================
                        # MODEL SIMULATION #
#===============================================================================

# Here we run the simple exercise of evaluating the burden at each dollar value
# between [4.01, 5]. We choose this interval because it allows us to see the
# role that notes play better than just the role of coins that we see in the
# [0.01, 1] interval. The $2 note does not exist here since we will never
# leave it in the model.
if (run_stylized == TRUE) {
  stylized_exercise_1 <- calc_optimal_tokens(
      amounts_list = as.list(seq(4, 5, 0.01)),
      using_tokens = c(0.01, 0.05, 0.10, 0.25, 0.50,
                       1, 5, 10, 20, 50, 100),
      only_20note = FALSE
    )

}

# Solving the model if we haven't already solved it
amnt_list <- as.list(tran_df$amnt)

# We are using parallel computing here to speed up the optimization algos.
# I am putting in my own number of cores, however, you will need to change it
# for your machine. If your machine is Windows, then this will not work and
# you will need to set num_cores == 1 since Windows does not support the
# parallel::mclapply() function. A potential work around is to spin up a
# Windows Subsytem for Linux instance and run the code on there since you can
# take advantage of a Linux distro. The speed gains make it worthwhile. A
# windows machine will take approx. 10 hours while a Linux machine using
# parallel computing takes about 10 minutes.
num_cores <- 6

optimal_tokens_baseline <- amnt_list %>%
  parallel::mclapply(
    X = .,
    FUN = calc_optimal_tokens,
    using_tokens = c(
      0.01, 0.05, 0.10, 0.25, 0.50,
      1, 5, 10, 20, 50, 100
    ),
    only_20note = FALSE,
    mc.cores = num_cores,
    mc.preschedule = TRUE,
    mc.cleanup = TRUE
  )

optimal_tokens_only_20note <- amnt_list %>%
  parallel::mclapply(
    X = .,
    FUN = calc_optimal_tokens,
    using_tokens = c(
      0.01, 0.05, 0.10, 0.25, 0.50,
      1, 5, 10, 20, 50, 100
    ),
    only_20note = TRUE,
    mc.cores = num_cores,
    mc.preschedule = TRUE,
    mc.cleanup = TRUE
  )

optimal_tokens_df <- optimal_tokens_baseline %>%
  bind_rows() %>%
  mutate(only_20note = "All coins and notes") %>%
  bind_rows(
    x = .,
    y = optimal_tokens_only_20note %>%
      bind_rows() %>%
      mutate(only_20note = "Only $20 note")
  ) %>%
  as_tibble()

save(
  list = c("optimal_tokens_df", "stylized_exercise_1"),
  file = str_c("baseline-results_", Sys.Date(), ".RData")
)

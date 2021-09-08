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
# combined to run. I suggest using the already generated model solutions
# datasets provided on GitHub to review the results. Outside of that, the window
# containing the script should be extended to the edge of the dashed lines for
# optimal readibility. The window is approximately 80 lines long. It should be
# noted that I the uasid variable reported is not actually the "uasid" variable
# provided by USC but rather is the "id" variable created by the Atlanta Fed.
# The variable was simply renamed for the purposes of the script. This script
# reflects a major revision from the original draft.

# Cleaning out the environment before running the script
rm(list = ls())
initial_run <- TRUE
run_stylized <- TRUE
last_run <- "2021-09-07"

#===============================================================================
# LIBRARIES #
#===============================================================================
library(tidyverse)
library(haven)
library(lpSolve)
library(stargazer)
library(EnvStats)
library(grDevices)
library(graphics)
library(xtable)

#===============================================================================
# PARAMETERS #
#===============================================================================
# insert the path to your datasets here
# General directory for more robust exporting
directory <- str_c(Sys.getenv("OLDPWD"), "/change-burden/")
# Insert the path to the dataset here
setwd(paste0(directory, "data/"))

set.seed(1995)

#===============================================================================
                                  # DATA IMPORT #
#===============================================================================
tran_df <- read_rds("cash_transactions_df.rds") %>%
  dplyr::rename(uasid = id)

#===============================================================================
# FUNCTIONS #
#===============================================================================
# These are just wrapper functions
p25 <- function(X, na.rm)
  return(quantile(x = X, probs = 0.25, na.rm = na.rm))

p75 <- function(X, na.rm)
  return(quantile(x = X, probs = 0.75, na.rm = na.rm))

p95 <- function(X, na.rm)
  return(quantile(x = X, probs = 0.95, na.rm = na.rm))

#===============================================================================
# This is the simple symmetric rounding policy.
symmetric_rounding <- function(amount) {
  counterfactual_amount <- round(amount / 0.05) * 0.05

  return(counterfactual_amount)
}

#===============================================================================
# This rounding policy asserts that firms will only round their prices up to
# the nearest 5 cents or they will leave their prices alone. The only deviation
# from this is when an item is priced at, say, $10.99 in which case they would
# round prices down to $10.95 so as not to lose out on the marketing angle of
# the salience of the first two numbers rather than the last two.
asymmetric_rounding <- function(amount) {
  rounded_down_lower <- as.character(1:2)
  rounded_down_upper <- as.character(6:7)
  rounded_down <- c(rounded_down_lower, rounded_down_upper)

  if (str_sub(amount, 4, 5) == "99") {
    counter_amount <- as.numeric(
      str_c(str_sub(amount, 1, 3), "95")
    )
  } else if (str_sub(amount, 5, 5) %in% rounded_down) {
    new_amount <- as.numeric(
      str_c(
        str_sub(amount, 1, 4),
        ifelse(
          test = str_sub(amount, 5, 5) %in% rounded_down_lower,
          yes = "5",
          no = "8"
        )
      )
    )
    counter_amount <- round(new_amount / 0.05) * 0.05

  } else {
    counter_amount <- round(amount / 0.05) * 0.05

  }

  return(counter_amount)
}

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

    return(optimal_tokens_df)
  }

  optimal_tokens_calculations <- amounts_list %>%
    purrr::map(
      .x = .,
      .f = compute_optimal_tokens
    ) %>%
    bind_rows()

  return(optimal_tokens_calculations)
}

#===============================================================================
counterfactual_sim <- function(rounding_policy) {

  if (rounding_policy == "symmetric") {
    counterfactual_tran_df <- tran_df %>%
      mutate_at(
        .vars = vars(amnt),
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
      tibble(amnt = .)


    counterfactual_tran_df <- tran_df %>%
      dplyr::select(-amnt) %>%
      bind_cols(
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
          amounts_list = as.list(seq(4.05, 5, 0.05)),
          using_tokens = c(
            0.05, 0.10, 0.25, 0.50,
            1, 5, 10, 20, 50, 100
          ),
          only_20note = FALSE
        )

      }

    counterfactual_amnt_list <- as.list(counterfactual_tran_df$amnt)
    num_cores <- 6

    counter_optimal_tokens_baseline <- counterfactual_amnt_list %>%
      parallel::mclapply(
        X = .,
        FUN = calc_optimal_tokens,
        using_tokens = c(0.05, 0.10, 0.25, 0.50, 1, 5, 10, 20, 50, 100),
        only_20note = FALSE,
        mc.cores = num_cores,
        mc.preschedule = TRUE,
        mc.cleanup = TRUE
      ) %>%
      bind_rows()

    counter_optimal_tokens_only_20note <- counterfactual_amnt_list %>%
      parallel::mclapply(
        X = .,
        FUN = calc_optimal_tokens,
        using_tokens = c(0.05, 0.10, 0.25, 0.50, 1, 5, 10, 20, 50, 100),
        only_20note = TRUE,
        mc.cores = num_cores,
        mc.preschedule = TRUE,
        mc.cleanup = TRUE
      ) %>%
      bind_rows()

    counterfactual_optimal_tokens_df <- counter_optimal_tokens_baseline %>%
      mutate(only_20note = "All coins and notes") %>%
      bind_rows(
        x = .,
        y = counter_optimal_tokens_only_20note %>%
          mutate(only_20note = "Only $20 note")
      )

    save(
      list = c("counterfactual_optimal_tokens_df", "stylized_exercise_2"),
      file = str_c("counterfactual-results_", Sys.Date(), ".RData")
    )

  } else {
    counter_path <- str_c(
      "counter_optimal_denom_model_solutions_", last_run, ".RData"
    )
    load(counter_path)

  }

  postCounter_df <- counterfactual_optimal_tokens_df %>%
    bind_cols(
      x = .,
      y = bind_rows(
        x = counterfactual_tran_df,
        y = counterfactual_tran_df
      )
    ) %>%
    mutate(alpha_tilde = a) %>%
    dplyr::rename(n_tokens_counter = n_tokens) %>%
    inner_join(
      x = .,
      y = counterfactual_optimal_tokens_df %>%
        bind_cols(., bind_rows(tran_df, tran_df)) %>%
        dplyr::rename(alpha = amnt) %>%
        dplyr::select(
          uasid, date, diary_day, tran, alpha, only_20note, n_tokens
        ),
      by = c("uasid", "date", "diary_day", "tran", "only_20note")
    ) %>%
    mutate(
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
    as_tibble()

  return(postCounter_df)
}

#===============================================================================
        # COUNTERFACTUAL SIMULATION: ELIMINATION OF THE PENNY #
#===============================================================================
# Re-solving the model under the different rounding policies
symmetric_pol_df <- counterfactual_sim(rounding_policy = "symmetric")
asymmetric_pol_df <- counterfactual_sim(rounding_policy = "asymmetric")

save(
  list = str_c(c("asymmetric", "symmetric"), "_pol_df"),
  file = str_c("policy-results_", Sys.Date(), ".RData")
)

#===============================================================================
# Here we are going to estimate the price effects of the rounding policies
# on consumers. We aggregate the transactions to the monthly level so that
# the results are more interpretable.
price_effects_results <- list(symmetric_pol_df, asymmetric_pol_df) %>%
  as.list() %>%
  purrr::map(
    .x = .,
    .f = function(using_df) {
      
      matching_df <- tibble(
        diary_day = rep(c(1:3), length(2015:2019)),
        year = 2015:2019
      )
      est_df <- full_join(
        x = using_df,
        y = matching_df,
        by = c("uasid", "diary_day", "year")
      )

    }
  )

#===============================================================================
# EVERYTHING BENEATH HERE WILL NEED TO GET EDITED OR WRAPPED INTO A MAP()
# FUNCTION SO THAT IT ACCOUNTS FOR BOTH THE SYMMETRIC AND ASYMMETRIC POLICIES

# Expected number of tokens (coins and notes)
subset(postCounter_df, has_2note != "No $2 note")$n_tokens %>%
  mean(na.rm = TRUE) %>%
  print()
subset(postCounter_df, has_2note == "No $2 note")$n_tokens %>%
  mean(na.rm = TRUE) %>%
  print()

subset(postCounter_df, has_2note != "No $2 note")$n_tokens_counter %>%
  mean(na.rm = TRUE) %>%
  print()
subset(postCounter_df, has_2note == "No $2 note")$n_tokens_counter %>%
  mean(na.rm = TRUE) %>%
  print()

#===============================================================================
# Summary statistics
c(mean, sd, median, p25, p75, p95, min, max) %>%
  as.list() %>%
  purrr::map(
    .f = function(h) {
      output <- h(postCounter_df$n_tokens, na.rm = TRUE)

      return(output)
    }
  )

#===============================================================================
# Conditional summary statistics
c(mean, sd, median, p25, p75, p95, min, max) %>%
  as.list() %>%
  purrr::map(
    .f = function(h) {
      output <- h(subset(postCounter_df, alpha >= 0.01 &
                           alpha <= 100)$n_tokens,
                  na.rm = TRUE)

      return(output)
    }
  )

c(mean, sd, median, p25, p75, p95, min, max) %>%
  as.list() %>%
  purrr::map(
    .f = function(h) {
      output <-
        h(subset(postCounter_df, alpha  > 100)$n_tokens, na.rm = TRUE)

      return(output)
    }
  )

#===============================================================================

c(mean, sd, median, p25, p75, p95, min, max) %>%
  as.list() %>%
  purrr::map(
    .f = function(h) {
      output <- h(postCounter_df$n_tokens_counter, na.rm = TRUE)

      return(output)
    }
  )

#===============================================================================

c(mean, sd, median, p25, p75, p95, min, max) %>%
  as.list() %>%
  purrr::map(
    .f = function(h) {
      output <-
        h(subset(postCounter_df, alpha >= 0.01 &
                   alpha <= 100)$n_tokens,
          na.rm = TRUE)

    }
  )

c(mean, sd, median, p25, p75, p95, min, max) %>%
  as.list() %>%
  purrr::map(
    .f = function(h) {
      output <- h(subset(postCounter_df, alpha  > 100)$n_tokens_counter,
                  na.rm = TRUE)

      return(output)
    }
  )

#===============================================================================
                          # HYPOTHESIS TESTING #
#===============================================================================
set.seed(1995)

# Turning the hypothesis testing code into a function
burden_delta_hypTest <- function(df) {
  out <- df %>%
    summarize(
      mean_current = mean(n_tokens, na.rm = TRUE),
      mean_counter = mean(n_tokens_counter, na.rm = TRUE),
      numer = sqrt(
        var(n_tokens_counter) + var(n_tokens) -
          2 * cov(n_tokens_counter, n_tokens)
      ),
      denomi = sqrt(n()),
      se = numer / denomi
    ) %>%
    mutate(
      Delta = mean_counter - mean_current,
      ci_lower = Delta - 2.576 * se,
      ci_upper = Delta + 2.576 * se,
      interval = str_c("[", round(ci_lower, 2), ", ", round(ci_upper, 2), "]")
    ) %>%
    dplyr::select(mean_current, mean_counter, Delta, se, interval)

  return(out)
}

hyp_tests <- c("All coins and notes", "No $2 note") %>%
  as.list() %>%
  purrr::map(
    .f = function(which_economy) {
      using_data <- postCounter_df %>%
        subset(has_2note == which_economy) %>%
        mutate(economy = which_economy)

      all_calc <- using_data %>%
        burden_delta_hypTest(df = .) %>%
        mutate(economy = which_economy,
               var = "all")

      lt100_calc <- subset(using_data, alpha >= 0.01 & alpha <= 100) %>%
        burden_delta_hypTest(df = .) %>%
        mutate(economy = which_economy,
               var = "<= 100")

      gt100_calc <- subset(using_data, alpha > 100) %>%
        burden_delta_hypTest(df = .) %>%
        mutate(economy = which_economy,
               var = "> 100")

      edu_calc <- using_data$education_ %>%
        unique() %>%
        na.omit() %>%
        str_c() %>%
        as.list() %>%
        map(
          .f = function(e) {
            print(e)

            out <- using_data %>%
              subset(education_ == e) %>%
              burden_delta_hypTest(df = .) %>%
              mutate(economy = which_economy,
                     var = e)

            return(out)

          }
        )

      income_calc <- using_data$income_ %>%
        unique() %>%
        na.omit() %>%
        str_c() %>%
        as.list() %>%
        map(
          .f = function(i) {
            print(i)

            out <- using_data %>%
              subset(income_ == i) %>%
              burden_delta_hypTest(df = .) %>%
              mutate(economy = which_economy,
                     var = i)

            return(out)

          }
        )

      output <- all_calc %>%
        bind_rows(., lt100_calc) %>%
        bind_rows(., gt100_calc) %>%
        bind_rows(., edu_calc) %>%
        bind_rows(., income_calc)

      return(output)
    }
  )

bind_rows(hyp_tests) %>%
  subset(economy == "All coins and notes") %>%
  dplyr::select(-economy) %>%
  dplyr::select(var, everything()) %>%
  xtable::xtable() %>%
  print(include.rownames = FALSE)

bind_rows(hyp_tests) %>%
  subset(economy == "No $2 note") %>%
  dplyr::select(-economy) %>%
  dplyr::select(var, everything()) %>%
  xtable::xtable() %>%
  print(include.rownames = FALSE)

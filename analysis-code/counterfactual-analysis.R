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
last_run <- "2021-09-05"

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
directory <- str_c(
  Sys.getenv("USERPROFILE"), 
  "\\PrescottLibrary\\papers\\academic-papers\\burden-cash\\"
)
setwd(paste0(directory, "data\\"))

set.seed(1995)

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
counterfactual_sim <- function(rounding_policy) {
  if (rounding_policy == "symmetric") {
    counterfactual_tran_df <- tran_df %>%
      mutate_at(
        .vars = vars(amnt),
        .funs = symmetric_rounding
      )
    
  } else if (rounding_policy == "asymmetric") {
    counterfactual_tran_df <- tran_df %>%
      mutate_at(
        .vars = vars(amnt),
        .funs = asymmetric_rounding
      )
    
  } else {
    stop(paste0(
      "This rounding policy is not supported. Please choose either",
      "'symmetric' or 'asymmetric'"
    ))
    
  }
  
  if (initial_run == TRUE) {
    counterfactual_amnt_list <- as.list(counterfactual_tran_df$amnt)
    
    counter_optimal_tokens_baseline <- counterfactual_amnt_list %>% 
      parallel::mclapply(
        X = .,
        FUN = calc_optimal_tokens,
        using_tokens = c(0.05, 0.10, 0.25, 0.50, 1, 5, 10, 20, 50, 100),
        only_20note = FALSE,
        mc.cores = num_cores,
        mc.preschedule = TRUE,
        mc.cleanup = TRUE
      )
    
    counter_optimal_tokens_only_20note <- counterfactual_amnt_list %>% 
      parallel::mclapply(
        X = .,
        FUN = calc_optimal_tokens,
        using_tokens = c(0.05, 0.10, 0.25, 0.50, 1, 5, 10, 20, 50, 100),
        only_20note = TRUE,
        mc.cores = num_cores,
        mc.preschedule = TRUE,
        mc.cleanup = TRUE
      )
    
    counterfactual_optimal_tokens_df <- counter_optimal_tokens_baseline %>%
      mutate(only_20note = "All coins and notes") %>%
      bind_rows(
        x = .,
        y = counter_optimal_tokens_only_20note %>%
          mutate(only_20note = "Only $20 note")
      )
    
    write_rds(
      x = counterfactual_optimal_tokens_df,
      file = str_c(
        "counter_optimal_denom_model_solutions_",
        Sys.Date(),
        ".rds"
      )
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
      y = optimal_tokens_df %>%
        bind_cols(., bind_rows(tran_df, tran_df)) %>%
        dplyr::rename(alpha = amnt) %>%
        dplyr::select(uasid, date, diary_day, tran, alpha, has_2note, n_tokens),
      by = c("uasid", "date", "diary_day", "tran", "has_2note")
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
    )
  
  return(postCounter_df)
}

#===============================================================================
        # COUNTERFACTUAL SIMULATION: ELIMINATION OF THE PENNY #
#===============================================================================
if (run_stylized == TRUE) {
  stylized_exercise_1_counter <- calc_optimal_tokens(
    amounts_list = as.list(seq(4.05, 5, 0.05)),
    using_tokens = c(0.05, 0.10, 0.25, 0.50,
                     1, 5, 10, 20, 50, 100),
    only_20note = FALSE
  )
  
}

# Re-solving the model under the different rounding policies
symmetric_pol_df <- counterfactual_sim(rounding_policy = "symmetric")
asymmetric_pol_df <- counterfactual_sim(rounding_policy = "asymmetric")

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
      
      lt100_calc <-
        subset(using_data, alpha >= 0.01 & alpha <= 100) %>%
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

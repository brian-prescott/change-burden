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
# optimal readibility. The window is approximately 80 lines long. This script
# reflects a major revision from the original draft.

# IMPORTANT NOTE: THE VARIABLE UASID IN THE DATASETS IS NOT THE SAME AS THE ONE
# PROVIDED UAS. RATHER, IT IS THE FRBA ID VARIABLE FROM THE PUBLIC DATASETS THAT
# HAS BEEN RENAMED FOR THE SAKE OF WORKING WITH THE SCRIPT.

# Cleaning out the environment before running the script
rm(list = ls())
initial_run <- FALSE
run_stylized <- TRUE
last_base_run <- "2021-09-07"
last_counter_run <- "2021-09-07"

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
library(fixest)

#===============================================================================
# PARAMETERS #
#===============================================================================
# insert the path to your datasets here
# General directory for more robust exporting
directory <- stringr::str_remove(Sys.getenv("PWD"), "analysis-code")
# Insert the path to the dataset here
setwd(paste0(directory, "data/"))

set.seed(1995)

#===============================================================================
                                  # DATA IMPORT #
#===============================================================================
load(
  file = "change-burden-dfs_2021-09-10.RData"
)


# This is equivalent to the cash_transactions.rds in the "data" folder.
tran_df <- change_burden_dfs$cash_transactions %>%
  dplyr::rename(uasid = id)

# The all transactions dataset is the in-person transactions.
in_person_df <- change_burden_dfs$all_transactions %>%
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
    baseline_path <- str_c(
      "baseline-results_", last_base_run, ".RData"
    )
    load(baseline_path)

    counter_path <- str_c(
      "counterfactual-results_", last_counter_run, ".RData"
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
    bind_cols(
      x = .,
      y = optimal_tokens_df %>%
        dplyr::select(n_tokens)
    ) %>%
    inner_join(
      x = .,
      y = counterfactual_optimal_tokens_df %>%
        bind_cols(., bind_rows(tran_df, tran_df)) %>%
        dplyr::rename(alpha = amnt) %>%
        dplyr::select(
          uasid, date, diary_day, tran, alpha, only_20note
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
# Here we are going to estimate the price effects of the rounding policies
# on consumers. We aggregate the transactions to the monthly level so that
# the results are more interpretable. We do this for both the cash only
# transactions and all in-person transactions. This is done to reflect the
# fact that merchants do not steer consumers in-person and so we should not
# reflect differential pricing in the face of penny elimination.
price_effects_dfs <- list(tran_df, in_person_df) %>%
  as.list() %>%
  purrr::map(
    .x = .,
    .f = function(using_df) {
      matching_df <- using_df %>%
        mutate(new_id = str_c(uasid, year, sep = "-")) %>%
        dplyr::select(new_id) %>%
        unlist() %>%
        unique() %>%
        as.list() %>%
        purrr::map(
          .x = .,
          .f = function(id) {
            output <- data.frame(
              uasid = str_sub(id, 1, 6),
              year = as.numeric(str_sub(id, 8, 11))
            ) %>%
            full_join(
              x = .,
              y = data.frame(
                year = rep(as.numeric(str_sub(id, 8, 11)), 3),
                diary_day = 1:3
              ),
              by = "year"
            ) %>%
            as_tibble()

            return(output)
          }
        ) %>%
        bind_rows() %>%
        as_tibble() %>%
        mutate_at(
          .vars = vars(uasid),
          .funs = as.numeric
        )

      policies <- c("symmetric", "asymmetric")
      output_dfs <- policies %>%
        purrr::map(
          .x = .,
          .f = function(policy) {
            if (policy == "symmetric") {
              out <- using_df %>%
                dplyr::rename(alpha = amnt) %>%
                mutate(
                  alpha_c = symmetric_rounding(alpha),
                  Delta_alpha = alpha_c - alpha
                ) %>%
                mutate(year = as.numeric(as.character(year))) %>%
                dplyr::select(uasid, diary_day, year, contains("alpha"))

              est_df <- dplyr::full_join(
                x = out,
                y = matching_df,
                by = c("uasid", "diary_day", "year")
              ) %>%
              as_tibble() %>%
              left_join(
                x = .,
                y = using_df %>%
                mutate(
                    year = as.numeric(as.character(year)),
                    unbanked = as.numeric(banked != 1 & !is.na(banked))
                  ) %>%
                  dplyr::select(
                    income_, age_cohort_, unbanked, education_, work_employed,
                    married, hh_size, gender, contains("race"), age, 
                    hispaniclatino, year, region, region_year, uasid
                  ) %>%
                  group_by(uasid, year) %>%
                  unique() %>%
                  ungroup(),
                by = c("uasid", "year")
              )

            } else {
              out <- using_df %>%
                dplyr::rename(alpha = amnt)
              alpha_counter <- out$alpha %>%
                as.list() %>%
                purrr::map(.f = asymmetric_rounding) %>%
                unlist() %>%
                as.numeric()
              out$alpha_c <- alpha_counter

              est_df <- dplyr::full_join(
                x = out %>%
                  mutate(Delta_alpha = alpha_c - alpha) %>%
                  mutate(year = as.numeric(as.character(year))) %>%
                  dplyr::select(uasid, diary_day, year, contains("alpha")),
                y = matching_df,
                by = c("uasid", "diary_day", "year")
              ) %>%
              as_tibble() %>%
              left_join(
                x = .,
                y = using_df %>%
                  mutate(
                    year = as.numeric(as.character(year)),
                    unbanked = as.numeric(banked != 1 & !is.na(banked))
                  ) %>%
                  dplyr::select(
                    income_, age_cohort_, unbanked, education_, work_employed,
                    married, hh_size, gender, contains("race"), age,
                    hispaniclatino, year, region, region_year, uasid
                  ) %>%
                  group_by(uasid, year) %>%
                  unique() %>%
                  ungroup(),
                by = c("uasid", "year")
              )

            }

            return(est_df)
          }
        )
      names(output_dfs) <- policies

      return(output_dfs)
    }
  )
names(price_effects_dfs) <- c("cash_trans", "all_trans")

policy_rounding_calcs <- function(outcome) {
  using_delta <- ifelse(
    test = tolower(outcome) == "price",
    yes = "Delta_alpha",
    no = "Delta_burden"
  )

  effects_calcs <- 1:2 %>%
    as.list() %>%
    purrr::map(
      .x = .,
      .f = function(i) {
        final_out <- c("All coins and notes", "Only $20 note") %>%
          as.list() %>%
          purrr::map(
            .x = .,
            .f = function(j) {
              df <- price_effects_dfs[[i]] %>%
                dplyr::filter(only_20note == j)
              policy <- str_c(c("symmetric", "asymmetric"), "-policy")

              est_df <- df %>%
                mutate_at(
                  .vars = vars(contains("alpha"), contains("Delta")),
                  .funs = ~ ifelse(
                    test = is.na(.),
                    yes = 0,
                    no = .
                  )
                ) %>%
                mutate(counter = 1) %>%
                group_by(uasid, year, diary_day) %>%
                summarize_at(
                  .vars = vars(contains("alpha"), contains("delta"), counter),
                  .funs = ~ sum(x = ., na.rm = TRUE)
                ) %>%
                ungroup() %>%
                left_join(
                  x = .,
                  y = df %>%
                  dplyr::select(-c(
                    contains("delta"), contains("alpha"), contains("token"), a,
                    pi, merch, used_cash, amnt, tran, date, diary_day, payee,
                    payment, in_person, only_20note, highest_education,
                    income_hh
                  )) %>%
                  unique() %>%
                  dplyr::filter(!is.na(age) & !is.na(hhincome)),
                  by = c("uasid", "year")
                ) %>%
                mutate(
                  year = factor(
                    x = year,
                    levels = c(2015:2019)
                  ),
                  income_ = relevel(
                    income_, ref = "50-74k"
                  ),
                  education_ = relevel(
                    education_, ref = "Bachelor's degree"
                  ),
                  race_ = case_when(
                    race_asian == 1 ~ "Asian",
                    race_black == 1 ~ "Black",
                    race_other == 1 ~ "Other",
                    race_white == 1 ~ "White"
                  ),
                  race_ = factor(
                    x = race_,
                    levels = c("White", "Asian", "Black", "Other")
                  ),
                  unbanked = as.numeric(banked != 1 & !is.na(banked))
                ) %>%
                dplyr::rename(Delta = all_of(using_delta)) %>%
                mutate(Delta = -1 * Delta) # adjusting the ordering of Delta

              est <- fixest::feols(
                31 * Delta ~ income_ + age_cohort_ + unbanked +  education_ +
                work_employed + married + hh_size + gender + race_ + age +
                hispaniclatino | year + region + region_year,
                se = "cluster",
                cluster = "uasid",
                data = est_df
              )
              print(paste(j, c("symmetric", "asymmetric")[i], "results"))
              print(summary(est))

              return(est)
            }
          )
      names(final_out) <- c("all_notes", "only_20notes")

      return(final_out)
    }
  )
  names(effects_calcs) <- stringr::str_c(c("symm", "asymm"), "_results")

  return(effects_calcs)
}

#===============================================================================
price_effect_calcs <- function(dataset, policy) {
  df <- price_effects_dfs[[dataset]][[policy]]
  est_df <- df %>%
    mutate_at(
      .vars = vars(contains("alpha")),
      .funs = ~ ifelse(
        test = is.na(.),
        yes = 0,
        no = .
      )
    ) %>%
    mutate(counter = 1) %>%
    group_by(uasid, year, diary_day) %>%
    summarize_at(
      .vars = vars(contains("alpha"), counter),
      .funs = ~ sum(x = ., na.rm = TRUE)
    ) %>%
    ungroup() %>%
    left_join(
      x = .,
      y = df %>%
      dplyr::select(-c(contains("alpha"), diary_day)) %>%
      group_by(uasid, year) %>%
      unique() %>%
      ungroup() %>%
      dplyr::filter(!is.na(education_) & !is.na(income_)),
      by = c("uasid", "year")
    ) %>%
    mutate(
      year = factor(
        x = year,
        levels = c(2015:2019)
      ),
      income_ = relevel(
        income_, ref = "50-74k"
      ),
      education_ = relevel(
        education_, ref = "Bachelor's degree"
      ),
      race_ = case_when(
        race_asian == 1 ~ "Asian",
        race_black == 1 ~ "Black",
        race_other == 1 ~ "Other",
        race_white == 1 ~ "White"
      ),
      race_ = factor(
        x = race_,
        levels = c("White", "Asian", "Black", "Other")
      )
    ) %>%
    dplyr::rename(Delta = Delta_alpha)

  est <- fixest::feols(
    31 * Delta ~ unbanked + income_ + education_ + age_cohort_ +
    work_employed + married + hh_size + gender + race_ + age +
    hispaniclatino | year + region + region_year,
    se = "cluster",
    cluster = "uasid",
    data = est_df
  )
  print(paste(dataset, policy, "results"))
  print(summary(est))

  return(est)
}

#===============================================================================
        # COUNTERFACTUAL SIMULATION: ELIMINATION OF THE PENNY #
#===============================================================================
# If you have already done the initial solving then you can skip the first
# part of the conditional. Note, solving the model under both policies takes
# approximately 20 minutes with 6 cores.
if (initial_run == TRUE) {
  # Re-solving the model under the different rounding policies
  symmetric_pol_df <- counterfactual_sim(rounding_policy = "symmetric")
  asymmetric_pol_df <- counterfactual_sim(rounding_policy = "asymmetric")

  save(
    list = str_c(c("asymmetric", "symmetric"), "_pol_df"),
    file = str_c("policy-results_", Sys.Date(), ".RData")
  )

} else {
  load(
    file = str_c("policy-results_", last_counter_run, ".RData")
  )

}

price_effects <- str_c(c("cash", "all"), "_trans") %>%
  as.list() %>%
  purrr::map(
    .x = .,
    .f = function(df) {
      output <- c("symmetric", "asymmetric") %>%
        as.list() %>%
        purrr::map(
          .x = .,
          .f = price_effect_calcs,
          dataset = df
        )
      names(output) <- c("symm", "asymm")

      return(output)
    }
  )
names(price_effects) <- c("cash", "all")

# Exporting the results for LaTeX
etable(
  price_effects$cash$symm,
  price_effects$cash$asymm,
  price_effects$all$symm,
  price_effects$all$asymm,
  tex = FALSE
)
etable(
  price_effects$cash$symm,
  price_effects$cash$asymm,
  price_effects$all$symm,
  price_effects$all$asymm,
  tex = TRUE
)

burden_effects_calcs <- policy_rounding_calcs(outcome = "burden")
etable(
  burden_effects_calcs$symm_results$all_notes,
  burden_effects_calcs$symm_results$only_20notes,
  burden_effects_calcs$asymm_results$all_notes,
  burden_effects_calcs$asymm_results$only_20notes,
  tex = FALSE
)
etable(
  burden_effects_calcs$symm_results$all_notes,
  burden_effects_calcs$symm_results$only_20notes,
  burden_effects_calcs$asymm_results$all_notes,
  burden_effects_calcs$asymm_results$only_20notes,
  tex = TRUE
)
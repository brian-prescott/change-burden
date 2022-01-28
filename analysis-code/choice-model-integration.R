#==============================================================================#
# PREAMBLE #
#==============================================================================#
# Written by: Brian Prescott

# Since the differences on the burden between symmetric and asymmetric price
# rounding are small, we will assume that all prices are rounded symmetrically.
# This is done solely for computational simplicity.

# When estimating the counterfactual the choice model, we do it under two
# simulated economies. Under the first, we assume that the consumer and merchant
# can both use the minimum number of tokens necessary to transact. Under the
# second assumption, we assume that the consumer must use a $20 note if they
# choose to pay with cash. Thus, the consumer faces some cost by choosing cash.

# Cleaning out the enviornment before running the code
rm(list = ls())

# This is TRUE only if you want to run everything from scratch again. Else,
# set the last_run variable to the date of your last estimation.
first_run <- FALSE
if (first_run == FALSE) {
  last_run <- "2022-01-16"

}

counter_run <- FALSE
if (counter_run == FALSE) {
  last_counter_run <- "2022-01-27"

}

# Setting the working directory
setwd(paste0(
  "/home/bp/PrescottLibrary/economics/papers/academic-papers/change-burden/",
  "data/"
))

#==============================================================================#
# PACKAGES #
#==============================================================================#
# Loading in the packages that will be used for the analysis
library(tidyverse)
library(stringi)
library(haven)
library(mlogit)
library(gmnl)
library(lpSolve)
library(parallel)

#==============================================================================#
# SETUP #
#==============================================================================#
# Output figure parameters
output_height <- 7
output_width <- 12
output_res <- 400

#==============================================================================#
# FUNCTIONS #
#==============================================================================#
# This is the simple symmetric rounding policy. We chose to use this policy
# since our quantification exercise showed little difference in the expected
# burden between the symmetric and asymmetric policies.
symmetric_rounding <- function(amount) {
  counterfactual_amount <- round(amount / 0.05) * 0.05

  return(counterfactual_amount)
}

# Asymmetric rounding function. This and the previous rounding function could
# be placed into a separate file and then called into this one, however, I am
# not going to spend the time doing that just yet.
asymmetric_rounding <- function(amount) {
  rounded_down_lower <- as.character(1:2)
  rounded_down_upper <- as.character(6:7)
  rounded_down <- c(rounded_down_lower, rounded_down_upper)

  amnt_intervals <- c(
    seq(1, 1000000, 10),
    seq(2, 1000000, 10),
    seq(6, 1000000, 10),
    seq(7, 1000000, 10)
  )
  if_else_condition <- ifelse(
    test = str_sub(stri_reverse(amount), 1, 1) %in% rounded_down &
      !(amount %in% amnt_intervals) &
      str_sub(stri_reverse(amount), 1, 2) != "1." &
      str_sub(stri_reverse(amount), 1, 2) != "2." &
      str_sub(stri_reverse(amount), 1, 2) != "6." &
      str_sub(stri_reverse(amount), 1, 2) != "7.",
    yes = TRUE,
    no = FALSE
  )
 if (if_else_condition) {
    new_amount <- str_c(
        ifelse(
          test = str_sub(stri_reverse(amount), 1, 1) %in% rounded_down_lower,
          yes = "5",
          no = "8"
        ),
        str_sub(stri_reverse(amount), 2)
      ) %>%
      stri_reverse() %>%
      as.numeric()
    counter_amount <- round(new_amount / 0.05) * 0.05

  } else {
    counter_amount <- round(amount / 0.05) * 0.05

  }

  return(counter_amount)
}


# This function solves the model of optimal consumer-merchant interactions
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
expected_welfare <- function(fitted_model,
                             counterfactual,
                             assumption_burden,
                             round_policy,
                             rounding_df
                             ) {
  if (!(assumption_burden %in% c(1, 2))) {
    stop("The variable 'assumption_burden' must be 1 or 2.")

  }

  df <- fitted_model$mf %>%
    dplyr::mutate(
      pi = rep(c("card", "cash"), nrow(fitted_model$mf) / 2)
    ) %>%
    dplyr::select(method, pi, everything())
  theta <- fitted_model$coefficients

  # Setting the seed for replication purposes.
  set.seed(1995)

  # Generating the random shock each consumer receives
  eta <- rnorm(n = nrow(df) / 2, mean = 0, sd = 1)

  if (counterfactual == TRUE) {
    df$amnt <- rounding_df$amnt
    df$burden <- rounding_df$burden

  }

  welfare_df <- c("cash", "card") %>%
    as.list() %>%
    purrr::map(
      .x = .,
      .f = function(j) {
        output <- df %>%
          dplyr::filter(pi == j) %>%
          dplyr::mutate(choice_id = 1:n()) %>%
          dplyr::mutate(
            V_cost = cost*(
              theta["cost"] +
                theta["cost.age"]*age +
                theta["cost.income"]*income +
                theta["cost.education"]*education +
                theta["cost.cc_rewards"]*cc_rewards +
                theta["cost.ln_revolving_balance"]*ln_revolving_balance +
                theta["sd.cost"] * eta
            ),
            V_convenience = convenience*(
              theta["convenience"] + 
                theta["convenience.age"]*age +
                theta["convenience.income"]*income + 
                theta["convenience.education"]*education + 
                theta["convenience.cc_rewards"]*cc_rewards +
                theta["convenience.ln_revolving_balance"]*ln_revolving_balance +
                theta["sd.convenience"] * eta
            ),
            V_security = security*(
              theta["security"] +
                theta["security.age"]*age +
                theta["security.income"]*income +
                theta["security.education"]*education +
                theta["security.bill"]*bill +
                theta["sd.security"] * eta
            ),
            V_records = records*(
              theta["records"] +
                theta["records.age"]*age +
                theta["records.income"]*income +
                theta["records.education"]*education +
                theta["records.bill"]*bill +
                theta["sd.records"] * eta
            ),
            V_burden = burden*(
              theta["burden"] + 
                theta["burden.age"]*age +
                theta["burden.income"]*income + 
                theta["burden.education"]*education + 
                theta["burden.cc_rewards"]*cc_rewards +
                theta["burden.ln_revolving_balance"]*ln_revolving_balance +
                theta["sd.burden"] * eta
            )
          )

        if (j == "cash") {
          final_output <- output %>% 
            dplyr::mutate(
              V_cash = V_cost + V_convenience + V_security + V_records +
              V_burden + theta["cash:(intercept)"] + theta["cash:amnt"]*amnt
            ) %>%
            dplyr::select(-c(V_cost, V_convenience, V_security, V_records))
        } else {
          final_output <- output %>%
            dplyr::mutate(
              V_card = V_cost + V_convenience +
              V_security + V_records + V_burden
            ) %>%
            dplyr::select(choice_id, V_card)

        }

        return(final_output)
      }
    ) %>%
    purrr::reduce(
      .x = .,
      .f = inner_join,
      by = "choice_id"
    ) %>%
    dplyr::arrange(choice_id) %>%
    dplyr::select(V_cash, V_card, everything()) %>%
    dplyr::mutate(
      W = log(exp(V_cash) + exp(V_card))
    ) %>%
    tibble::as_tibble() %>%
    dplyr::select(W, V_cash, V_card, everything()) %>%
    dplyr::mutate(
      pi = ifelse(
        method == TRUE,
        yes = "cash",
        no = "card"
      )
    ) %>%
    dplyr::select(-method)

  if (counterfactual == TRUE) {
    welfare_df <- welfare_df %>%
      dplyr::select(choice_id, W, V_cash, V_card) %>%
      dplyr::rename_at(
        .vars = vars(W, V_cash, V_card),
        .funs = ~ stringr::str_c(., "_c")
      )

  }

  return(welfare_df)
}

#==============================================================================#
# DATA PREP #
#==============================================================================#
# We have to estimate the baseline data twice. The first time we will assume
# the lower bound burden optimization problem and the second time we will
# consider the upper bound case.
analysis_df <- read_rds(file = "demand_model_df_2021-12-23.rds") %>%
  dplyr::mutate(
    rewards_nonRevolver = as.numeric(cc_rewards == 1) * ln_revolving_balance
  )

# Market share of each payment method
analysis_df$method %>%
  table() %>%
  prop.table()

if (first_run == TRUE) {
  model_df_list <- 1:2 %>%
  as.list() %>%
  purrr::map(
    .x = .,
    .f = function(k) {
    print(k)
    burden_assumption <- k
    # This takes quite a while to run. It could be sped up by using parallel
    # methods
    realized_burden <- seq_len(nrow(analysis_df)) %>%
      as.list() %>%
      parallel::mclapply(
        FUN = function(i) {
          # We are assuming that the consumer has no coins on their person.
          theta_p <- c(0.01, 0.05, 0.10, 0.50, 1, 5, 10, 20, 50, 100)
          using_amnt <- as.numeric(analysis_df[i, "amnt"])

          # Solving the optimal consumer-merchant transaction model for each
          # observed transaction amount.
          if (burden_assumption == 1) {
            b_star <- calc_optimal_tokens(
              amounts_list = as.list(using_amnt),
              using_tokens = theta_p,
              only_20note = FALSE
            )

          } else {
            b_star <- calc_optimal_tokens(
              amounts_list = as.list(using_amnt),
              using_tokens = theta_p,
              only_20note = TRUE
            )

          }

          output <- b_star %>%
            dplyr::rename(burden_cash = n_tokens) %>%
            dplyr::select(-a)

          return(output)
        },
        mc.cores = 6,
        mc.preschedule = TRUE,
        mc.cleanup = TRUE
      ) %>%
      dplyr::bind_rows()
    analysis_df$burden_cash <- realized_burden$burden_cash
    analysis_df$burden_card <- 0

    analysis_df$choiceid <- seq_len(nrow(analysis_df))
    final_analysis_df <- analysis_df %>%
      dplyr::select(
        dplyr::ends_with("_cash"), dplyr::ends_with("_card"), everything()
      )
    varying_start <- which(colnames(final_analysis_df) == "cost_cash")
    varying_end <- which(colnames(final_analysis_df) == "burden_card")

    # Specifying the mixed logit data.frame for the model estimation
    model_data <- mlogit::mlogit.data(
      data = final_analysis_df,
      choice = "method",
      shape = "wide",
      sep = "_",
      varying = varying_start:varying_end
    )

      return(model_data)
    }
  )
  names(model_df_list) <- c("lower", "upper")

}

#==============================================================================#
# CHOICE MODEL ESTIMATION #
#==============================================================================#
if (first_run == FALSE) {
  load(file = "lower-bnd-demand-model-results_2022-01-14.RData")
  lower_bnd_model <- model_estimation

  load(file = "upper-bnd-demand-model-results_2022-01-14.RData")
  upper_bnd_model <- model_estimation

  model_estimates <- list(lower_bnd_model, upper_bnd_model)

} else {
  # Estimating the mixed logit model
  observed_heterogeneity <- c(
    "age", "income", "education", "ln_revolving_balance", "cc_rewards",
    "bill"
  )
  model_heterogeneity <- paste(
    "age", "income", "education", "ln_revolving_balance", "cc_rewards",
    "bill",
    sep = " + "
  )

  model <- formula(
    paste(
      "method ~ cost + convenience + security + records + burden | 1",
      "amnt | 0 | 1", model_heterogeneity,
      sep = " + "
    )
  )

  # We're estimating the mixed logit models jointly here
  model_estimates <- c("lower", "upper") %>%
    as.list() %>%
    parallel::mclapply(
      FUN = function(model_type) {
        model_estimation <- gmnl::gmnl(
          formula = model,
          data = model_df_list[[str_c(model_type)]],
          model = "mixl",
          ranp = c(
            cost = "n",
            convenience = "n",
            security = "n",
            records = "n",
            burden = "n"
          ),
          mvar = list(
            cost = c(
              "age", "income", "education", "ln_revolving_balance", "cc_rewards"
            ),
            convenience = c(
              "age", "income", "education", "ln_revolving_balance", "cc_rewards"
            ),
            security = c(
              "age", "income", "education", "bill"
            ),
            records = c(
              "age", "income", "education", "bill"
            ),
            burden = c(
              "age", "income", "education", "ln_revolving_balance", "cc_rewards"
            )
          ),
          R = 500,
          haltons = NA
        )

        # Saving the results of the model
        save(
          list = c("model_estimation"),
          file = str_c(model_type, "-bnd-demand-model-results_2022-01-14.RData")
        )

        return(model_estimation)
      },
      mc.cores = 6,
      mc.preschedule = TRUE,
      mc.cleanup = TRUE
    )

}

#==============================================================================#
# COUNTERFACTUAL WELFARE ANALYSIS #
#==============================================================================#
# Run this code if it is not your first run
if (counter_run == FALSE) {
  load(
    file = stringr::str_c(
      "counterfactual_simulations_", last_counter_run, ".RData"
    )
  )

} else {
  welfare_baseline_lower <- expected_welfare(
    fitted_model = model_estimates[[1]],
    counterfactual = FALSE,
    assumption_burden = 1
  )
  welfare_baseline_upper <- expected_welfare(
    fitted_model = model_estimates[[2]],
    counterfactual = FALSE,
    assumption_burden = 2
  )

  rounding_df_list <- c(symmetric_rounding, asymmetric_rounding) %>%
    as.list() %>%
    purrr::map(
      .x = .,
      .f = function(price_rounding) {
        if (price_rounding(1.06) == symmetric_rounding(1.06)) {
          df <- model_estimates[[1]]$mf

        } else {
          df <- model_estimates[[2]]$mf

        }

        # The code is giving a warning when I perform this directly onto the
        # column vector. Therefore, I am using a paralled lapply functional
        # to speed this up.
        new_amnts <- df$amnt %>%
          as.list() %>%
          parallel::mclapply(
            FUN = function(a) {
              output <- data.frame(amnt_c = price_rounding(amount = a))

              return(output)
            },
            mc.cores = 6,
            mc.preschedule = TRUE,
            mc.cleanup = TRUE
          ) %>%
          dplyr::bind_rows() %>%
          unlist() %>%
          as.numeric()
        output <- tibble::tibble(amnt = new_amnts)

        # Now we need to adjust the expected burden
        realized_burden <- 1:2 %>%
          as.list() %>%
          purrr::map(
            .x = .,
            .f = function(assumption_burden) {
              inter_output <- seq_len(nrow(analysis_df)) %>%
                as.list() %>%
                parallel::mclapply(
                  FUN = function(i) {
                    # We are assuming that the consumer has no coins on their 
                    # person.
                    theta_p <- c(0.05, 0.10, 0.50, 1, 5, 10, 20, 50, 100)
                    using_amnt <- price_rounding(
                      as.numeric(analysis_df[i, "amnt"])
                    )

                    # Solving the optimal consumer-merchant transaction model 
                    # for each observed transaction amount.
                    if (assumption_burden == 1) {
                      b_star <- calc_optimal_tokens(
                        amounts_list = as.list(using_amnt),
                        using_tokens = theta_p,
                        only_20note = FALSE
                      )

                    } else {
                      b_star <- calc_optimal_tokens(
                        amounts_list = as.list(using_amnt),
                        using_tokens = theta_p,
                        only_20note = TRUE
                      )

                    }

                    output <- b_star %>%
                      dplyr::rename(burden = n_tokens) %>%
                      dplyr::select(-a)

                    return(output)
                  },
                  mc.cores = 6,
                  mc.preschedule = TRUE,
                  mc.cleanup = TRUE
                ) %>%
                dplyr::bind_rows()

              return(inter_output)
            }
          )
        names(realized_burden) <- c("lower", "upper")

        final_output <- realized_burden %>%
          purrr::map(
            .x = .,
            .f = function(df) {
              out <- dplyr::bind_cols(
                x = dplyr::bind_rows(x = df, y = df),
                y = output
              ) %>%
              tibble::as_tibble()

              return(out)
            }
          )

        return(final_output)
      }
    )
  names(rounding_df_list) <- c("sym", "asym")

  counterfactual_simulations <- c(1, 2) %>%
    as.list() %>%
    purrr::map(
      .x = .,
      .f = function(i) {
        final_calcs <- c("sym", "asym") %>%
          as.list() %>%
          purrr::map(
            .x = .,
            .f = function(p) {
              welfare_counterfactual <- expected_welfare(
                fitted_model = model_estimates[[i]],
                counterfactual = TRUE,
                assumption_burden = i,
                round_policy = p,
                rounding_df = rounding_df_list[[p]][[c("lower", "upper")[i]]]
              ) %>%
                dplyr::inner_join(
                  x = .,
                  y = list(welfare_baseline_lower, welfare_baseline_upper)[[i]],
                  by = "choice_id"
                ) %>%
                dplyr::mutate(
                  policy = c("All coins and notes", "Only $20 notes")[i]
                )
              welfare_counterfactual <- welfare_counterfactual %>%
                dplyr::mutate(
                  Delta_W = ((W_c - W) / W) * 100,
                  round_policy = p
                )

              return(welfare_counterfactual)
            }
          )
          names(final_calcs) <- c("sym", "asym")

          return(final_calcs)
        }
      )
  names(counterfactual_simulations) <- c("lower", "upper")

  # Saving the counterfactual simulations
  save(
    list = c("counterfactual_simulations"),
    file = stringr::str_c("counterfactual_simulations_", Sys.Date(), ".RData")
  )

}

#==============================================================================#
# COUNTERFACTUAL FIGURES #
#==============================================================================#
counterfactual_figures_df <- counterfactual_simulations$lower$sym %>%
  dplyr::select(Delta_W) %>%
  dplyr::mutate(
    bound = "All coins and notes",
    policy = "Symmetric"
  ) %>%
  dplyr::bind_rows(
    x = .,
    y = counterfactual_simulations$lower$asym %>%
    dplyr::select(Delta_W) %>%
    dplyr::mutate(
      bound = "All coins and notes",
      policy = "Asymmetric"
    )
  ) %>%
  dplyr::bind_rows(
    x = .,
    y = counterfactual_simulations$upper$sym %>%
    dplyr::select(Delta_W) %>%
    dplyr::mutate(
      bound = "Only $20 notes",
      policy = "Symmetric"
    )
  ) %>%
  dplyr::bind_rows(
    x = .,
    y = counterfactual_simulations$upper$asym %>%
    dplyr::select(Delta_W) %>%
    dplyr::mutate(
      bound = "Only $20 notes",
      policy = "Asymmetric"
    )
  ) %>%
  dplyr::mutate(
    bound = factor(
      x = bound,
      levels = c("All coins and notes", "Only $20 notes")
    ),
    policy = factor(
      x = policy,
      levels = c("Symmetric", "Asymmetric")
    )
  )

counterfactual_figures_df %>%
  dplyr::group_by(bound, policy) %>%
  dplyr::mutate_at(
    .vars = dplyr::vars(Delta_W),
    .funs = ~ ifelse(
      test = . > quantile(., probs = 0.975) | . < quantile(., probs = 0.025),
      yes = NA,
      no = .
    )
  ) %>%
  na.omit() %>%
  ggplot(
    data = .,
    aes(x = Delta_W)
  ) +
  geom_histogram(
    aes(y = ..density..),
    fill = "seagreen4",
    color = "white",
    bins = 40
  ) +
  scale_y_continuous(
    name = "Density",
#    limits = c(0, 0.005),
    expand = c(0, 0)
  ) +
  xlab(expression(paste(Delta, "W"[i]))) +
  facet_wrap(~ policy + bound) +
  theme_bw() +
  theme(
    text = element_text(family = "serif"),
    axis.text = element_text(size = 14, color = "black"),
    strip.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 15, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
ggplot2::ggsave(
  filename = "counterfactual-welfare_2022-01-25.jpeg",
  device = "jpeg",
  plot = last_plot(),
  height = output_height,
  width = output_width
)

#==============================================================================#
income_counterfactual_figures_df <- counterfactual_simulations$lower$asym %>%
    dplyr::select(Delta_W, income) %>%
    dplyr::mutate(
      bound = "All coins and notes",
      policy = "Asymmetric"
    ) %>%
    dplyr::bind_rows(
      x = .,
      y = counterfactual_simulations$upper$asym %>%
      dplyr::select(Delta_W, income) %>%
      dplyr::mutate(
        bound = "Only $20 notes",
        policy = "Asymmetric"
      )
    ) %>%
      dplyr::mutate(
      income = exp(income) * 52,
      income_dist = case_when(
        income < 38000 ~ "Lowest quintile",
        income >= 38000 & income < 56000 ~ "Second quintile",
        income >= 56000 & income < 80000 ~ "Middle quintile",
        income >= 80000 & income < 120000 ~ "Fourth quintile",
        income > 120000 ~ "Highest quintile"
      ),
      income_dist = factor(
        x = income_dist,
        levels = str_c(
          c("Lowest", "Second", "Middle", "Fourth", "Highest"), " quintile"
        )
      ),
      bound = factor(
        x = bound,
        levels = c("All coins and notes", "Only $20 notes")
      ),
      policy = factor(
        x = policy,
        levels = c("Symmetric", "Asymmetric")
      )
    )

income_counterfactual_figures_df %>%
  dplyr::group_by(income_dist, bound) %>%
  dplyr::mutate_at(
    .vars = dplyr::vars(Delta_W),
    .funs = ~ ifelse(
      test = . > quantile(., probs = 0.975) | . < quantile(., probs = 0.025),
      yes = NA,
      no = .
    )
  ) %>%
  na.omit() %>%
#  dplyr::filter(Delta_W >= -750 & Delta_W <= 200) %>%
  ggplot(
    data = .,
    aes(x = Delta_W)
  ) +
  geom_histogram(
    aes(y = ..density..),
    fill = "seagreen4",
    color = "white",
    bins = 20
  ) +
  scale_y_continuous(
    name = "Density",
#    limits = c(0, 0.004),
    expand = c(0, 0)
  ) +
  xlab(expression(paste(Delta, "W"[i]))) +
  facet_wrap(~ bound + income_dist, ncol = 5) +
  theme_bw() +
  theme(
    text = element_text(family = "serif"),
    axis.text = element_text(size = 14, color = "black"),
    strip.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 15, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
ggplot2::ggsave(
  filename = "income-counterfactual-welfare_2022-01-25.jpeg",
  device = "jpeg",
  plot = last_plot(),
  height = output_height,
  width = output_width
)

#==============================================================================#
# COUNTERFACTUAL TABLES #
#==============================================================================#
welfare_changes_income_table <- c("lower", "upper") %>%
  as.list() %>%
  purrr::map(
    .x = .,
    .f = function(bound) {
      inter_output <- c("sym", "asym") %>%
      as.list() %>%
      purrr::map(
        .x = .,
        .f = function(policy) {
          i <- bound
          j <- policy
          table <- counterfactual_simulations[[i]][[j]] %>%                  
            dplyr::mutate(
            income = exp(income) * 52,
            income_dist = case_when(
              income < 38000 ~ "Lowest quintile",
              income >= 38000 & income < 56000 ~ "Second quintile",
              income >= 56000 & income < 80000 ~ "Middle quintile",
              income >= 80000 & income < 120000 ~ "Fourth quintile",
              income > 120000 ~ "Highest quintile"
            ),
            income_dist = factor(
              x = income_dist,
              levels = str_c(
                c("Lowest", "Second", "Middle", "Fourth", "Highest"),
                " quintile"
              )
            )
          ) %>%
          dplyr::select(income_dist, Delta_W) %>%
          dplyr::group_by(income_dist) %>%
          dplyr::mutate_at(
            .vars = dplyr::vars(Delta_W),
            .funs = ~ ifelse(
              test = . > quantile(., probs = 0.975) | 
              . < quantile(., probs = 0.025),
              yes = NA,
              no = .
            )
          ) %>%
          na.omit() %>%
          dplyr::summarize(
            mean = mean(Delta_W, na.rm = TRUE),
            p1 = quantile(x = Delta_W, probs = 0.01, na.rm = TRUE),
            p5 = quantile(x = Delta_W, probs = 0.05, na.rm = TRUE),
            p25 = quantile(x = Delta_W, probs = 0.25, na.rm = TRUE),
            median = median(Delta_W, na.rm = TRUE),
            p75 = quantile(x = Delta_W, probs = 0.75, na.rm = TRUE),
            p95 = quantile(x = Delta_W, probs = 0.95, na.rm = TRUE),
            p99 = quantile(x = Delta_W, probs = 0.99, na.rm = TRUE),
            N = n()
          )

        return(table)
        }
      )
    names(inter_output) <- c("symmetric", "asymmetric")
    print(inter_output)

    return(inter_output)
    }
  )
names(welfare_changes_income_table) <- c("lower", "upper")

# These are the tables we will put into the LaTeX document.
welfare_changes_income_table$lower %>%
  dplyr::bind_rows() %>%
  xtable::xtable() %>%
  print(include.rownames = FALSE)

#==============================================================================#
welfare_changes_table <- c("lower", "upper") %>%
  as.list() %>%
  purrr::map(
    .x = .,
    .f = function(bound) {
      inter_output <- c("sym", "asym") %>%
      as.list() %>%
      purrr::map(
        .x = .,
        .f = function(policy) {
          i <- bound
          j <- policy
          table <- counterfactual_simulations[[i]][[j]] %>%
          dplyr::select(Delta_W) %>%
          dplyr::mutate_at(
            .vars = dplyr::vars(Delta_W),
            .funs = ~ ifelse(
              test = . > quantile(., probs = 0.975) |
              . < quantile(., probs = 0.025),
              yes = NA,
              no = .
            )
          ) %>%
          na.omit() %>%
          dplyr::summarize(
            mean = mean(Delta_W, na.rm = TRUE),
            p10 = quantile(x = Delta_W, probs = 0.10, na.rm = TRUE),
#            p5 = quantile(x = Delta_W, probs = 0.05, na.rm = TRUE),
            p25 = quantile(x = Delta_W, probs = 0.25, na.rm = TRUE),
            median = median(Delta_W, na.rm = TRUE),
            p75 = quantile(x = Delta_W, probs = 0.75, na.rm = TRUE),
            p90 = quantile(x = Delta_W, probs = 0.90, na.rm = TRUE),
#            p99 = quantile(x = Delta_W, probs = 0.99, na.rm = TRUE),
            N = n()
          )
        return(table)

        }
      )
    names(inter_output) <- c("symmetric", "asymmetric")
    print(inter_output)

    return(inter_output)
    }
  )
names(welfare_changes_table) <- c("lower", "upper")

welfare_changes_table$lower %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(policy = c("symmetric", "asymmetric")) %>%
  dplyr::select(policy, everything())

welfare_changes_table$upper %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(policy = c("symmetric", "asymmetric")) %>%
  dplyr::select(policy, everything())

#=================================================================================================================
                                                  # PREAMBLE #
#=================================================================================================================

# Written by: Brian Prescott

# Purpose: This script generates all of the figures and tables, solves the optimal currency denomination problem
# and conducts the counterfactual policy simulation for the paper "The Burden of Change and the Penny Policy 
# Debare" by Brian Prescott and Oz Shy. 

# Notes: If this is your initial use of the code (i.e. if initial_run == TRUE), then be warned that the 
# optimization routines take approximately 10 hours combined to run. Outside of that, the window containing the 
# script should be extended to the edge of the dashed lines for optimal readibility. It should be noted that I the
# uasid variable reported is not actually the "uasid" variable provided by USC but rather is the "id" variable 
# created by the Atlanta Fed. The variable was simply renamed for the purposes of the script.

#=================================================================================================================
                                                  # LIBRARIES #
#=================================================================================================================

library(tidyverse)
library(haven)
library(lpSolve)
library(stargazer)
library(EnvStats)
library(data.table)
library(grDevices)
library(graphics)
library(xtable)

#=================================================================================================================
                                                  # PARAMETERS #
#=================================================================================================================

directory <- ... # insert the path to your datasets here 
setwd(directory)

set.seed(10)

initial_run <- FALSE

last_run <- "2021-03-22"

output_height <- 7
output_width <- 12
output_res <- 400
  
#=================================================================================================================
                                                  # FUNCTIONS #
#=================================================================================================================

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#=================================================================================================================

# These are just wrapper functions
p25 <- function(X, na.rm) return(quantile(x = X, probs = 0.25, na.rm = na.rm))

p75 <- function(X, na.rm) return(quantile(x = X, probs = 0.75, na.rm = na.rm))

p95 <- function(X, na.rm) return(quantile(x = X, probs = 0.95, na.rm = na.rm))

#=================================================================================================================

calc_optimal_tokens <- function(amounts_list, using_tokens) {
  
  compute_optimal_tokens <- function(a) {
    
    amount <<- a
    
    tokens <- using_tokens
    
    n_tokens <- length(tokens)
    
    f.obj <- rep(1, n_tokens * 2)
  
    f.constraint <- c(rep(1, n_tokens * 2), 
                      tokens, rep(0, n_tokens),
                      tokens, -tokens, 
                      diag(n_tokens * 2)) %>%
            matrix(data = ., 
               nrow = 3 + (n_tokens * 2), 
               byrow = TRUE)
  
    f.direction <- c(">=", ">=", "==", rep(">=", n_tokens * 2))
    f.rhs <- c(1, amount, amount, rep(0, n_tokens * 2))
  
    solve_optimal_tokens <- lp("min",
                               f.obj,
                               f.constraint,
                               f.direction,
                               f.rhs,
                               int.vec = 1:length(f.obj),
                               all.bin = FALSE)
    
    optimal_number_tokens <- round(solve_optimal_tokens$objval, 0)
    
    optimal_tokens_df <- optimal_number_tokens %>%
      data.frame(n_tokens = ., 
                 a = amount) %>%
      as_tibble()

    # Adding in a progress bar to keep track of the optimization
    progress_bar$tick()$print()
    
    return(optimal_tokens_df)
    
  } 
    
  # Adding this line in for the progress bar
  suppressWarnings(progress_bar <- progress_estimated(length(amounts_list))) 
  
  optimal_tokens_calculations <- amounts_list %>%
    purrr::map(.f = compute_optimal_tokens) %>%
    bind_rows()

  return(optimal_tokens_calculations)
  
}

#=================================================================================================================

penny_elim_rounding <- function(amount) {
  
  counterfactual_amount <- round(amount / 0.05) * 0.05
  
  return(counterfactual_amount)
  
}

#=================================================================================================================

factor2numeric <- function(x) return(as.numeric(as.character(x))) 

#=================================================================================================================
                                                  # DATA IMPORT #
#=================================================================================================================

tran_df <- read_rds("cash_transactions_df.rds") %>%
  dplyr::rename(uasid = id)

#=================================================================================================================
                                                # DESCRIPTIVE ANALYSIS #
#=================================================================================================================

# Number of consumers
tran_df$uasid %>%
  unique() %>%
  length()

# Number of transactions
nrow(tran_df)

# Merchant type summary statistics
merch_sumStats <- list(mean, sd, median, min, max) %>%
  purrr::map(.x = ., 
             .f = function(h) {
               
               sumS_df <- tran_df %>%
                 mutate(payee = factor(payee, 
                        levels = c(1:8), 
                        labels = c("Financial services provider", 
                                   "Education provider", 
                                   "Hospital, doctor or dentist", 
                                   "Government", "Nonprofit, charity or religion", 
                                   "A person", 
                                   "Retail store or online retailer", 
                                   "Business that primarily sells services")))

               setDT(sumS_df)[, c(levels(sumS_df$payee), "payee") := 
                                c(lapply(levels(payee), function(x) as.integer(x == payee)), .(NULL))]

               table <- sumS_df[,(ncol(sumS_df) - 7):ncol(sumS_df)] %>%
                 summarise_all(.tbl = ., 
                               .funs = ~ h(x = ., na.rm = TRUE)) %>%
                 round(2) %>%
                 t() %>%
                 data.frame() %>%
                 rownames_to_column(.data = ., 
                                    var = "Covariate") %>%
                 arrange(Covariate) %>%
                 dplyr::rename(h = ".")
             }) %>%
  purrr::reduce(.x = ., 
                .f = inner_join, 
                by = "Covariate")

# Summary statistics generation
sumStat_table <- list(mean, sd, median, min, max) %>%
  purrr::map(.x = ., 
             .f = function(h) {
               
               sumS_df <- tran_df 
               
               setDT(sumS_df)[, c(levels(sumS_df$income_), "income_") := 
                                c(lapply(levels(income_), function(x) as.integer(x == income_)), .(NULL))]
               
               setDT(sumS_df)[, c(levels(sumS_df$age_cohort_), "age_cohort_") := 
                                c(lapply(levels(age_cohort_), function(x) as.integer(x == age_cohort_)), .(NULL))]
               
               setDT(sumS_df)[, c(levels(sumS_df$education_), "education_") := 
                                c(lapply(levels(education_), function(x) as.integer(x == education_)), .(NULL))]

               table <- sumS_df %>%
                 dplyr::select(-c(ends_with(".y"), hispaniclatino_group, birthYear, payment, income_hh, 
                                  year, date, tran, diary_day, pi, merch, highest_education, payee,
                                  marital_status, used_cash, banked, in_person, merge_year, uasid)) %>%
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
               
             }) %>%
  purrr::reduce(.x = ., 
                .f = inner_join, 
                by = "Covariate") %>%
  dplyr::rename(mean = h.x, 
                sd = h.y, 
                median = h) 
sumStat_table %>%
  bind_rows(x = ., 
            y = merch_sumStats %>%
              dplyr::rename(mean = h.x, 
                            sd = h.y, 
                            median = h)) %>%
  mutate_at(.vars = vars(-Covariate), 
            .funs = ~ round(x = ., digits = 2)) %>%
  dplyr::rename(max = median) %>%
  dplyr::rename(median = h.x.x, 
                min = h.y.y) %>%
  xtable::xtable() %>%
  print(include.rownames = FALSE)

#=================================================================================================================
                                           # MODEL SIMULATION #
#=================================================================================================================

# Here we run the simple exercise of evaluating the burden at each dollar value between [0.01, 1]
stylized_exercise_1 <- calc_optimal_tokens(amounts_list = as.list(seq(0.01, 1, 0.01)),
                                           using_tokens = c(0.01, 0.05, 0.10, 0.25, 0.50,
                                                            1, 2, 5, 10, 20, 50, 100))

# Solving the model if we haven't already solved it
if (initial_run == TRUE) {
  
  amnt_list <- as.list(tran_df$amnt)
  
  optimal_tokens_w_2note <- calc_optimal_tokens(amounts_list = amnt_list,
                                                using_tokens = c(0.01, 0.05, 0.10, 0.25, 0.50,
                                                                 1, 2, 5, 10, 20, 50, 100)) 
  
  optimal_tokens_wo_2note <- calc_optimal_tokens(amounts_list = amnt_list,
                                                 using_tokens = c(0.01, 0.05, 0.10, 0.25, 0.50,
                                                                  1, 5, 10, 20, 50, 100))

  optimal_tokens_df <- optimal_tokens_w_2note %>%
    mutate(has_2note = "All coins and notes") %>%
    bind_rows(., 
              optimal_tokens_wo_2note %>%
                mutate(has_2note = "No $2 note"))
  
  write_rds(optimal_tokens_df,
            file = str_c("optimal_denom_model_solutions_", Sys.Date(), ".rds"))
  
} else {
  
  optimal_tokens_df <- read_rds(str_c("optimal_denom_model_solutions_", last_run, ".rds"))
  
}

#=================================================================================================================
                            # COUNTERFACTUAL SIMULATION: ELIMINATION OF THE PENNY #
#=================================================================================================================

stylized_exercise_1.counter <- calc_optimal_tokens(amounts_list = as.list(seq(0.05, 1, 0.05)),
                                                   using_tokens = c(0.05, 0.10, 0.25, 0.50,
                                                                    1, 2, 5, 10, 20, 50, 100))

counterfactual_tran_df <- tran_df %>%
  mutate_at(.vars = vars(amnt),
            .funs = penny_elim_rounding) 

if (initial_run == TRUE) {

  counterfactual_amnt_list <- as.list(counterfactual_tran_df$amnt)
  
  counterfactual_optimal_tokens_w_2note <- calc_optimal_tokens(amounts_list = counterfactual_amnt_list,
                                                               using_tokens = c(0.05, 0.10, 0.25, 0.50,
                                                                                1, 2, 5, 10, 20, 50, 100))
  
  counterfactual_optimal_tokens_wo_2note <- calc_optimal_tokens(amounts_list = counterfactual_amnt_list,
                                                                using_tokens = c(0.05, 0.10, 0.25, 0.50,
                                                                                 1, 5, 10, 20, 50, 100))
  
  counterfactual_optimal_tokens_df <- counterfactual_optimal_tokens_w_2note %>%
    mutate(has_2note = "All coins and notes") %>%
    bind_rows(., 
              counterfactual_optimal_tokens_wo_2note %>%
                mutate(has_2note = "No $2 note"))

  write_rds(counterfactual_optimal_tokens_df,
            file = str_c("counter_optimal_denom_model_solutions_", Sys.Date(), ".rds"))
  
} else {
  
  counter_path <- str_c("counter_optimal_denom_model_solutions_", last_run, ".rds")
  counterfactual_optimal_tokens_df <- read_rds(counter_path)
  
}

postCounter_df <- counterfactual_optimal_tokens_df %>%
  bind_cols(., bind_rows(counterfactual_tran_df, counterfactual_tran_df)) %>%
  mutate(alpha_tilde = a) %>%
  dplyr::rename(n_tokens_counter = n_tokens) %>% 
  inner_join(x = ., 
             y = optimal_tokens_df %>%
               bind_cols(., bind_rows(tran_df, tran_df)) %>%
               dplyr::rename(alpha = amnt) %>%
               dplyr::select(uasid, date, diary_day, tran, alpha, has_2note, n_tokens), 
             by = c("uasid", "date", "diary_day", "tran", "has_2note")) %>%
  mutate(Delta_alpha = round(alpha_tilde, 2) - round(alpha, digits = 2),
         delta_penny = abs(Delta_alpha),
         Delta_burden = round(n_tokens_counter, 0) - round(n_tokens, digits = 0), 
         payee = factor(payee, 
                        levels = c(1:8), 
                        labels = c("Financial services provider", 
                                   "Education provider", 
                                   "Hospital, doctor or dentist", 
                                   "Government", "Nonprofit, charity or religion", 
                                   "A person", 
                                   "Retail store or online retailer", 
                                   "Business that primarily sells services")))

#=================================================================================================================
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

#=================================================================================================================

c(mean, sd, median, p25, p75, p95, min, max) %>%
  as.list() %>%
  purrr::map(.f = function(h) {
    
    output <- h(postCounter_df$n_tokens, na.rm = TRUE)
    
  })

#=================================================================================================================

c(mean, sd, median, p25, p75, p95, min, max) %>%
  as.list() %>%
  purrr::map(.f = function(h) {
    
    output <- h(subset(postCounter_df, alpha >= 0.01 & alpha <= 100)$n_tokens, na.rm = TRUE)
    
  })

c(mean, sd, median, p25, p75, p95, min, max) %>%
  as.list() %>%
  purrr::map(.f = function(h) {
    
    output <- h(subset(postCounter_df, alpha  > 100)$n_tokens, na.rm = TRUE)
    
  })

#=================================================================================================================

c(mean, sd, median, p25, p75, p95, min, max) %>%
  as.list() %>%
  purrr::map(.f = function(h) {
    
    output <- h(postCounter_df$n_tokens_counter, na.rm = TRUE)
    
  })

#=================================================================================================================

c(mean, sd, median, p25, p75, p95, min, max) %>%
  as.list() %>%
  purrr::map(.f = function(h) {
    
    output <- h(subset(postCounter_df, alpha >= 0.01 & alpha <= 100)$n_tokens, na.rm = TRUE)
    
  })

c(mean, sd, median, p25, p75, p95, min, max) %>%
  as.list() %>%
  purrr::map(.f = function(h) {
    
    output <- h(subset(postCounter_df, alpha  > 100)$n_tokens_counter, na.rm = TRUE)
    
  })


#=================================================================================================================
                                              # FIGURES #
#=================================================================================================================

cash_dist <- tran_df %>%
  subset(amnt <= 100) %>%
  ggplot(data = ., aes(x = amnt)) +
  geom_histogram(aes(y = ..density..),
                 color = "white",
                 bins = 105,
                 bindwidth = 1,
                 # bins = 75,
                 fill = "royalblue4") +
  scale_y_continuous("Density",
                     limits = c(0, 0.101),
                     breaks = seq(0, 1, .02),
                     expand = c(0, 0)) +
  scale_x_continuous("Amount ($)", 
                     limits = c(0, 101),
                     breaks = seq(0, 100, 5), 
                     expand = c(0, 0)) +
  theme_bw() + 
  theme(text = element_text(family = "serif"),
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank())
print(cash_dist)
# ggsave(filename = "cash_dist_2015-2019.jpeg", 
#        device = "jpeg",
#        plot = last_plot(), 
#        height = output_height,
#        width = output_width)

#=================================================================================================================

max_shares_fig1 <- postCounter_df %>%
  subset(alpha <= 100 & has_2note == "All coins and notes") %>%
  dplyr::select(n_tokens) %>%
  table() %>%
  prop.table() %>%
  data.frame() %>%
  dplyr::rename(n_tokens = ".",
                share = "Freq") %>%
  mutate(share = share * 100,
         n_tokens = as.numeric(as.character(n_tokens)), 
         which_notes = "All coins and notes") %>%
  dplyr::select(n_tokens) %>%
  unlist() %>%
  factor2numeric() %>%
  max()

max_shares_fig2 <- postCounter_df %>%
  subset(alpha <= 100 & has_2note != "All coins and notes") %>%
  dplyr::select(n_tokens) %>%
  table() %>%
  prop.table() %>%
  data.frame() %>%
  dplyr::rename(n_tokens = ".",
                share = "Freq") %>%
  mutate(share = share * 100,
         n_tokens = as.numeric(as.character(n_tokens)), 
         which_notes = "All coins and notes") %>%
  dplyr::select(n_tokens) %>%
  unlist() %>%
  factor2numeric() %>%
  max()

shares_figure_1 <- postCounter_df %>%
  subset(has_2note == "All coins and notes") %>%
  dplyr::select(n_tokens) %>%
  table() %>%
  prop.table() %>%
  data.frame() %>%
  dplyr::rename(n_tokens = ".",
                share = "Freq") %>%
  mutate(share = share * 100,
         n_tokens = as.numeric(as.character(n_tokens)), 
         which_notes = "All coins and notes") %>%
  subset(n_tokens <= max_shares_fig1) %>%
  ggplot(data = ., aes(x = n_tokens, y = share)) + 
  geom_histogram(stat = "identity", 
                 fill = "royalblue4", 
                 width = 0.3,
                 color = "white") +
  scale_x_continuous(limits = c(0.75, 9.5),
                     breaks = seq(1, 10, 1)) +
  scale_y_continuous("Share of cash transactions",
                     limits = c(0, 31), 
                     breaks = seq(0, 35, 5),
                     expand = c(0, 0)) +
  geom_text(aes(label = round(share, 1)), 
            nudge_y = 0.5,
            color = "black",
            size = 5,
            family = "serif") + 
  xlab(expression(italic(b^"*"))) +
  ggtitle("All coins and notes") +
  theme_bw() + 
  theme(text = element_text(family = "serif"),
        plot.title = element_text(size = 15, hjust = 0.50),
        axis.text = element_text(size = 14, color = "black"),
        strip.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank())

shares_figure_2 <- postCounter_df %>%
  subset(has_2note != "All coins and notes") %>%
  dplyr::select(n_tokens) %>%
  table() %>%
  prop.table() %>%
  data.frame() %>%
  dplyr::rename(n_tokens = ".",
                share = "Freq") %>%
  mutate(share = share * 100,
         n_tokens = as.numeric(as.character(n_tokens)), 
         which_notes = "No $2 notes") %>%
  subset(n_tokens <= max_shares_fig2) %>%
  ggplot(data = ., aes(x = n_tokens, y = share)) + 
  geom_histogram(stat = "identity", 
                 fill = "royalblue4", 
                 width = 0.3,
                 color = "white") +
  scale_x_continuous(limits = c(0.75, 9.5),
                     breaks = seq(1, 100, 1)) +
  scale_y_continuous("Share of cash transactions",
                     limits = c(0, 31), 
                     breaks = seq(0, 35, 5),
                     expand = c(0, 0)) +
  geom_text(aes(label = round(share, 1)), 
            nudge_y = 0.5,
            color = "black",
            size = 5,
            family = "serif") + 
  xlab(expression(italic(b^"*"))) +
  ggtitle("No $2 notes") + 
  theme_bw() + 
  theme(text = element_text(family = "serif"),
        plot.title = element_text(size = 15, hjust = 0.50),
        axis.text = element_text(size = 14, color = "black"),
        strip.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank())

multiplot(shares_figure_1, shares_figure_2, cols = 2)
# dev.copy(jpeg,
#          "optimal_tokens_share_2021-04-07.jpeg",
#          height = output_height,
#          width = output_width,
#          units = "in", 
#          res = output_res)
# dev.off()

#=================================================================================================================

stylizedExercise_figure_1 <- stylized_exercise_1 %>%
  ggplot(data = ., aes(x = a, y = n_tokens)) +
  geom_line(size = 1,
            lty = 3,
            color = "midnightblue") +
  geom_point(size = 2,
             color = "midnightblue") +
  scale_x_continuous(limits = c(0, 1),
                     breaks = c(0.01, seq(0.05, 1, 0.05))) +
  scale_y_continuous(limits = c(0, 6),
                     breaks = seq(0, 6, 1)) +
  theme_bw() + 
  xlab("") +
  # ylab(bquote(b(alpha))) +
  ylab("Optimal burden") +
  ggtitle("Economy with pennies (current)") +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(size = 15, hjust = 0.50),
        axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 15, color = "black", family = "serif"),
        panel.grid.minor = element_blank())
print(stylizedExercise_figure_1)

stylizedExercise_figure_1.counter <- stylized_exercise_1.counter %>%
  ggplot(data = ., aes(x = a, y = n_tokens)) +
  geom_line(size = 1,
            lty = 3,
            color = "midnightblue") +
  geom_point(size = 2,
             color = "midnightblue") +
  scale_x_continuous(limits = c(0, 1),
                     breaks = c(0.01, seq(0.05, 1, 0.05))) +
  scale_y_continuous(limits = c(0, 4),
                     breaks = seq(0, 6, 1)) +
  theme_bw() + 
  ggtitle("Economy without pennies (counterfactual)") +
  xlab("Amount") +
  ylab("Optimal burden") +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(size = 15, hjust = 0.50),
        axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 15, color = "black", family = "serif"),
        panel.grid.minor = element_blank())
print(stylizedExercise_figure_1.counter)

multiplot(stylizedExercise_figure_1, 
          stylizedExercise_figure_1.counter, 
          cols = 1)
# dev.copy(jpeg,
#          "optimal_tokens_stylizedExercise_2021-04-06.jpeg",
#          height = output_height,
#          width = output_width,
#          units = "in", 
#          res = output_res)
# dev.off()

#=================================================================================================================

max_fig1 <- postCounter_df %>%
  subset(alpha_tilde <= 100 & has_2note == "All coins and notes") %>%
  dplyr::select(n_tokens) %>%
  table() %>%
  prop.table() %>%
  data.frame() %>%
  dplyr::rename(n_tokens = ".",
                share = "Freq") %>%
  dplyr::select(n_tokens) %>%
  unlist() %>%
  factor2numeric() %>%
  max() 

max_fig1_counter <- postCounter_df %>%
  subset(alpha_tilde <= 100 & has_2note == "All coins and notes") %>%
  dplyr::select(n_tokens_counter) %>%
  table() %>%
  prop.table() %>%
  data.frame() %>%
  dplyr::rename(n_tokens_counter = ".",
                share = "Freq") %>%
  dplyr::select(n_tokens_counter) %>%
  unlist() %>%
  factor2numeric() %>%
  max() 

counter_shares_figure_1 <- postCounter_df %>%
  subset(has_2note == "All coins and notes") %>%
  dplyr::select(n_tokens_counter) %>%
  table() %>%
  prop.table() %>%
  data.frame() %>%
  dplyr::rename(n_tokens_counter = ".",
                share = "Freq") %>%
  mutate(share = share * 100,
         n_tokens_counter = as.numeric(as.character(n_tokens_counter))) %>%
  subset(n_tokens_counter <= max_fig1_counter) %>%
  ggplot(data = ., aes(x = n_tokens_counter, y = share)) + 
  geom_histogram(aes(color = "color1", fill = "color1", alpha = "color1"), 
                 stat = "identity", 
                 size = 1,
                 width = 0.3) +
  geom_histogram(data =  postCounter_df %>%
                   subset(has_2note == "All coins and notes") %>%
                   dplyr::select(n_tokens) %>%
                   table() %>%
                   prop.table() %>%
                   data.frame() %>%
                   dplyr::rename(n_tokens = ".",
                                 share = "Freq") %>%
                   mutate(share = share * 100,
                          n_tokens = as.numeric(as.character(n_tokens))) %>%
                   subset(n_tokens <= max_fig1),
                 aes(x = n_tokens, y = share, color = "color2", fill = "color2", alpha = "color2"), 
                 width = 0.3, 
                 size = 1,
                 stat = "identity") +
  scale_x_continuous(limits = c(0.75, 8.5),
                     breaks = seq(1, 8, 1)) +
  scale_y_continuous("Share of cash transactions",
                     limits = c(0, 36), 
                     breaks = seq(0, 35, 5),
                     expand = c(0, 0)) +
  scale_fill_manual("Burden:", 
                    values = c("color1" = "skyblue1", "color2" = "royalblue4"), 
                    labels = c("color1" = "Without pennies (counterfactual)", "color2" = "With pennies (current)")) +
  scale_color_manual("Burden:", 
                     values = c("color1" = "skyblue1", "color2" = "royalblue4"), 
                     labels = c("color1" = "Without pennies (counterfactual)", "color2" = "With pennies (current)")) +
  scale_alpha_manual("Burden:", 
                     values = c("color1" = 1, "color2" = 0), 
                     labels = c("color1" = "Without pennies (counterfactual)", "color2" = "With pennies (current)")) +
  ggtitle("All coins and notes") + 
  xlab(expression(italic(b^"*"))) +
  theme_bw() + 
  theme(text = element_text(family = "serif"),
        legend.position = "bottom",
        plot.title = element_text(size = 15, hjust = 0.50),
        axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 15, color = "black"),
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank()) + 
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))


max_fig2 <- postCounter_df %>%
  subset(alpha_tilde <= 100 & has_2note != "All coins and notes") %>%
  dplyr::select(n_tokens) %>%
  table() %>%
  prop.table() %>%
  data.frame() %>%
  dplyr::rename(n_tokens = ".",
                share = "Freq") %>%
  dplyr::select(n_tokens) %>%
  unlist() %>%
  as.character() %>%
  as.numeric() %>%
  max()

max_fig2_counter <- postCounter_df %>%
  subset(alpha_tilde <= 100 & has_2note != "All coins and notes") %>%
  dplyr::select(n_tokens_counter) %>%
  table() %>%
  prop.table() %>%
  data.frame() %>%
  dplyr::rename(n_tokens_counter = ".",
                share = "Freq") %>%
  dplyr::select(n_tokens_counter) %>%
  unlist() %>%
  as.character() %>%
  as.numeric() %>%
  max()

counter_shares_figure_2 <- postCounter_df %>%
  subset(has_2note != "All coins and notes") %>%
  dplyr::select(n_tokens_counter) %>%
  table() %>%
  prop.table() %>%
  data.frame() %>%
  dplyr::rename(n_tokens_counter = ".",
                share = "Freq") %>%
  mutate(share = share * 100,
         n_tokens_counter = as.numeric(as.character(n_tokens_counter))) %>%
  subset(n_tokens_counter <= max_fig2_counter) %>%
  ggplot(data = ., aes(x = n_tokens_counter, y = share)) + 
  geom_histogram(aes(color = "color1", fill = "color1", alpha = "color1"), 
                 stat = "identity", 
                 size = 1,
                 width = 0.3) +
  geom_histogram(data =  postCounter_df %>%
                   subset(has_2note != "All coins and notes") %>%
                   dplyr::select(n_tokens) %>%
                   table() %>%
                   prop.table() %>%
                   data.frame() %>%
                   dplyr::rename(n_tokens = ".",
                                 share = "Freq") %>%
                   mutate(share = share * 100,
                          n_tokens = as.numeric(as.character(n_tokens))) %>%
                   subset(n_tokens <= max_fig1),
                 aes(x = n_tokens, y = share, color = "color2", fill = "color2", alpha = "color2"), 
                 width = 0.3, 
                 size = 1,
                 stat = "identity") +
  scale_x_continuous(limits = c(0.75, 9.5),
                     breaks = seq(1, 10, 1)) +
  scale_y_continuous("Share of cash transactions",
                     limits = c(0, 36), 
                     breaks = seq(0, 35, 5),
                     expand = c(0, 0)) +
  scale_fill_manual("Burden:", 
                    values = c("color1" = "skyblue1", "color2" = "royalblue4"), 
                    labels = c("color1" = "Without pennies (counterfactual)", "color2" = "With pennies (current)")) +
  scale_color_manual("Burden:", 
                     values = c("color1" = "skyblue1", "color2" = "royalblue4"), 
                     labels = c("color1" = "Without pennies (counterfactual)", "color2" = "With pennies (current)")) +
  scale_alpha_manual("Burden:", 
                     values = c("color1" = 1, "color2" = 0), 
                     labels = c("color1" = "Without pennies (counterfactual)", "color2" = "With pennies (current)")) +
  ggtitle("No $2 notes") + 
  xlab(expression(italic(b^"*"))) +
  theme_bw() + 
  theme(text = element_text(family = "serif"),
        legend.position = "bottom",
        plot.title = element_text(size = 15, hjust = 0.50),
        axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 15, color = "black"),
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank()) + 
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

multiplot(counter_shares_figure_1, counter_shares_figure_2, cols = 2)
# dev.copy(jpeg,
#          "counter_optimal_tokens_share_2021-04-07.jpeg",
#          height = output_height,
#          width = output_width,
#          units = "in", 
#          res = output_res)
# dev.off()

#=================================================================================================================

deltaAlpha_hist_merch <- postCounter_df$payee %>% 
  unique() %>% 
  na.omit() %>% 
  str_c() %>% 
  as.list() %>%
  map(.f = function(p) {
    
    y_title <- ifelse(p %in% c("A person", "Nonprofit, charity or religion", "Government",
                               "Hospital, doctor or dentist"), "", "Share at merchant")
    
    
    out_figure <- postCounter_df %>%
      subset(payee == p) %>%
      dplyr::select(Delta_alpha) %>% 
      mutate(Delta_alpha = round(Delta_alpha, 2)) %>%
      table() %>% 
      prop.table() %>% 
      data.frame() %>% 
      dplyr::rename(Change = ".", 
                    Share = Freq) %>%
      mutate(Change = as.numeric(as.character(Change))) %>%
      ggplot(data = ., aes(x = Change, y = Share * 100)) +
      geom_histogram(stat = "identity", 
                     fill = "firebrick", 
                     bins = 5,
                     width = 0.008,
                     color = "white") +
      # geom_density(color = "royalblue4") +
      scale_y_continuous(y_title,
                         limits = c(0, 125), 
                         breaks = seq(0, 100, 25),
                         expand = c(0, 0)) +
      scale_x_continuous(limits = c(-0.03, 0.03), 
                         breaks = seq(-.02, .02, .01)) +
      xlab(expression(Delta^a)) +
      ggtitle(p) +
      geom_text(aes(label = round(Share * 100, 2)),
                nudge_y = 10, 
                family = "serif") +
      theme_bw() + 
      theme(text = element_text(family = "serif"),
            plot.title = element_text(hjust = 0.50),
            axis.text = element_text(size = 13, color = "black"),
            axis.title = element_text(size = 14, color = "black"),
            panel.grid.minor = element_blank(), 
            panel.grid.major.x = element_blank()) 
    
    return(out_figure)
     
  })

gridExtra::grid.arrange(deltaAlpha_hist_merch[[1]], deltaAlpha_hist_merch[[2]], deltaAlpha_hist_merch[[3]], 
                        deltaAlpha_hist_merch[[4]], deltaAlpha_hist_merch[[5]], deltaAlpha_hist_merch[[6]], 
                        deltaAlpha_hist_merch[[7]], deltaAlpha_hist_merch[[8]],
                        ncol = 2)
# dev.copy(jpeg,
#          "dist_merch_types-2021-04-01.jpeg",
#          height = output_height + 2,
#          width = output_width,
#          units = "in",
#          res = output_res)
# dev.off()

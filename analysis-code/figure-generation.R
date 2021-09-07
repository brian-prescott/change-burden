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
library(data.table)
library(grDevices)
library(graphics)
library(xtable)
library(boot)

#===============================================================================
# PARAMETERS #
#===============================================================================
# insert the path to your datasets here
directory <- str_c(
  Sys.getenv("USERPROFILE"), 
  "\\PrescottLibrary\\papers\\academic-papers\\burden-cash\\"
)
setwd(paste0(directory, "data\\"))

output_height <- 7
output_width <- 12
output_res <- 400

#===============================================================================
                                  # FIGURES #
#===============================================================================
cash_dist <- tran_df %>%
  subset(amnt <= 100) %>%
  ggplot(data = ., aes(x = amnt)) +
  geom_histogram(
    aes(y = ..density..),
    color = "white",
    bins = 105,
    bindwidth = 1,
    # bins = 75,
    fill = "royalblue4"
  ) +
  scale_y_continuous(
    "Density",
    limits = c(0, 0.101),
    breaks = seq(0, 1, .02),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    "Amount ($)",
    limits = c(0, 101),
    breaks = seq(0, 100, 5),
    expand = c(0, 0)
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "serif"),
    axis.text = element_text(size = 13, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
print(cash_dist)
# ggsave(filename = "cash_dist_2015-2019.jpeg",
#        device = "jpeg",
#        plot = last_plot(),
#        height = output_height,
#        width = output_width)

#===============================================================================

max_shares_fig1 <- postCounter_df %>%
  subset(alpha <= 100 & has_2note == "All coins and notes") %>%
  dplyr::select(n_tokens) %>%
  table() %>%
  prop.table() %>%
  data.frame() %>%
  dplyr::rename(n_tokens = ".",
                share = "Freq") %>%
  mutate(
    share = share * 100,
    n_tokens = as.numeric(as.character(n_tokens)),
    which_notes = "All coins and notes"
  ) %>%
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
  mutate(
    share = share * 100,
    n_tokens = as.numeric(as.character(n_tokens)),
    which_notes = "All coins and notes"
  ) %>%
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
  mutate(
    share = share * 100,
    n_tokens = as.numeric(as.character(n_tokens)),
    which_notes = "All coins and notes"
  ) %>%
  subset(n_tokens <= max_shares_fig1) %>%
  ggplot(data = ., aes(x = n_tokens, y = share)) +
  geom_histogram(
    stat = "identity",
    fill = "royalblue4",
    width = 0.3,
    color = "white"
  ) +
  scale_x_continuous(limits = c(0.75, 9.5),
                     breaks = seq(1, 10, 1)) +
  scale_y_continuous(
    "Share of cash transactions",
    limits = c(0, 31),
    breaks = seq(0, 35, 5),
    expand = c(0, 0)
  ) +
  geom_text(
    aes(label = round(share, 1)),
    nudge_y = 0.5,
    color = "black",
    size = 5,
    family = "serif"
  ) +
  xlab(expression(italic(b ^ "*"))) +
  ggtitle("All coins and notes") +
  theme_bw() +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(size = 15, hjust = 0.50),
    axis.text = element_text(size = 14, color = "black"),
    strip.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 15, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

shares_figure_2 <- postCounter_df %>%
  subset(has_2note != "All coins and notes") %>%
  dplyr::select(n_tokens) %>%
  table() %>%
  prop.table() %>%
  data.frame() %>%
  dplyr::rename(n_tokens = ".",
                share = "Freq") %>%
  mutate(
    share = share * 100,
    n_tokens = as.numeric(as.character(n_tokens)),
    which_notes = "No $2 notes"
  ) %>%
  subset(n_tokens <= max_shares_fig2) %>%
  ggplot(data = ., aes(x = n_tokens, y = share)) +
  geom_histogram(
    stat = "identity",
    fill = "royalblue4",
    width = 0.3,
    color = "white"
  ) +
  scale_x_continuous(limits = c(0.75, 9.5),
                     breaks = seq(1, 100, 1)) +
  scale_y_continuous(
    "Share of cash transactions",
    limits = c(0, 31),
    breaks = seq(0, 35, 5),
    expand = c(0, 0)
  ) +
  geom_text(
    aes(label = round(share, 1)),
    nudge_y = 0.5,
    color = "black",
    size = 5,
    family = "serif"
  ) +
  xlab(expression(italic(b ^ "*"))) +
  ggtitle("No $2 notes") +
  theme_bw() +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(size = 15, hjust = 0.50),
    axis.text = element_text(size = 14, color = "black"),
    strip.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 15, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

multiplot(shares_figure_1, shares_figure_2, cols = 2)
# dev.copy(jpeg,
#          "optimal_tokens_share_2021-04-07.jpeg",
#          height = output_height,
#          width = output_width,
#          units = "in",
#          res = output_res)
# dev.off()

#===============================================================================

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
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(size = 15, hjust = 0.50),
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(
      size = 15,
      color = "black",
      family = "serif"
    ),
    panel.grid.minor = element_blank()
  )
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
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(size = 15, hjust = 0.50),
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(
      size = 15,
      color = "black",
      family = "serif"
    ),
    panel.grid.minor = element_blank()
  )
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

#===============================================================================

max_fig1 <- postCounter_df %>%
  subset(alpha_tilde <= 100 &
           has_2note == "All coins and notes") %>%
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
  subset(alpha_tilde <= 100 &
           has_2note == "All coins and notes") %>%
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
  geom_histogram(
    aes(
      color = "color1",
      fill = "color1",
      alpha = "color1"
    ),
    stat = "identity",
    size = 1,
    width = 0.3
  ) +
  geom_histogram(
    data =  postCounter_df %>%
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
    aes(
      x = n_tokens,
      y = share,
      color = "color2",
      fill = "color2",
      alpha = "color2"
    ),
    width = 0.3,
    size = 1,
    stat = "identity"
  ) +
  scale_x_continuous(limits = c(0.75, 8.5),
                     breaks = seq(1, 8, 1)) +
  scale_y_continuous(
    "Share of cash transactions",
    limits = c(0, 36),
    breaks = seq(0, 35, 5),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    "Burden:",
    values = c("color1" = "skyblue1", "color2" = "royalblue4"),
    labels = c("color1" = "Without pennies (counterfactual)", 
               "color2" = "With pennies (current)")
  ) +
  scale_color_manual(
    "Burden:",
    values = c("color1" = "skyblue1", "color2" = "royalblue4"),
    labels = c("color1" = "Without pennies (counterfactual)",
               "color2" = "With pennies (current)")
  ) +
  scale_alpha_manual(
    "Burden:",
    values = c("color1" = 1, "color2" = 0),
    labels = c("color1" = "Without pennies (counterfactual)", 
               "color2" = "With pennies (current)")
  ) +
  ggtitle("All coins and notes") +
  xlab(expression(italic(b ^ "*"))) +
  theme_bw() +
  theme(
    text = element_text(family = "serif"),
    legend.position = "bottom",
    plot.title = element_text(size = 15, hjust = 0.50),
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 15, color = "black"),
    legend.text = element_text(size = 14, color = "black"),
    legend.title = element_text(size = 15, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))


max_fig2 <- postCounter_df %>%
  subset(alpha_tilde <= 100 &
           has_2note != "All coins and notes") %>%
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
  subset(alpha_tilde <= 100 &
           has_2note != "All coins and notes") %>%
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
  geom_histogram(
    aes(
      color = "color1",
      fill = "color1",
      alpha = "color1"
    ),
    stat = "identity",
    size = 1,
    width = 0.3
  ) +
  geom_histogram(
    data =  postCounter_df %>%
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
    aes(
      x = n_tokens,
      y = share,
      color = "color2",
      fill = "color2",
      alpha = "color2"
    ),
    width = 0.3,
    size = 1,
    stat = "identity"
  ) +
  scale_x_continuous(limits = c(0.75, 9.5),
                     breaks = seq(1, 10, 1)) +
  scale_y_continuous(
    "Share of cash transactions",
    limits = c(0, 36),
    breaks = seq(0, 35, 5),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    "Burden:",
    values = c("color1" = "skyblue1", "color2" = "royalblue4"),
    labels = c(
      "color1" = "Without pennies (counterfactual)",
      "color2" = "With pennies (current)"
    )
  ) +
  scale_color_manual(
    "Burden:",
    values = c("color1" = "skyblue1", "color2" = "royalblue4"),
    labels = c("color1" = "Without pennies (counterfactual)", 
               "color2" = "With pennies (current)")
  ) +
  scale_alpha_manual(
    "Burden:",
    values = c("color1" = 1, "color2" = 0),
    labels = c("color1" = "Without pennies (counterfactual)", 
               "color2" = "With pennies (current)")
  ) +
  ggtitle("No $2 notes") +
  xlab(expression(italic(b ^ "*"))) +
  theme_bw() +
  theme(
    text = element_text(family = "serif"),
    legend.position = "bottom",
    plot.title = element_text(size = 15, hjust = 0.50),
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 15, color = "black"),
    legend.text = element_text(size = 14, color = "black"),
    legend.title = element_text(size = 15, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

multiplot(counter_shares_figure_1, counter_shares_figure_2, cols = 2)
# dev.copy(jpeg,
#          "counter_optimal_tokens_share_2021-04-07.jpeg",
#          height = output_height,
#          width = output_width,
#          units = "in",
#          res = output_res)
# dev.off()

#===============================================================================

deltaAlpha_hist_merch <- postCounter_df$payee %>%
  unique() %>%
  na.omit() %>%
  str_c() %>%
  as.list() %>%
  map(
    .f = function(p) {
      y_title <-
        ifelse(
          p %in% c(
            "A person",
            "Nonprofit, charity or religion",
            "Government",
            "Hospital, doctor or dentist"
          ),
          "",
          "Share at merchant"
        )
      
      
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
        geom_histogram(
          stat = "identity",
          fill = "firebrick",
          bins = 5,
          width = 0.008,
          color = "white"
        ) +
        # geom_density(color = "royalblue4") +
        scale_y_continuous(
          y_title,
          limits = c(0, 125),
          breaks = seq(0, 100, 25),
          expand = c(0, 0)
        ) +
        scale_x_continuous(limits = c(-0.03, 0.03),
                           breaks = seq(-.02, .02, .01)) +
        xlab(expression(Delta ^ a)) +
        ggtitle(p) +
        geom_text(aes(label = round(Share * 100, 2)),
                  nudge_y = 10,
                  family = "serif") +
        theme_bw() +
        theme(
          text = element_text(family = "serif"),
          plot.title = element_text(hjust = 0.50),
          axis.text = element_text(size = 13, color = "black"),
          axis.title = element_text(size = 14, color = "black"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank()
        )
      
      return(out_figure)
      
    }
  )

gridExtra::grid.arrange(
  deltaAlpha_hist_merch[[1]],
  deltaAlpha_hist_merch[[2]],
  deltaAlpha_hist_merch[[3]],
  deltaAlpha_hist_merch[[4]],
  deltaAlpha_hist_merch[[5]],
  deltaAlpha_hist_merch[[6]],
  deltaAlpha_hist_merch[[7]],
  deltaAlpha_hist_merch[[8]],
  ncol = 2
)
# dev.copy(jpeg,
#          "dist_merch_types-2021-04-01.jpeg",
#          height = output_height + 2,
#          width = output_width,
#          units = "in",
#          res = output_res)
# dev.off()

#===============================================================================
# Plotting the empirical cumulative distribution function and the theoretical
# log-normal distribution function. Doing this for the current and
# counterfactual change burdens

postCounter_df %>%
  ggplot(data = ., aes(x = n_tokens)) +
  stat_ecdf(geom = "point", color = "black", size = 3) +
  geom_line(
    data = plnorm(
      q = unique(na.omit(postCounter_df$n_tokens)),
      meanlog = log(mean(postCounter_df$n_tokens, na.rm = TRUE)),
      sdlog = log(sd(postCounter_df$n_tokens, na.rm = TRUE))
    ) %>%
      data.frame(x = unique(na.omit(
        postCounter_df$n_tokens
      )),
      y = .) %>%
      arrange(x),
    aes(x = x, y = y),
    size = 0.5
  ) +
  scale_x_continuous("Cash burden",
                     breaks = seq(0, 65, 5)) +
  scale_y_continuous("") +
  ggtitle("Current economy") +
  theme_bw() +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(size = 15, hjust = 0.5),
    axis.text = element_text(size = 13, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.minor = element_blank()
  )

postCounter_df %>%
  ggplot(
    data = ., 
    aes(x = n_tokens_counter)
  ) +
  stat_ecdf(
    geom = "point", 
    color = "black", 
    size = 3
  ) +
  geom_line(
    data = plnorm(
      q = unique(na.omit(postCounter_df$n_tokens_counter)),
      meanlog = log(mean(postCounter_df$n_tokens_counter, na.rm = TRUE)),
      sdlog = log(sd(postCounter_df$n_tokens_counter, na.rm = TRUE))
    ) %>%
      data.frame(x = unique(
        na.omit(postCounter_df$n_tokens_counter)
      ),
      y = .) %>%
      arrange(x),
    aes(x = x, y = y),
    size = 0.5
  ) +
  scale_x_continuous("Cash burden",
                     breaks = seq(0, 65, 5)) +
  scale_y_continuous("") +
  ggtitle("Counterfactual economy") +
  theme_bw() +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(size = 15, hjust = 0.5),
    axis.text = element_text(size = 13, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.minor = element_blank()
  )

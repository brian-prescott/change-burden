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
last_base_run <- "2021-09-07"
last_counter_run <- "2021-09-08"

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
                                # FUNCTIONS #
#===============================================================================
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

factor2numeric <- function(x) {
  return(as.numeric(as.character(x)))
}

#===============================================================================
# PARAMETERS #
#===============================================================================
# insert the path to your datasets here
directory <- stringr::str_remove(Sys.getenv("PWD"), "analysis-code")
setwd(paste0(directory, "data"))

output_height <- 7
output_width <- 12
output_res <- 400

#===============================================================================
                                  # DATA IMPORT #
#===============================================================================
tran_df <- read_rds("cash_transactions_df.rds") %>%
  dplyr::rename(uasid = id)

load(str_c("baseline-results_", last_base_run, ".RData"))
load(str_c("policy-results_", last_counter_run, ".RData"))
load("counterfactual-results_2021-09-07.RData")

#===============================================================================
# DATA PREP #
#===============================================================================
baseline_figures_df <- c("Symmetric policy", "Asymmetric policy") %>%
  as.list() %>%
  purrr::map(
    .x = .,
    .f = function(policy) {
      out <- c("All coins and notes", "Only $20 note") %>%
        as.list() %>%
        purrr::map(
          .x = .,
          .f = function(economy) {
            threshold <- optimal_tokens_df %>%
              dplyr::filter(only_20note == economy & a <= 100) %>%
              dplyr::select(n_tokens) %>%
              table() %>%
              prop.table() %>%
              data.frame() %>%
              dplyr::rename(n_tokens = ".") %>%
              mutate(n_tokens = as.numeric(as.character(n_tokens))) %>%
              dplyr::select(n_tokens) %>%
              unlist() %>%
              as.numeric() %>%
              max()

            output <- optimal_tokens_df %>%
              dplyr::filter(only_20note == economy) %>%
              dplyr::select(n_tokens) %>%
              table() %>%
              prop.table() %>%
              data.frame() %>%
              dplyr::rename(
                n_tokens_counter = ".",
                share = "Freq"
              ) %>%
              mutate(
                share = share * 100,
                n_tokens = as.numeric(as.character(n_tokens_counter)),
                which_policy = policy,
                which_notes = economy
              ) %>%
              dplyr::filter(n_tokens <= threshold)

          return(output)
        }
      )

    return(out)
  }
) %>%
bind_rows() %>%
mutate(model = "With pennies (current)")

shares_figure_df <- 1:2 %>%
  as.list() %>%
  purrr::map(
    .x = .,
    .f = function(i) {
      df <- list(symmetric_pol_df, asymmetric_pol_df)[[i]]
      policy <- c("Symmetric policy", "Asymmetric policy")[i]

      final_df <- c("All coins and notes", "Only $20 note") %>%
        as.list() %>%
        purrr::map(
          .x = .,
          .f = function(economy) {
            using_df <- df %>%
              dplyr::filter(only_20note == economy)

            threshold <- unique(
              max(
                dplyr::filter(using_df, alpha <= 100)$n_tokens_counter
              )
            )

            output <- using_df %>%
              dplyr::select(n_tokens_counter) %>%
              table() %>%
              prop.table() %>%
              data.frame() %>%
              dplyr::rename(
                n_tokens_counter = ".",
                share = "Freq"
              ) %>%
              mutate(
                share = share * 100,
                n_tokens = as.numeric(as.character(n_tokens_counter)),
                which_policy = policy,
                which_notes = economy
              ) %>%
              dplyr::filter(n_tokens <= threshold)

            return(output)
          }
        ) %>%
        bind_rows()

      return(final_df)
    }
  ) %>%
  bind_rows() %>%
  as_tibble() %>%
  mutate(model = "Without pennies (counterfactual)") %>%
  bind_rows(
    x = .,
    y = baseline_figures_df
  )


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
ggplot2::ggsave(
  filename = "cash_dist_2015-2019.jpeg",
  device = "jpeg",
  plot = last_plot(),
  height = output_height,
  width = output_width
)

#===============================================================================
# The choice of policy has no effect on the outcome
baseline_figures_df %>%
  dplyr::filter(which_policy == "Symmetric policy") %>%
  ggplot(
    data = ., 
    aes(
      x = n_tokens, 
      y = share
    )
  ) +
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
  theme_bw() +
  facet_wrap(~ which_notes) +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(size = 15, hjust = 0.50),
    axis.text = element_text(size = 14, color = "black"),
    strip.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 15, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
ggplot2::ggsave(
  filename = "baseline-burden_2015-2019.jpeg",
  device = "jpeg",
  plot = last_plot(),
  height = output_height,
  width = output_width
)
#===============================================================================
shares_figure_df %>%
  mutate(
    which_policy = factor(
      x = which_policy,
      levels = str_c(c("Symmetric", "Asymmetric"), " policy")
    )
  ) %>%
  ggplot(
    data = .,
    aes(
      x = n_tokens,
      y = share,
      color = model,
      fill = model,
      alpha = model
    )
  ) +
  geom_histogram(
    stat = "identity",
    position = position_dodge(0),
    size = 1,
    width = 0.3
  ) +
  scale_x_continuous(
    limits = c(0.75, 8.5),
    breaks = seq(1, 8, 1)
  ) +
  scale_y_continuous(
    name = "Share of cash transactions",
    limits = c(0, 27.5),
    breaks = seq(0, 30, 5),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Without pennies (counterfactual)" = "skyblue1",
      "With pennies (current)" = "royalblue4"
    )
  ) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Without pennies (counterfactual)" = "skyblue1",
      "With pennies (current)" = "royalblue4"
    )
  ) +
  scale_alpha_manual(
    name = NULL,
    values = c(
      "Without pennies (counterfactual)" = 1, 
      "With pennies (current)" = 0
    )
  ) +
  xlab(expression(italic(b ^ "*"))) +
  facet_wrap(~ which_policy + which_notes) +
  theme_bw() +
  theme(
    text = element_text(family = "serif"),
    legend.position = "bottom",
    plot.title = element_text(size = 15, hjust = 0.50),
    axis.text = element_text(size = 14, color = "black"),    
    axis.title = element_text(size = 15, color = "black"),
    strip.text = element_text(size = 14, color = "black"),
    legend.text = element_text(size = 14, color = "black"),
    legend.title = element_text(size = 15, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))
ggplot2::ggsave(
  filename = "counterfactual-burden_2015-2019.jpeg",
  device = "jpeg",
  plot = last_plot(),
  height = output_height,
  width = output_width
)

#===============================================================================
stylizedExercise_figure_1 <- stylized_exercise_1 %>%
  ggplot(data = ., aes(x = a, y = n_tokens)) +
  geom_line(size = 1,
            lty = 3,
            color = "midnightblue") +
  geom_point(size = 2,
             color = "midnightblue") +
  scale_x_continuous(limits = c(4, 5),
                     breaks = seq(4, 5, 0.05)) +
  scale_y_continuous(limits = c(1, 6),
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

stylizedExercise_figure_1.counter <- stylized_exercise_2 %>%
  ggplot(
    data = ., 
    aes(
      x = a, 
      y = n_tokens
    )
  ) +
  geom_line(
    size = 1,
    lty = 3,
    color = "midnightblue"
  ) +
  geom_point(size = 2,
             color = "midnightblue") +
  scale_x_continuous(limits = c(4, 5),
                     breaks = seq(4, 5, 0.05)) +
  scale_y_continuous(limits = c(1, 6),
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

multiplot(
  stylizedExercise_figure_1,
  stylizedExercise_figure_1.counter,
  cols = 1
)
dev.copy(
  jpeg,
  "optimal-tokens-stylized-exercise.jpeg",
  height = output_height,
  width = output_width,
  units = "in",
  res = output_res
)
dev.off()

#===============================================================================
deltaAlpha_hist_merch <- list(symmetric_pol_df, asymmetric_pol_df) %>%
  purrr::map(
    .x = .,
    .f = function(postCounter_df) {
      output <- postCounter_df$payee %>%
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
              scale_x_continuous(
                limits = c(-0.05, 0.05),
                breaks = seq(-.05, .05, .01)
              ) +
              xlab(expression(Delta^a)) +
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

    return(output)
  }
)
names(deltaAlpha_hist_merch) <- str_c(c("symmetric", "asymmetric"), "_policy")
gridExtra::grid.arrange(
  deltaAlpha_hist_merch$symmetric_policy[[1]],
  deltaAlpha_hist_merch$symmetric_policy[[2]],
  deltaAlpha_hist_merch$symmetric_policy[[3]],
  deltaAlpha_hist_merch$symmetric_policy[[4]],
  deltaAlpha_hist_merch$symmetric_policy[[5]],
  deltaAlpha_hist_merch$symmetric_policy[[6]],
  deltaAlpha_hist_merch$symmetric_policy[[7]],
  deltaAlpha_hist_merch$symmetric_policy[[8]],
  ncol = 2
)
dev.copy(
  jpeg,
  str_c("merch-price-effects-symmetric_", Sys.Date(), ".jpeg"),
  height = output_height + 2,
  width = output_width,
  units = "in",
  res = output_res
)
dev.off()

gridExtra::grid.arrange(
  deltaAlpha_hist_merch$asymmetric_policy[[1]],
  deltaAlpha_hist_merch$asymmetric_policy[[2]],
  deltaAlpha_hist_merch$asymmetric_policy[[3]],
  deltaAlpha_hist_merch$asymmetric_policy[[4]],
  deltaAlpha_hist_merch$asymmetric_policy[[5]],
  deltaAlpha_hist_merch$asymmetric_policy[[6]],
  deltaAlpha_hist_merch$asymmetric_policy[[7]],
  deltaAlpha_hist_merch$asymmetric_policy[[8]],
  ncol = 2
)
dev.copy(
  jpeg,
  str_c("merch-price-effects-asymmetric_", Sys.Date(), ".jpeg"),
  height = output_height + 2,
  width = output_width,
  units = "in",
  res = output_res
)
dev.off()
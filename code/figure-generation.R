#===============================================================================
                                # PREAMBLE #
#===============================================================================
# Written by: Brian Prescott
# Last edited: 2022-07-15

# Purpose: This script generates the original figures and tables for the paper
# "Cash Payments and the Penny Policy Debate" by Brian Prescott and Oz Shy.

# Notes: We have since added extra figures which are housed in new scripts.

# Cleaning out the environment before running the script
rm(list = ls())

# THESE SHOULD BE CONSIDERED BEFORE EVERY RUN.
last_base_run <- "2022-07-16"
last_counter_run <- "2022-07-16"

#===============================================================================
                                # LIBRARIES #
#===============================================================================
library(tidyverse)
library(grDevices)
library(graphics)
library(xtable)

#==============================================================================#
                                # FUNCTIONS #
#==============================================================================#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots <- length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots == 1) {

    print(plots[[1]])

  } else {

    # Set up the page
    grid.newpage() #nolint
    pushViewport(
      viewport(layout = grid.layout(nrow(layout), ncol(layout)))
    ) #nolint

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

#==============================================================================#
                                # VARIABLES #
#==============================================================================#
# insert the path to your datasets here
directory <- stringr::str_remove(Sys.getenv("PWD"), "code")
setwd(directory)

output_height <- 7
output_width <- 12
output_res <- 400

#==============================================================================#
                                # DATA IMPORT #
#==============================================================================#
tran_df <- readr::read_rds("./data/cash_transactions_df.rds") %>%
  dplyr::rename(uasid = id)

load(stringr::str_c("./data/baseline-results_", last_base_run, ".RData"))
load(stringr::str_c("./data/policy-results_", last_counter_run, ".RData"))
load(
  stringr::str_c(
    "./data/counterfactual-results_", last_counter_run, ".RData"
  )
)

#==============================================================================#
                                # DATA PREP #
#==============================================================================#
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
              dplyr::rename(alpha = a) %>%
              dplyr::filter(only_20note == economy & alpha <= 100) %>%
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


#==============================================================================#
                                # FIGURES #
#==============================================================================#
cash_dist <- tran_df %>%
  subset(amnt <= 100) %>%
  ggplot(data = ., aes(x = amnt)) +
  geom_histogram(
    aes(y = ..density..),
    color = "white",
    bins = 105,
    bindwidth = 1,
    # bins = 75,
    fill = "#0e4768"
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
    axis.text = element_text(size = 17, color = "black"),
    axis.title = element_text(size = 18, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
print(cash_dist)
ggplot2::ggsave(
  filename = "./figures/cash_dist_2015-2019.jpeg",
  device = "jpeg",
  plot = cash_dist,
  height = output_height,
  width = output_width
)

#==============================================================================#
# The choice of policy has no effect on the outcome
baseline_burden_figure <- baseline_figures_df %>%
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
    fill = "#0e4768",
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
    size = 7,
    family = "serif"
  ) +
  xlab(expression(italic(b ^ "*"))) +
  theme_bw() +
  facet_wrap(~ which_notes) +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(size = 15, hjust = 0.50),
    axis.text = element_text(size = 16, color = "black"),
    strip.text = element_text(size = 16, color = "black"),
    axis.title = element_text(size = 17, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
ggplot2::ggsave(
  filename = "./figures/baseline-burden_2015-2019.jpeg",
  device = "jpeg",
  plot = baseline_burden_figure,
  height = output_height,
  width = output_width
)
#==============================================================================#
shares_figure_data <- shares_figure_df %>%
  mutate(
    which_policy = factor(
      x = which_policy,
      levels = str_c(c("Symmetric", "Asymmetric"), " policy")
    )
  )

# Checking to see where and how the symmetric policy is different from the
# asymmetric variant. Differences exist, however, they are very small.
shares_figure_data %>%
  dplyr::filter(
    which_notes == "All coins and notes" &
    model == "Without pennies (counterfactual)" &
    which_policy == "Asymmetric policy"
  ) %>%
  dplyr::select(share, n_tokens)
shares_figure_data %>%
  dplyr::filter(
    which_notes == "All coins and notes" &
    model == "Without pennies (counterfactual)" &
    which_policy == "Symmetric policy"
  ) %>%
  dplyr::select(share, n_tokens)

shares_figure_data %>%
  dplyr::filter(
    which_notes == "Only $20 note" &
    model == "Without pennies (counterfactual)" &
    which_policy == "Asymmetric policy"
  ) %>%
  dplyr::select(share, n_tokens)
shares_figure_data %>%
  dplyr::filter(
    which_notes == "Only $20 note" &
    model == "Without pennies (counterfactual)" &
    which_policy == "Symmetric policy"
  ) %>%
  dplyr::select(share, n_tokens)

# Plotting the results
counter_shares_figure <- shares_figure_data %>%
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
    limits = c(0, 29.5),
    breaks = seq(0, 30, 5),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Without pennies (counterfactual)" = "#b5c7d1",
      "With pennies (current)" = "#0e4768"
    )
  ) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Without pennies (counterfactual)" = "#b5c7d1",
      "With pennies (current)" = "#0e4768"
    )
  ) +
  scale_alpha_manual(
    name = NULL,
    values = c(
      "Without pennies (counterfactual)" = 1,
      "With pennies (current)" = 0
    )
  ) +
  xlab(expression(italic(b^"*"))) +
  facet_wrap(~ which_policy + which_notes) +
  theme_bw() +
  theme(
    text = element_text(family = "serif"),
    legend.position = "bottom",
    plot.title = element_text(size = 15, hjust = 0.50),
    axis.text = element_text(size = 16, color = "black"),
    axis.title = element_text(size = 18, color = "black"),
    strip.text = element_text(size = 16, color = "black"),
    legend.text = element_text(size = 16, color = "black"),
    legend.title = element_text(size = 16, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))
ggplot2::ggsave(
  filename = "./figures/counterfactual-burden_2015-2019.jpeg",
  device = "jpeg",
  plot = counter_shares_figure,
  height = output_height,
  width = output_width
)

#==============================================================================#
all_rounding_vals <- tibble::tibble(amnt = as.character(seq(-0.02, 0.04, 0.01)))

price_effects_tbl <- list(symmetric_pol_df, asymmetric_pol_df) %>%
  purrr::map(
    .f = function(df) {
      output <- df %>%
        dplyr::filter(only_20note == "All coins and notes") %>%
        dplyr::select(Delta_alpha) %>%
        round(x = ., digits = 2) %>%
        table() %>%
        prop.table() %>%
        data.frame() %>%
        dplyr::rename(
          amnt = ".",
          share = Freq
        ) %>%
        dplyr::mutate(
          share = share * 100,
        ) %>%
        arrange(amnt) %>%
        dplyr::full_join(
          x = .,
          y = all_rounding_vals,
          by = "amnt"
        ) 

      return(output)
    }
  )
names(price_effects_tbl) <- c("sym", "asym")

price_effects_tbl %>%
  purrr::reduce(
    .x = .,
    .f = full_join,
    by = "amnt",
    suffix = c("_sym", "_asym")
    ) %>%
  t() %>%
  data.frame() %>%
  dplyr::mutate_all(.funs = as.numeric) %>%
  tibble::as_tibble() %>%
  xtable::xtable()

#==============================================================================#
penny_df <- readr::read_csv("./data/penny-sq-df.csv") %>%
  dplyr::mutate(
    response = ifelse(
      test = response == "I always leave pennies with cashier or tip jar, etc",
      yes = "I always leave pennies with \ncashier or tip jar, etc",
      no = response
    ),
    response = ifelse(
      test = response == 
      "Give pennies to charity box, take/leave penny, or tip jar",
      yes = "Give pennies to charity box, \ntake/leave penny, or tip jar",
      no = response
    ),
    response = ifelse(
      test = response == "Put pennies in pocket, purse, or wallet",
      yes = "Put pennies in pocket,\n purse, or wallet",
      no = response
    ),
  )

penny_sq_loops <- unique(penny_df$question)

penny_sq_figures <- penny_sq_loops %>%
  as.list() %>%
  purrr::map(
    .x = .,
    .f = function(q) {
      output <- penny_df %>%
        dplyr::filter(question == q) %>%
        dplyr::mutate(year = factor(x = year, levels = c("2020", "2019"))) %>%
        ggplot(
          data = .,
          aes(
            x = response,
            y = share,
            fill = year
          )
        ) +
        geom_histogram(
          stat = "identity",
          position = position_dodge(0.5),
          width = 0.5,
          show.legend = ifelse(
            test = q == penny_sq_loops[1],
            yes = FALSE,
            no = TRUE
          )
        ) +
        geom_text(
          aes(y = share + 2.5, label = share),
          position = position_dodge(0.5),
          size = 6,
          color = "black",
          family = "serif"
        ) +
        scale_y_continuous(
          name = "Share of consumers",
          limits = c(0, 83),
          breaks = seq(0, 80, 20),
          expand = c(0, 0)
        ) +
        scale_x_discrete(name = NULL) +
        scale_fill_manual(
          name = NULL,
          values = c("#0e4678", "#f5892f"),
          guide = guide_legend(reverse = TRUE)
        ) +
        theme_bw() +
        coord_flip() +
        facet_wrap(~ question) +
        theme(
          text = element_text(family = "serif"),
          plot.title = element_text(size = 18, hjust = 0.50),
          strip.text = element_text(size = 16, color = "black"),
          legend.text = element_text(size = 16, color = "black"),
          axis.text = element_text(size = 16, color = "black"),
          axis.title = element_text(size = 17, color = "black"),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "bottom",
          legend.background = element_rect(color = "black")
        )

      return(output)
    }
  )
gridExtra::grid.arrange(
  penny_sq_figures[[1]],
  penny_sq_figures[[2]],
  ncol = 1
)
dev.copy(
  jpeg,
  "./figures/penny-sq.jpeg",
  height = output_height + 2,
  width = output_width,
  units = "in",
  res = output_res
)
dev.off()
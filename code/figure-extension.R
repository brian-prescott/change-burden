#==============================================================================#
# PREAMBLE #
#==============================================================================#
# Written by: Brian Prescott
# Last edited: 2022-07-15

# Purpose: This script generates the new figures and tables for the paper
# "Cash Payments and the Penny Policy Debate" by Brian Prescott and Oz Shy.
# These figures and tables were added after our first round of revisions at the
# Journal of Economic Behavior and Organization.

# Cleaning out the environment before running the script
rm(list = ls())

initial_run <- FALSE
run_stylized <- FALSE
last_base_run <- "2022-07-16"
last_counter_run <- "2022-07-16"

#==============================================================================#
# LIBRARIES #
#==============================================================================#
library(tidyverse)
library(grDevices)
library(graphics)
library(xtable)

#==============================================================================#
# VARIABLES #
#==============================================================================#
# Insert the path to your datasets here
directory <- stringr::str_remove(Sys.getenv("PWD"), "code")
setwd(directory)

output_height <- 7
output_width <- 12
output_res <- 400

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

#==============================================================================#
factor2numeric <- function(x) {
  return(as.numeric(as.character(x)))
}

#==============================================================================#
                                  # DATA IMPORT #
#==============================================================================#
tran_df <- readr::read_rds("./data/cash_transactions_df.rds") %>%
  dplyr::rename(uasid = id)

load(stringr::str_c("./data/baseline-results_", last_base_run, ".RData"))
load(
  stringr::str_c(
    "./data/extension-policy-results_", last_counter_run, ".RData"
  )
)
load(
  stringr::str_c(
    "./data/extensions-counterfactual-results_", last_counter_run, ".RData"
  )
)

ext_asymmetric_pol_df %>%
  dplyr::mutate(Delta_alpha = round(x = Delta_alpha, digits = 2)) %>%
  dplyr::filter(Delta_alpha > 0.09) %>%
  dplyr::select(dplyr::contains("alpha")) %>%
  print()

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
  mutate(model = "With pennies and nickels (current)")

shares_figure_df <- 1:2 %>%
  as.list() %>%
  purrr::map(
    .x = .,
    .f = function(i) {
      df <- list(ext_symmetric_pol_df, ext_asymmetric_pol_df)[[i]]
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
  mutate(model = "Without pennies and nickels (counterfactual)") %>%
  bind_rows(
    x = .,
    y = baseline_figures_df
  )

#==============================================================================#
                                  # FIGURES #
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
    model == "Without pennies and nickels (counterfactual)" &
    which_policy == "Asymmetric policy"
  ) %>%
  dplyr::select(share, n_tokens, which_notes, which_policy) %>%
  print()

shares_figure_data %>%
  dplyr::filter(
    which_notes == "All coins and notes" &
    model == "Without pennies and nickels (counterfactual)" &
    which_policy == "Symmetric policy"
  ) %>%
  dplyr::select(share, n_tokens, which_notes, which_policy) %>%
  print()

shares_figure_data %>%
  dplyr::filter(
    which_notes == "Only $20 note" &
    model == "Without pennies and nickels (counterfactual)" &
    which_policy == "Asymmetric policy"
  ) %>%
  dplyr::select(share, n_tokens, which_notes, which_policy)  %>%
  print()

shares_figure_data %>%
  dplyr::filter(
    which_notes == "Only $20 note" &
    model == "Without pennies and nickels (counterfactual)" &
    which_policy == "Symmetric policy"
  ) %>%
  dplyr::select(share, n_tokens, which_notes, which_policy)  %>%
  print()

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
    limits = c(0, 28.5),
    breaks = seq(0, 30, 5),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Without pennies and nickels (counterfactual)" = "#b5c7d1",
      "With pennies and nickels (current)" = "#0e4768"
    )
  ) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Without pennies and nickels (counterfactual)" = "#b5c7d1",
      "With pennies and nickels (current)" = "#0e4768"
    )
  ) +
  scale_alpha_manual(
    name = NULL,
    values = c(
      "Without pennies and nickels (counterfactual)" = 1,
      "With pennies and nickels (current)" = 0
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
  filename = "./figures/extension-counterfactual-burden_2015-2019.jpeg",
  device = "jpeg",
  plot = counter_shares_figure,
  height = output_height,
  width = output_width
)

#==============================================================================#
all_rounding_vals <- tibble::tibble(amnt = as.character(seq(-0.04, 0.09, 0.01)))

price_effects_tbl <- list(ext_symmetric_pol_df, ext_asymmetric_pol_df) %>%
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
          share = round(x = share * 100, digits = 2),
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

tex_table <- price_effects_tbl %>%
  purrr::reduce(
    .x = .,
    .f = full_join,
    by = "amnt",
    suffix = c("_sym", "_asym")
    ) %>%
  t() %>%
  data.frame() %>%
  dplyr::mutate_all(.funs = as.numeric)

print(str(tex_table))
tex_table %>%
  xtable() %>%
  print(include.rownames = FALSE)
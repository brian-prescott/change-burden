#==============================================================================#
# PREAMBLE #
#==============================================================================#
# Written by: Brian Prescott
# Last edited: 2022-07-15

# Purpose: This is a new script which is constructing the stylized exercise
# results with an additional result. The additional result is the inclusion of
# the nickel in our counterfactual results for the paper
# "Cash Payments and the Penny Policy Debate" by Brian Prescott and Oz Shy.

# Cleaning out the environment before running the script
rm(list = ls())

last_base_run <- "2022-07-16"
last_counter_run <- "2022-07-16"
last_exten_run <- "2022-07-16"

#==============================================================================#
# LIBRARIES #
#==============================================================================#
library(tidyverse)
library(grDevices)
library(graphics)

#==============================================================================#
# VARIABLES #
#==============================================================================#
# Insert the path to your datasets here.
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
    "./data/extensions-counterfactual-results_", last_exten_run, ".RData"
  )
)
load(
  stringr::str_c(
    "./data/counterfactual-results_", last_counter_run, ".RData"
  )
)

#==============================================================================#
# FIGURE GENERATION #
#==============================================================================#
stylized_baseline <- stylized_exercise_1 %>%
  ggplot(data = ., aes(x = a, y = n_tokens)) +
  geom_line(
    size = 1,
    lty = 3,
    color = "#0e4768"
  ) +
  geom_point(size = 2, color = "#0e4768") +
  scale_x_continuous(
    limits = c(4, 5),
    breaks = seq(4, 5, 0.05)
  ) +
  scale_y_continuous(
    limits = c(1, 6),
    breaks = seq(0, 6, 1)
  ) +
  theme_bw() +
  xlab("") +
  ylab("Optimal burden") +
  ggtitle("Economy with pennies (current)") +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(size = 18, hjust = 0.50),
    axis.text = element_text(size = 16, color = "black"),
    axis.title = element_text(
      size = 17,
      color = "black",
      family = "serif"
    ),
    panel.grid.minor = element_blank()
  )

stylized_counter_2 <- stylized_exercise_2 %>%
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
    color = "#0e4768"
  ) +
  geom_point(size = 2, color = "#0e4768") +
  scale_x_continuous(
    limits = c(4, 5),
    breaks = seq(4, 5, 0.05)
  ) +
  scale_y_continuous(
    limits = c(1, 6),
    breaks = seq(0, 6, 1)
  ) +
  theme_bw() +
  ggtitle("Economy without pennies (counterfactual)") +
  xlab("Amount") +
  ylab("Optimal burden") +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(size = 18, hjust = 0.50),
    axis.text = element_text(size = 16, color = "black"),
    axis.title = element_text(
      size = 17,
      color = "black",
      family = "serif"
    ),
    panel.grid.minor = element_blank()
  )

stylized_counter_3 <- stylized_exercise_3 %>%
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
    color = "#0e4768"
  ) +
  geom_point(size = 2, color = "#0e4768") +
  scale_x_continuous(
    limits = c(4, 5),
    breaks = seq(4, 5, 0.1)
  ) +
  scale_y_continuous(
    limits = c(1, 6),
    breaks = seq(0, 6, 1)
  ) +
  theme_bw() +
  ggtitle("Economy without pennies and nickels (counterfactual)") +
  xlab("Amount") +
  ylab("Optimal burden") +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(size = 18, hjust = 0.50),
    axis.text = element_text(size = 16, color = "black"),
    axis.title = element_text(
      size = 17,
      color = "black",
      family = "serif"
    ),
    panel.grid.minor = element_blank()
  )

multiplot(
  stylized_baseline,
  stylized_counter_2,
  stylized_counter_3,
  cols = 1
)
dev.copy(
  jpeg,
  "./figures/extension-optimal-tokens.jpeg",
  height = output_height,
  width = output_width,
  units = "in",
  res = output_res
)
multiplot(
  stylized_baseline,
  stylized_counter_2,
  stylized_counter_3,
  cols = 1
)
dev.off()
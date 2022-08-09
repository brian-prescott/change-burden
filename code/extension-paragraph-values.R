#==============================================================================#
                                # PREAMBLE #
#==============================================================================#
# Written by: Brian Prescott
# Last edited: 2022-07-15

# Purpose: This script will generate the values reported in the article
# "Cash Payments and the Penny Policy Debate" by Brian Prescott and Oz Shy.

# Clearing out the environment
rm(list = ls())

# When was the last counterfactual run?
last_counter_run <- "2022-07-16"

#==============================================================================#
                                # PACKAGES #
#==============================================================================#
library(tidyverse)
library(zoo)

#==============================================================================#
                                # FUNCTION #
#==============================================================================#
cat_n <- function(x) {
    return(cat(x, "\n"))
}

#==============================================================================#
                                # VARIABLES #
#==============================================================================#
directory <- paste0(stringr::str_remove(Sys.getenv("PWD"), "code"), "/data")
setwd(directory)

#==============================================================================#
                                # DATA IMPORT #
#==============================================================================#
load(file = str_c("extension-policy-results_", last_counter_run, ".RData"))
load(file = str_c("policy-results_", last_counter_run, ".RData"))

# We currently have that eliminating both the penny and nickel does not lower
# the expected change burden over the full sample. This result is a bit
# puzzling. I should figure out which transactions results in a higher burden
# in the extension counterfactual relative to the original.

cat_n("Estimate and difference in symmetric counterfactuals")
ext_symmetric_pol_df %>%
    dplyr::filter(only_20note == "All coins and notes") %>%
    dplyr::select(n_tokens_counter) %>%
    unlist() %>%
    mean(x = ., na.rm = TRUE) %>%
    print()
print(
    mean(
        x = subset(
            ext_symmetric_pol_df, only_20note == "All coins and notes"
        )$n_tokens_counter,
        na.rm = TRUE
    ) -
    mean(
        x = subset(
            symmetric_pol_df, only_20note == "All coins and notes"
        )$n_tokens_counter, 
        na.rm = TRUE
    )
)
cat_n("Estimate and difference in asymmetric counterfactuals")
ext_asymmetric_pol_df %>%
    dplyr::filter(only_20note == "All coins and notes") %>%
    dplyr::select(n_tokens_counter) %>%
    unlist() %>%
    mean(x = ., na.rm = TRUE) %>%
    print()
print(
    mean(
        x = subset(
            ext_asymmetric_pol_df, only_20note != "All coins and notes"
        )$n_tokens_counter,
        na.rm = TRUE
    ) -
    mean(
        x = subset(
            asymmetric_pol_df, only_20note != "All coins and notes"
        )$n_tokens_counter,
        na.rm = TRUE
    )
)

# Now, we are dropping the original counterfactual values
rm(symmetric_pol_df)
rm(asymmetric_pol_df)

symmetric_pol_df <- ext_symmetric_pol_df
asymmetric_pol_df <- ext_asymmetric_pol_df

#==============================================================================#
                                # BASELINE CALCULATIONS #
#==============================================================================#
# Section: Quantifying the burden of holding change
using_df_section1 <- symmetric_pol_df %>%
    dplyr::filter(only_20note == "All coins and notes")
df_4_2 <- using_df_section1$n_tokens %>%
    round(x = ., digits = 2) %>%
    table() %>%
    prop.table() %>%
    data.frame() %>%
    dplyr::rename(
        burden = ".",
        share = Freq
    ) %>%
    dplyr::mutate(burden = as.numeric(as.character(burden))) %>%
    dplyr::filter(burden <= 10) %>%
    dplyr::mutate(burden = factor(x = burden))

## Total share of transactions
dplyr::summarize(df_4_2, sum_burden = sum(share, na.rm = TRUE))

## Burden == 2 # nolint
df_4_2$share[2] %>%
    round(x = ., digits = 2)

## Sum of 1 <= b <= 2
sum(df_4_2$share[1:2])

## Expected change burden
cat_n("Expected change burden:")
using_df_section1$n_tokens %>%
    round(x = ., digits = 2) %>%
    mean(x = ., na.rm = TRUE) %>%
    print()

cat_n("Expected change burden when a > 100:")
subset(using_df_section1, alpha > 100)$n_tokens %>%
    mean(x = ., na.rm = TRUE)

## Median of a > 100 transactions
using_df_section1 %>%
    dplyr::filter(alpha > 100) %>%
    dplyr::select(n_tokens) %>%
    dplyr::summarize(
        mean = mean(x = n_tokens, na.rm = TRUE),
        median = median(x = n_tokens, na.rm = TRUE)
    ) %>%
    data.frame()

## Difference in maximums
max(subset(using_df_section1, alpha > 100)$n_tokens, na.rm = TRUE) -
    max(subset(using_df_section1, alpha <= 100)$n_tokens, na.rm = TRUE)

#==============================================================================#
# Section: Quantifying the burden with restricted consumer choice
using_df_section2 <- asymmetric_pol_df %>%
    dplyr::filter(only_20note == "Only $20 note")
df_4_3 <- using_df_section2$n_tokens %>%
    round(x = ., digits = 2) %>%
    table() %>%
    prop.table() %>%
    data.frame() %>%
    dplyr::rename(
        burden = ".",
        share = Freq
    ) %>%
    dplyr::mutate(burden = as.numeric(as.character(burden))) %>%
    dplyr::filter(burden <= 10) %>%
    dplyr::mutate(burden = factor(x = burden))
## Total share of transactions
df_4_3 %>%
    dplyr::summarize(sum_burden = sum(share, na.rm = TRUE))

## Expected change burden
mean(round(using_df_section2$n_tokens, digits = 2), na.rm = TRUE)

# Percent change
(mean(round(using_df_section2$n_tokens, digits = 2), na.rm = TRUE) -
 mean(round(using_df_section1$n_tokens, digits = 2), na.rm = TRUE) ) /
    mean(round(using_df_section1$n_tokens, digits = 2), na.rm = TRUE) * 100

## Burden == 2 percent change # nolint
(df_4_3$share[2] - df_4_2$share[2]) / df_4_2$share[2] * 100
# From
df_4_2$share[2]
# To
df_4_3$share[2]

# Change in sum of shares over interval [1, 2]
sum(df_4_2$share[1:2])
sum(df_4_3$share[1:2])

## Median of a > 100 transactions
using_df_section2 %>%
    dplyr::filter(alpha > 100) %>%
    dplyr::select(n_tokens) %>%
    dplyr::summarize(
        mean = mean(x = n_tokens, na.rm = TRUE),
        median = median(x = n_tokens, na.rm = TRUE)
    ) %>%
    data.frame()

## Difference in maximums
max(subset(using_df_section2, alpha > 100)$n_tokens, na.rm = TRUE) -
    max(subset(using_df_section2, alpha <= 100)$n_tokens, na.rm = TRUE)

dplyr::inner_join(
    x = df_4_2,
    y = df_4_3,
    by = c("burden"), suffix = c("_unrest", "_rest")
) %>%
    dplyr::mutate(
        Delta = share_rest - share_unrest
    ) %>%
    dplyr::filter(Delta == max(Delta)) %>%
    dplyr::mutate(perc_change = (share_rest - share_unrest) / share_unrest)

#==============================================================================#
                                # COUNTERFACTUAL CALCULATIONS #
#==============================================================================#
using_df_section3 <- symmetric_pol_df %>%
    dplyr::filter(only_20note == "All coins and notes")
df_5_1 <- using_df_section3$n_tokens_counter %>%
    round(x = ., digits = 2) %>%
    table() %>%
    prop.table() %>%
    data.frame() %>%
    dplyr::rename(
        burden = ".",
        share = Freq
    ) %>%
    dplyr::mutate(burden = as.numeric(as.character(burden))) %>%
    dplyr::filter(burden <= 10) %>%
    dplyr::mutate(burden = factor(x = burden))
    
## Total share of transactions
df_5_1 %>%
    dplyr::summarize(sum_burden = sum(share, na.rm = TRUE))

df_5_1 %>%
    dplyr::mutate_all(.funs = ~ as.numeric(as.character(.))) %>%
    dplyr::mutate_all(.funs = ~ round(x = ., digits = 4))

# NEW MAXIMUM BURDEN IS b^* = 5

# Changes and percent changes
dplyr::inner_join(
    x = df_4_2,
    y = df_5_1,
    by = c("burden"), suffix = c("_base", "_counter")
) %>%
    dplyr::mutate(
        Delta = share_counter - share_base
    ) %>%
    dplyr::mutate(perc_change = (share_counter - share_base) / share_base)

sum(df_5_1$share[1:4] - df_4_2$share[1:4]) / sum(df_4_2$share[1:4])
sum(df_5_1$share[5:10] - df_4_2$share[5:10]) / sum(df_4_2$share[5:10])

cat_n("New expected burden, difference, and growth rate")
mean(using_df_section3$n_tokens_counter, na.rm = TRUE)

mean(using_df_section3$n_tokens_counter, na.rm = TRUE) -
    mean(using_df_section1$n_tokens, na.rm = TRUE)

# Percent change in expected cost of cash exchange
(mean(using_df_section3$n_tokens_counter, na.rm = TRUE) -
    mean(using_df_section1$n_tokens, na.rm = TRUE)) /
    mean(using_df_section1$n_tokens, na.rm = TRUE)

using_df_section4 <- asymmetric_pol_df %>%
    dplyr::filter(only_20note == "All coins and notes")
df_5_1_2 <- using_df_section4$n_tokens_counter %>%
    round(x = ., digits = 2) %>%
    table() %>%
    prop.table() %>%
    data.frame() %>%
    dplyr::rename(
        burden = ".",
        share = Freq
    ) %>%
    dplyr::mutate(burden = as.numeric(as.character(burden))) %>%
    dplyr::filter(burden <= 10) %>%
    dplyr::mutate(burden = factor(x = burden))

cat_n("New expected burden, difference, and growth rate for asym")
mean(using_df_section4$n_tokens_counter, na.rm = TRUE)
mean(using_df_section4$n_tokens_counter, na.rm = TRUE) -
    mean(using_df_section1$n_tokens, na.rm = TRUE)
(mean(using_df_section4$n_tokens_counter, na.rm = TRUE) -
    mean(using_df_section1$n_tokens, na.rm = TRUE)) /
    mean(using_df_section1$n_tokens, na.rm = TRUE)

round(df_5_1$share[3] * 100, digits = 1)
round(df_5_1_2$share[3] * 100, digits = 1)

round(sum(df_5_1$share[1:4]) * 100, digits = 2)
round(sum(df_5_1_2$share[1:4]) * 100, digits = 2)

#==============================================================================#
using_df_section5 <- symmetric_pol_df %>%
    dplyr::filter(only_20note == "Only $20 note")
df_5_2 <- using_df_section5$n_tokens_counter %>%
    round(x = ., digits = 2) %>%
    table() %>%
    prop.table() %>%
    data.frame() %>%
    dplyr::rename(
        burden = ".",
        share = Freq
    ) %>%
    dplyr::mutate(burden = as.numeric(as.character(burden))) %>%
    dplyr::filter(burden <= 10) %>%
    dplyr::mutate(burden = factor(x = burden))

cat_n("New expected burden, difference, and growth rate")
mean(using_df_section5$n_tokens_counter)
mean(using_df_section5$n_tokens_counter) - mean(using_df_section2$n_tokens)
(mean(using_df_section5$n_tokens_counter) - mean(using_df_section2$n_tokens)) /
    mean(using_df_section2$n_tokens)
dplyr::inner_join(
    x = df_4_3,
    y = df_5_2,
    by = c("burden"), suffix = c("_base", "_counter")
) %>%
    dplyr::mutate(
        Delta = share_counter - share_base
    ) %>%
    dplyr::mutate(perc_change = (share_counter - share_base) / share_base)

dplyr::inner_join(
    x = df_4_3,
    y = df_5_2,
    by = c("burden"), suffix = c("_base", "_counter")
) %>%
    dplyr::mutate(burden = as.numeric(as.character(burden))) %>%
    dplyr::filter(burden >= 7) %>%
    dplyr::summarize_all(.funs = sum) %>%
    dplyr::select(-burden) %>%
    dplyr::mutate(
        Delta = share_counter - share_base
    ) %>%
    dplyr::mutate(perc_change = (share_counter - share_base) / share_base)

#==============================================================================#
using_df_section6 <- asymmetric_pol_df %>%
    dplyr::filter(only_20note == "Only $20 note")
df_5_2_2 <- using_df_section6$n_tokens_counter %>%
    round(x = ., digits = 2) %>%
    table() %>%
    prop.table() %>%
    data.frame() %>%
    dplyr::rename(
        burden = ".",
        share = Freq
    ) %>%
    dplyr::mutate(burden = as.numeric(as.character(burden))) %>%
    dplyr::filter(burden <= 10) %>%
    dplyr::mutate(burden = factor(x = burden))

cat_n("New expected burden, difference, and growth rate for asym")
mean(using_df_section6$n_tokens_counter)
mean(using_df_section6$n_tokens_counter) - mean(using_df_section2$n_tokens)
(mean(using_df_section6$n_tokens_counter) - mean(using_df_section2$n_tokens)) /
    mean(using_df_section2$n_tokens)

dplyr::inner_join(
    x = df_4_3,
    y = df_5_2_2,
    by = c("burden"), suffix = c("_base", "_counter")
) %>%
    dplyr::mutate(
        Delta = share_counter - share_base
    ) %>%
    dplyr::mutate(perc_change = (share_counter - share_base) / share_base)
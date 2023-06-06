# rtables workshop R in Medicine
# June 6, 2023
# Gabriel Becker and Adrian Waddell


# Install rtables packages and dependencies ----
# install.packages(c("rtables", "remotes"))
# remotes::install_github("insightsengineering/rtables") # for workshop


# Getting started ----

# Data
library(rtables)

# synthetic adsl data
head(ex_adsl)
names(ex_adsl)

# R base table function
table(ex_adsl$ARM)
table(ex_adsl$SEX, ex_adsl$ARM)

# Now with qtable in rtables

library(rtables)
qtable(ex_adsl, "ARM")
qtable(ex_adsl, "SEX", "ARM")

# nested frequency tables
qtable(ex_adsl, c("COUNTRY", "SEX"), "ARM")

# using a aggregation function other than count
qtable(ex_adsl, c("COUNTRY", "SEX"), "ARM", avar = "AGE", afun = mean)

# multiple analysis values
fivenum2 <- \(x) setNames(as.list(fivenum(x)), c("min", "Q1", "MED", "Q3", "max"))
qtable(ex_adsl, "SEX", "ARM", avar = "AGE", afun = fivenum2)

# now using rtable's core way of defining a table

lyt <- basic_table() |>
  split_cols_by("ARM") |>
  split_rows_by("SEX") |>
  analyze("AGE", afun = fivenum2, format = "xx.xx")

build_table(lyt, ex_adsl)


# Basics of rtables ----

lyt <- basic_table() |>
  split_cols_by("ARM") |>
  analyze("AGE", mean, format = "xx.xx")

tbl <- build_table(lyt, ex_adsl)

print(tbl)


#  Deriving cell values ----

lyt <- basic_table() |>
  analyze("AGE")

build_table(lyt, ex_adsl)


# in_rows use and formatting options
fivenum_afun <- function(x) {
  in_rows(n = sum(!is.na(x)),
          "mean (sd)" = c(mean(x), sd(x)),
          median = median(x),
          "min - max" = range(x),
          .formats = c(n = "xx",
                       "mean (sd)" = "xx.x (xx.x)",
                       median = "xx.x",
                       "min - max" = "xx.x - xx.x"))
}

lyt2 <- basic_table() %>% analyze("AGE", fivenum_afun)

build_table(lyt2, ex_adsl)


# Cell Value Formatting  ----

rcell(1.234567, "xx.xx")
rcell(c(1, 0.1), "xx.xx (xx.xx%)")

formatters::list_valid_format_labels()

example(table_shell)



# Percentages ----
pct_afun <- function(x, .N_col) {
  rcell(
    sum(!is.na(x)) * c(1, 1/.N_col),
    format = "xx (xx.x%)"
  )
}

lyt <- basic_table() |>
  analyze("AGE", pct_afun)

build_table(lyt, ex_adsl)


# Faceting ----

## Column Faceting
library(ggplot2)
ggplot(ex_adsl, mapping = aes(x = AGE)) +
  geom_boxplot() +
  facet_grid(cols = vars(ARM))

lyt <- basic_table() |>
  split_cols_by("ARM") |>
  analyze("AGE", range, format = "xx.xx - xx.xx")

build_table(lyt, ex_adsl)

## Row Faceting
ggplot(ex_adsl, mapping = aes(x = AGE)) +
  geom_boxplot() +
  facet_grid(rows = vars(SEX))

lyt2 <- basic_table() |>
  split_rows_by("SEX") |>
  analyze("AGE", range,
          format = "xx.xx - xx.xx")

build_table(lyt2, ex_adsl)

## Grid Faceting

ggplot(ex_adsl, mapping = aes(x = AGE)) +
  geom_boxplot() +
  facet_grid(rows = vars(SEX),
             cols = vars(ARM))

lyt3 <- basic_table() |>
  split_cols_by("ARM") |>
  split_rows_by("SEX") |>
  analyze("AGE", range, format = "xx.xx - xx.xx")

build_table(lyt3, ex_adsl)

# Nested Splits ----
library(dplyr)

ex_adsl2 <- ex_adsl |>
  filter(SEX %in% c("M", "F"))

ggplot(ex_adsl2, mapping = aes(x = AGE)) +
  geom_boxplot() +
  facet_grid(cols = vars(ARM, SEX))

lyt <- basic_table() |>
  split_cols_by("ARM") |>
  split_cols_by("SEX", split_fun = drop_split_levels) |>
  analyze("AGE", mean, format = "xx.xx")

tbl <- build_table(lyt, ex_adsl2)

print(tbl)


# Group Summaries
ex_adsl3 <- ex_adsl |>
  mutate(B1HL = factor(ifelse(BMRKR1 > mean(BMRKR1), "H", "L"), levels = c("L", "H")))

lyt <- basic_table() |>
  split_cols_by("ARM") |>
  split_rows_by("SEX") |>
  split_rows_by("B1HL") |>
  analyze("AGE", \(x) list(B = "a"))

build_table(lyt, ex_adsl3)

lyt <- basic_table() |>
  split_cols_by("ARM") |>
  summarize_row_groups() |>
  split_rows_by("SEX") |>
  summarize_row_groups() |>
  split_rows_by("B1HL") |>
  summarize_row_groups() |>
  analyze("AGE", \(x) list(B = "a"))

build_table(lyt, ex_adsl3)


# Group Summaries in qtable ----
qtable(ex_adsl3, c("SEX", "B1HL"), "ARM", avar = "AGE", afun = \(x) list(B = "a"))

qtable(ex_adsl3, c("SEX", "B1HL"), "ARM", avar = "AGE", afun = \(x) list(B = "a"), summarize_groups = TRUE)

# Complex Tables ----
# code can be found here:
# https://github.com/insightsengineering/adv_rtables_training/blob/main/training2.Rmd#L627


# Analysis Reporting Datasets (ARDs) ----

## lifted from tests. Complex multi-featured table and corresponding data

makefakedat <- function(n  = 1000) {
    datadf <- data.frame(stringsAsFactors = FALSE,
                        ARM = c("ARM1", sample(c("ARM1", "ARM2"), n - 1, replace = TRUE)),
                        SEX = c("M", sample(c("M", "F"), n - 1, replace = TRUE)),
                        FACTOR2 = c("A", sample(c("A", "B", "C"), n - 1, replace = TRUE)),
                        RACE = c("WHITE", sample(c("WHITE", "BLACK"), n - 1, replace = TRUE)),
                        AGE = runif(n, 40, 70),
                        VAR3 = c("level1", sample(c("level1", "level2"), n - 1,
                                                  replace = TRUE)))

    datadf$ethn_label <- c(WHITE = "Caucasian", BLACK = "African American")[datadf$RACE]
    datadf$fac2_label <- paste("Level", datadf$FACTOR2)
    datadf$gend_label <- c(M = "Male", F = "Female")[datadf$SEX]
    datadf
}


set.seed(0)
rawdat <- makefakedat()

lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    ## add nested column split on SEX with value lables from gend_label
    split_cols_by("SEX", "Gender", labels_var = "gend_label") %>%
    ## No row splits have been introduced, so this adds
    ## a root split and puts summary content on it labelled Overall (N)
    ## add_colby_total(label = "All") %>%
    ##    summarize_row_groups(label = "Overall (N)", format = "(N=xx)") %>%
    ## add a new subtable that splits on RACE, value labels from ethn_label
    split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label", label_pos = "hidden") %>%
    summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
    ##
    ## Add nested row split within Race categories for FACTOR2
    ## using a split function that excludes level C
    ## value labels from fac2_label
    split_rows_by("FACTOR2", "Factor2",
                  split_fun = remove_split_levels("C"),
                  labels_var = "fac2_label",
                  label_pos = "hidden") %>%
    ## Add count summary within FACTOR2 categories
    summarize_row_groups("FACTOR2") %>%
    ## Add analysis/data rows by analyzing AGE variable
    ## Note afun is a function that returns 2 values in a named list
    ## this will create 2 data rows
    analyze("AGE", "Age Analysis",
            afun = function(x) list(mean = mean(x),
                                    median = median(x)),
            format = "xx.xx") %>%
    ## adding more analysis vars "compounds them", placing them at the same
    ## level of nesting as all previous analysis blocks, rather than
    ## attempting to further nest them
    analyze("AGE",
            "Age Analysis redux",
            afun = range,
            format = "xx.x - xx.x",
            table_names = "AgeRedux") %>%

    ## Note nested=FALSE, this creates a NEW subtable directly under the
    ## root split
    ## afun of table() gives us k count rows, where k is the number of
    ## levels of VAR3, in this case 2.
    analyze("VAR3", "Var3 Counts", afun = list_wrap_x(table), nested = FALSE)

tbl <- build_table(lyt, rawdat)

print(tbl)

ard <- as_ard(tbl)

print(ard, width = 1000)

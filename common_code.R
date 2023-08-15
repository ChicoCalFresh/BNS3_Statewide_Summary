
# Packages
library(tidyverse) #ggplot2, forcats, dplyr
library(knitr)
library(sjPlot)
library(kableExtra)
library(questionr)
library(gridExtra)
library(ggpubr)
library(scales)


# Global options
opts_chunk$set(echo = FALSE, warning=FALSE, message=FALSE, cache=FALSE, 
               fig.height=4, fig.width=6, fig.align = 'center')
sjPlot::set_theme(base = theme_bw())

# Define color palettes
# CHC branded colors
# blue 1 587db6, blue 2 5b81a3, green b5d43b, yellow ecf0b7

likert.pal.5 <- brewer_pal("div")(5)
likert.pal.5[3] <- "#b2b5a8"

likert.pal.3 <- brewer_pal("div")(3)
likert.pal.3[2] <- "#b2b5a8"

likert.pal.6 <- brewer_pal("div")(6)
likert.pal.6[3] <- "#b2b5a8"

chc_blue <- "#336699"


# Helper functions


## Display text answers without blank lines
display.text <- function(data, var){
  data %>% filter(!!as.symbol(var)!="") %>% select(!!as.symbol(var)) %>%
    kable(col.names=NULL) %>% 
    kable_styling(full_width = FALSE, 
                  bootstrap_options =c("striped", "responsive", "hover", "condensed"))
}



## Print number of respondents and percent they compose (non-missing) - use this for general bns questions.
## Data frame defaults to bns, y argument can be used to specify a different data frame
print_n_reporting <- function(x) { 
  paste0("(n=", 
         sum(!is.na(bns[[x]])), ", ", 
         percent(mean(!is.na(bns[[x]])), accuracy=1), " of ", nrow(bns), " reporting)."
  )
}

## Print number of respondents and percent they compose (non-missing) - use this for tmp data frames.
print_n_reporting_tmp <- function(x) {
  paste0("(n=", 
         sum(!is.na(tmp[[x]])), ", ", 
         percent(sum(!is.na(tmp[[x]]))/nrow(bns), accuracy=1), " of ", nrow(bns), " reporting)."
  )
}

## Print number of respondents and percent they compose (non-missing) - use this for tmp2 data frames.
print_n_reporting_tmp2 <- function(x) {
  paste0("(n=", 
         sum(!is.na(tmp2[[x]])), ", ", 
         percent(sum(!is.na(tmp2[[x]]))/nrow(bns), accuracy=1), " of ", nrow(bns), " reporting)."
  )
}


## Get percent for variable for given value (ex: Housing - Sleeping Places)
get_perct <- function(x, value) {
  paste0(sum(x==value, na.rm=TRUE), "/", sum(!is.na(x)),
         " (", percent(mean(x==value, na.rm = TRUE), accuracy=.1), ")")
}


## Create table of percentages for single multiple choice question (ex: Housing - Current Housing Situation)
question_table <- function(question, values, cnames) {
  temp_df <- data.frame(qs=character(), prc=character(), frq=numeric())
  for (idx in 1:length(values)) {
    temp_df[idx, 1] <- values[idx]
    temp_df[idx, 2] <- get_perct(bns[[question]], values[idx])
    temp_df[idx, 3] <- sum(bns[[question]] == values[idx], na.rm = TRUE)
  }
  temp_df <- temp_df %>% arrange(desc(frq)) %>% select(-frq)
  colnames(temp_df) <- cnames
  temp_df %>% kable() %>% kable_styling(bootstrap_options = "striped") %>% column_spec(2, width='3.5cm')
}


# Binary Indicators ----

## prepare multiple binary variables to be displayed as N(%) in a table

## Show percentage of students who selected A GIVEN VALUE (e.g. "Yes") across multiple binary variables (ex: Student Demographics - Identifiers)
binary_table <- function(var, value, row.names, punc = ".") {
  tmp <- as.data.frame(t(bns[var]))
  tmp2 <- data.frame(Freq=apply(tmp, 1, function(x, value) sum(x == value, na.rm=TRUE), value))
  n.s <- apply(tmp, 1, function(x) sum(!is.na(x))) # get this
  tmp2$label <- paste0(tmp2$Freq, " (", unname(percent(tmp2$Freq/n.s, accuracy=.1)), ")")
  rownames(tmp2) <- paste0(rnames, " (n = ", n.s, ")", punc) 
  tmp2 <- tmp2 %>% arrange(desc(Freq)) %>% select(-Freq)
  colnames(tmp2) <- "Yes (%)"
  tmp2 %>% kable() %>% kable_styling(bootstrap_options = "striped") %>% column_spec(2, width='3.5cm')
}

## Show percentage of students who selected YES == 1 across multiple binary variables (ex: Personal Demographics - Ethnicity)
prep_binary_vars <- function(question, xlabels, punc = " ") {
  bns %>%  summarize(across(contains(question),
                            list(x = ~ sum(.x, na.rm=TRUE),  # count how many 1's
                                 n = ~ sum(!is.na(.x))), # count how many non-missing values
                            .names = "{.fn}_{.col}")) %>% # specify the new variable names according to which function it's using
    pivot_longer(everything()) %>% # rshape to long format to have one variable for name, and one for value
    mutate(str = substr(name, 1, 1), # extract x and n from the 'name', 
           #name = gsub(paste0("x_", question, "_|n_", question, "_"), "", name)) %>% # clean variable name (keep actual variable in context)
           name = gsub("x_|n_", "", name)) %>% 
    pivot_wider(id_cols = name, values_from = value, names_from = str) %>% # pivot back wide so one column for x and one for n
    mutate(pct = x/n, # calculate percent and label
           pct_lab = paste0(x, "\n(", percent(pct, accuracy=.1),")"), 
           xlab = paste0(xlabels, " (n = ", n, ")", punc), 
           xlab = fct_reorder(xlab, desc(x))) %>% arrange(desc(x))
}




## Plot columns from multiple questions (plots the value from binary table iver)(ex: Food Secutiry - More Eating Situations)
binary_plot <- function(var, value, rnames) {
  tmp <- as.data.frame(t(bns[var]))
  tmp2 <- data.frame(Freq=apply(tmp, 1, function(x) sum(ifelse(x == value, 1, 0), na.rm = TRUE)))
  tmp2$labels <- rnames
  tmp2 <- tmp2 %>% arrange(desc(Freq))
  ggplot(tmp2, aes(x=reorder(labels, -Freq), y=Freq, label=Freq)) + geom_col(fill=primary.chc.col) +
    geom_text(aes(y=Freq+4)) + 
    scale_x_discrete(labels=label_wrap(28)) + ylab('') + xlab('')
}


# Likert Helpers ----

# Number of students who Agree and Strongly agree likert questions
likert_n_positive <- function(x) {
  unname(table(x)["Agree"]) + unname(table(x)["Strongly agree"]) 
}


# Number of students who disagree, strongly disagree, and neither agree nor disagree
likert_n_not_positive <- function(x) {
  unname(table(x)["Disagree"]) + unname(table(x)["Strongly disagree"]) + unname(table(x)["Neither agree nor disagree"])
}


# Percent of students who Agree and Strongly agree likert questions
likert_percent_positive <- function(x) {
  percent(unname(prop.table(table(x))["Agree"]) + unname(prop.table(table(x))["Strongly agree"]), .1)
}


# Percent of students who disagree, strongly disagree, and neither agree nor disagree
likert_percent_not_positive <- function(x) {
  percent(unname(prop.table(table(x))["Disagree"]) + unname(prop.table(table(x))["Strongly disagree"]) + unname(prop.table(table(x))["Neither agree nor disagree"]), .1)
}


# Confident Percent Positive
confident_scale_percent_negative <- function(x) {
  percent(unname(prop.table(table(x))["Not at all confident"]) + unname(prop.table(table(x))["Not very confident"]) + unname(prop.table(table(x))["Neutral"]), .1)
}


# Load data ----
bns <- readRDS(here::here("../../02. Data Analysis/BNS3-statewide/data", "bns3_statewide_clean.rds")) |> filter(!is.na(school))












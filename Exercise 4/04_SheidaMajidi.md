---
author: Sheida Majidi
date: 2024-04-06
output:
  html_document: default
  word_document: default
title: Exercise 4 - Relationship Between Examiner Centrality and
  Application Processing Time
---

# Introduction

In this exercise, I want to present my analysis on the relationship
between patent examiner centrality within the advice network and the
processing time of patent applications.

\`\`\`{r setup, include=FALSE} knitr::opts_chunk\$set(echo = TRUE)
library(tidyverse) library(lubridate) library(gender) library(wru)
library(igraph) library(ggraph) library(gridExtra)


    # Data loading and preprocessing

    First, I load the data and prepare it for the analysis:


    ```{r data}
    applications = read_csv("/Users/sheidamajidi/Desktop/Winter2024/Winter2024-2/ORGB672/Exercises/Exercise 4/app_data_starter.csv", show_col_types = FALSE)
    edges = read_csv("/Users/sheidamajidi/Desktop/Winter2024/Winter2024-2/ORGB672/Exercises/Exercise 4/edges_sample.csv",show_col_types = FALSE)

# Data Preprocessing

## Estimating examiner demographics

\`\`\`{r examiner-demographic-estimation, echo=FALSE}

# Gender estimation

examiner_names=applications %\>% distinct(examiner_name_first)

# Predict gender based on first names

examiner_names_gender=examiner_names %\>% do(results =
gender(.\$examiner_name_first, method = "ssa")) %\>% unnest(cols =
c(results), keep_empty = TRUE) %\>% select(examiner_name_first = name,
gender, proportion_female)

# Join gender data back to the main applications dataset

applications=applications %\>% left_join(examiner_names_gender, by =
"examiner_name_first")

# Race estimation

examiner_surnames=applications %\>% select(surname = examiner_name_last)
%\>% distinct()

# Predict race based on last names

examiner_race=predict_race(voter.file = examiner_surnames, surname.only
= T) %\>% as_tibble()

# Select the race with the highest probability for each last name

examiner_race=examiner_race %\>% mutate(max_race_p = pmax(pred.asi,
pred.bla, pred.his, pred.oth, pred.whi)) %\>% mutate(race = case_when(
max_race_p == pred.asi \~ "Asian", max_race_p == pred.bla \~ "black",
max_race_p == pred.his \~ "Hispanic", max_race_p == pred.oth \~ "other",
max_race_p == pred.whi \~ "white", TRUE \~ NA_character\_ ))

# Join race data back to the main applications dataset

applications=applications %\>% left_join(examiner_race, by =
c("examiner_name_last" = "surname"))

# Tenure calculation

examiner_dates=applications %\>% select(examiner_id, filing_date,
appl_status_date)

# Convert dates to a consistent format

examiner_dates=examiner_dates %\>% mutate(start_date = ymd(filing_date),
end_date = as_date(dmy_hms(appl_status_date)))

# Calculate the earliest and latest dates for each examiner and their tenure in days

examiner_dates=examiner_dates %\>% group_by(examiner_id) %\>% summarise(
earliest_date = min(start_date, na.rm = TRUE), latest_date =
max(end_date, na.rm = TRUE), tenure_days = interval(earliest_date,
latest_date) %/% days(1) ) %\>% filter(year(latest_date)\<2018)

# Join tenure data back to the main applications dataset

applications=applications %\>% left_join(examiner_dates, by =
"examiner_id")


    ## Creating processing time variable

    ```{r processing-time-variable, echo=FALSE}
    applications=applications %>%
      mutate(
        final_decision_date = coalesce(patent_issue_date, abandon_date),
        app_proc_time = as.numeric(difftime(final_decision_date, filing_date, units = "days"))
      )

# Centrality measures

First, I want to create a unique list of examiner ids before the
analysis:

\`\`\`{r unique-examiner-ids, echo=FALSE}
unique_examiner_ids=unique(c(edges$ego_examiner_id, edges$alter_examiner_id))

g=graph_from_data_frame(edges\[, c("ego_examiner_id",
"alter_examiner_id")\], directed = TRUE, vertices = data.frame(name =
unique_examiner_ids))


    Now, it's time to proceed with calculating the centrality measures:

    ```{r centrality-measures, echo=FALSE}
    centrality_entire=data.frame(
      examiner_id = V(g)$name,
      degree_centrality = degree(g, mode = "out"),
      betweenness_centrality = betweenness(g, directed = TRUE),
      closeness_centrality = closeness(g, mode = "out")
    )

    centrality_entire$examiner_id=as.numeric(centrality_entire$examiner_id)

    applications=applications %>%
      left_join(centrality_entire, by = "examiner_id")

    centrality_entire=data.frame(
      examiner_id = V(g)$name,
      degree_centrality = degree(g, mode = "out"),
      betweenness_centrality = betweenness(g, directed = TRUE),
      closeness_centrality = closeness(g, mode = "out")
    )

    centrality_entire$examiner_id=as.numeric(centrality_entire$examiner_id)

    applications=applications %>%
      left_join(centrality_entire, by = "examiner_id")

# Exploratory Data Analysis

`{r column-names, echo=FALSE} # column names to confirm the presence of 'gender' colnames(applications)`

\`\`\`{r eda, echo=FALSE} \# Histogram of processing time
ggplot(applications, aes(x = app_proc_time)) + geom_histogram(binwidth =
30, fill = "blue", color = "black") + labs(title = "Histogram of
Application Processing Time", x = "Processing Time (days)")


    # Regression Analysis

    First, I will remove the missing values in degree, betweenness, and closeness centrality.
    ```{r missing-value-remove, echo=FALSE}
    applications_clean=applications %>%
      filter(!is.na(degree_centrality.x),
             !is.na(betweenness_centrality.x),
             !is.na(closeness_centrality.x))

## Degree centrality linear regression model

First, I do the analysis to estimate the linear regression model with
degree_centrality as the independent variable

\`\`\`{r regression-analysis-degree-centrality, echo=FALSE} \# Estimate
the linear regression model with degree_centrality as the independent
variable degree_model=lm( app_proc_time \~ degree_centrality.x +
gender.x + race.x + tenure_days.x, data = applications_clean )

summary(degree_model)

    ### Explanation on degree centrality linear regression model

    The degree_model includes degree_centrality, gender, race, and tenure_days as independent variables, and app_proc_time as the dependent variable. The adjusted R-squared value of the model is 0.003339, which means that only about 0.33% of the variation in app_proc_time can be explained by the model. This is quite low, indicating that the model does not fit the data well.

    ## Betweenness centrality linear regression model

    Now, I do the analysis to estimate the linear regression model with betweenness_centrality as the independent variable

    ```{r regression-analysis-betweenness-centrality, echo=FALSE}
    # Betweenness centrality linear regression model
    betweenness_model=lm(
      app_proc_time ~ betweenness_centrality.x + gender.x + race.x + tenure_days.x,
      data = applications_clean
    )
    summary(betweenness_model)

### Explanation on betweenness centrality linear regression model

The betweenness_model includes betweenness_centrality, gender, race, and
tenure_days as independent variables, and app_proc_time as the dependent
variable. The adjusted R-squared value of the model is 0.003478, which
is still low, suggesting that betweenness_centrality is not a good
predictor of app_proc_time.

## Closeness centrality linear regression model

Next, I do the analysis to estimate the linear regression model with
closeness_centrality as the independent variable

`{r closeness_model} # Closeness centrality linear regression model closeness_model=lm(   app_proc_time ~ closeness_centrality.x + gender.x + race.x + tenure_days.x,   data = applications_clean ) summary(closeness_model)`

### Explanation on closeness centrality linear regression model

The closeness_model includes closeness_centrality, gender, race, and
tenure_days as independent variables, and app_proc_time as the dependent
variable. The adjusted R-squared value of the model is 0.008616, which
is slightly better than that of the betweenness_model, but still
relatively low. This suggests that while closeness_centrality may have
some predictive power for app_proc_time, it is not a strong predictor on
its own.

## Combined model of linear regression

Lastly, I do the analysis to estimate the linear regression combined
model

`{r combined_model, echo=FALSE} # Combined centrality linear regression model combined_model=lm(   app_proc_time ~ degree_centrality.x + betweenness_centrality.x + closeness_centrality.x + gender.x + race.x + tenure_days.x,   data = applications_clean ) summary(combined_model)`

### Explanation on combined model

The combined model (including degree, betweenness, and closeness
centralities) has an adjusted R-squared of 0.008712, while the
closeness_model has an adjusted R-squared of 0.008616 Although the
combined model has a slightly higher adjusted R-squared, the improvement
is not that significant

# Analysis to see if this relationship differ by examiner gender

## Degree-Gender interaction

`{r degree_gender_interaction} # Degree centrality model with interaction degree_gender_interaction=lm(   app_proc_time ~ degree_centrality.x * gender.x + race.x + tenure_days.x,   data = applications_clean ) summary(degree_gender_interaction)`

### Explanation on Degree-Gender interaction

In the degree-gender interaction model, there's a significant
interaction between degree centrality and gender, indicating that the
impact of degree centrality on processing time differs between male and
female examiners. While degree centrality slightly increases processing
time, its impact is lessened for male examiners, as suggested by the
negative interaction term. However, the overall explanatory power
remains low.

## Betweenness-Gender interaction

`{r betweenness_gender_interaction} # Betweenness centrality model with interaction betweenness_gender_interaction=lm(   app_proc_time ~ betweenness_centrality.x * gender.x + race.x + tenure_days.x,   data = applications_clean ) summary(betweenness_gender_interaction)`
\### Explanation on Betweenness-Gender interaction

The betweenness-gender interaction model reveals a negative effect of
betweenness centrality on processing time, which becomes more pronounced
for male examiners (positive interaction term). This suggests that
higher betweenness centrality could lead to longer processing times,
especially for males. Still, the model explains a very small portion of
the variability in processing time.

## Closeness-Gender interaction:

`{r closeness_gender_interaction} # Closeness centrality model with interaction closeness_gender_interaction=lm(   app_proc_time ~ closeness_centrality.x * gender.x + race.x + tenure_days.x,   data = applications_clean ) summary(closeness_gender_interaction)`

### Explanation on Closeness-Gender interaction

In the closeness-gender interaction model, closeness centrality shows a
substantial negative impact on processing time, which is altered for
male examiners, indicated by the negative interaction term. This implies
that while closeness generally reduces processing time, this effect is
less significant for male examiners.

## Combined-Gender interaction:

`{r combined_gender_interaction} # Combined model with interaction combined_gender_interaction=lm(   app_proc_time ~ (degree_centrality.x + betweenness_centrality.x + closeness_centrality.x) * gender.x + race.x + tenure_days.x,   data = applications_clean ) summary(combined_gender_interaction)`

### Explanation on Combined-Gender interaction

The combined-gender interaction model integrates all three centrality
measures and their interactions with gender. It shows varied effects of
centrality on processing time, with the interactions indicating that
gender moderates these effects. Despite incorporating more factors, the
model's adjusted R-squared remains low, indicating limited overall
explanatory power.

# Conclusion

Across all models, the interaction terms highlight that gender plays a
role in how centrality measures affect processing time. However, the low
adjusted R-squared values in each model suggest that centrality measures
and gender, while statistically significant in some cases, do not
robustly predict application processing times. The consistent low
explanatory power across models indicates that additional factors not
captured here are likely influencing processing times at the USPTO.
These findings underscore the complexity of the factors affecting
processing times and suggest that a more nuanced model, possibly
including more variables or different types of analysis, would be
necessary to fully understand these dynamics.

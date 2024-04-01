---
author: Sheida Majidi
date: 2024-03-31
output:
  html_document: default
  word_document: default
title: Exercise 3 - Analysis of examiner demographics and advice
  networks at the USPTO
---

# Introduction

In this analysis, I delve into the demographics of patent examiners and
their advice networks within the United States Patent and Trademark
Office (USPTO).

\`\`\`{r setup, include=FALSE} knitr::opts_chunk\$set(echo = TRUE)
library(tidyverse) library(lubridate) library(gender) library(wru)
library(igraph) library(ggraph) library(gridExtra)


    # Data loading and preprocessing

    First, I load the data and prepare it for the analysis:


    ```{r data}
    applications = read_csv("/Users/sheidamajidi/Desktop/Winter2024/Winter2024-2/ORGB672/Exercises/Exercise 3/app_data_starter.csv")
    edges = read_csv("/Users/sheidamajidi/Desktop/Winter2024/Winter2024-2/ORGB672/Exercises/Exercise 3/edges_sample.csv")

# Demographic analysis

## Gender estimation

I'll estimate the gender of examiners based on their first names to
explore gender diversity.

\`\`\`{r gender-estimation, echo=FALSE} examiner_names = applications
%\>% distinct(examiner_name_first) %\>% drop_na(examiner_name_first)

examiner_names_gender = examiner_names %\>% rowwise() %\>% mutate(gender
= gender(examiner_name_first, method = "ssa")\$gender\[1\]) %\>%
ungroup() %\>% select(examiner_name_first, gender)

applications = applications %\>% left_join(examiner_names_gender, by =
"examiner_name_first")


    ## Race estimation

    Next, I'll estimate the racial background of the examiners using their last names.

    ```{r race-estimation, echo=FALSE}
    examiner_surnames = applications %>%
      distinct(examiner_name_last) %>%
      drop_na(examiner_name_last) %>%
      rename(surname = examiner_name_last)

    examiner_race = examiner_surnames %>%
      mutate(race = predict_race(voter.file = examiner_surnames, surname.only = TRUE)) %>%
      select(surname, race)

    applications = applications %>%
      left_join(examiner_race, by = c("examiner_name_last" = "surname"))

## Tenure calculation

Tenure at the organization could provide insights into experience levels
and employee turnover.

\`\`\`{r tenure-calculation, echo=FALSE} applications = applications
%\>% mutate(filing_date = ymd(filing_date), appl_status_date =
ymd_hms(appl_status_date), tenure_days = as.numeric(appl_status_date -
filing_date))



    # Workgroup analysis

    I'll focus on two workgroups for a detailed comparison of their demographics.


    ```{r workgroup-analysis, echo=FALSE}
    workgroup_ids = substr(applications$examiner_art_unit, 1, 3) %>%
      unique() %>%
      sort(decreasing = TRUE) %>%
      head(2)

    applications_filtered = applications %>%
      filter(substr(examiner_art_unit, 1, 3) %in% workgroup_ids)

# Network analysis

## Advice network creation

Now, let's examine the advice networks to understand the interaction
patterns among examiners.

\`\`\`{r network-creation, echo=FALSE} \# extract unique examiner IDs
from the edges data frame unique_examiner_ids =
unique(c(edges$ego_examiner_id, edges$alter_examiner_id))

# create the vertices data frame with unique examiner IDs

vertices_df = data.frame(name = unique_examiner_ids)

# create the edges data frame with the correct mapping

edges_df = edges %\>% filter(ego_examiner_id %in% unique_examiner_ids &
alter_examiner_id %in% unique_examiner_ids) %\>% select(ego =
ego_examiner_id, alter = alter_examiner_id)

# create the advice network graph with explicit vertices and edges

advice_network = graph_from_data_frame(d = edges_df, vertices =
vertices_df, directed = TRUE)

# check the order of the graph (number of vertices)

gorder(advice_network)



    ## Centrality scores

    Centrality scores will help us identify key influencers and collaborators in the network.

    ```{r centrality-scores, echo=FALSE}
    # calculate centrality scores
    centrality_scores = data.frame(
      examiner_id = as.numeric(V(advice_network)$name),  # Convert to numeric to match applications
      degree = degree(advice_network),
      closeness = closeness(advice_network),
      betweenness = betweenness(advice_network)
    )

    # ensure examiner_id in applications is of type numeric (double)
    applications$examiner_id <- as.numeric(applications$examiner_id)

    # performing the join
    applications = applications %>%
      left_join(centrality_scores, by = "examiner_id")

# Results and discussion

Let's delve into the results to uncover insights about the demographics
and network dynamics of USPTO examiners.

## Demographic distribution

\`\`\`{r demographic-distribution, echo=FALSE} applications_filtered =
applications

# create the plot using all available art units

ggplot(applications_filtered, aes(x = race.x, fill = gender.x)) +
geom_bar(position = "dodge") + facet_wrap(\~examiner_art_unit)




    ## Network visualization

    Visualizing the advice network to see how examiners interact within and across workgroups.

    ```{r network-visualization, echo=FALSE}

    V(advice_network)$degree = degree(advice_network)

    # Add gender information to the graph's vertices
    # 'examiner_id' in 'applications' corresponds to vertex names in 'advice_network'
    gender_info = applications %>% 
      select(examiner_id, gender.x) %>% 
      distinct()

    gender_info$examiner_id <- as.character(gender_info$examiner_id)

    # Merge gender info into the graph
    for (i in seq_along(V(advice_network))) {
      vertex_name <- V(advice_network)$name[i]
      V(advice_network)$gender.x[i] <- gender_info$gender.x[gender_info$examiner_id == vertex_name]
    }

    # plot the network
    ggraph(advice_network, layout = "kk") +
      geom_edge_link(color = "gray80") +
      geom_node_point(aes(size = degree, color = gender.x)) +
      theme_minimal() +
      labs(title = "Advice Network by Centrality and Gender")

## Demographic statistics

\`\`\`{r demographic-statistics, echo=FALSE} \# Distribution by gender
and race gender_distribution = applications %\>% group_by(gender.x) %\>%
summarise(Count = n())

race_distribution=applications %\>% group_by(race.x) %\>%
summarise(Count = n())

# Print the distributions

print(gender_distribution) print(race_distribution)



    ## Network centrality measures

    ```{r network-centrality-measures, echo=FALSE}
    # Centrality summary statistics
    centrality_summary=centrality_scores %>% 
      summarise(
        Average_Degree = mean(degree, na.rm = TRUE),
        Median_Degree = median(degree, na.rm = TRUE),
        Average_Closeness = mean(closeness, na.rm = TRUE),
        Median_Closeness = median(closeness, na.rm = TRUE),
        Average_Betweenness = mean(betweenness, na.rm = TRUE),
        Median_Betweenness = median(betweenness, na.rm = TRUE)
      )

    print(centrality_summary)

# Conclusion

Taking a look at the USPTO's data, it's clear we've got a vibrant mix of
examiners from various racial backgrounds, with white and Asian
colleagues being particularly prevalent. There's room for growth in
representation across all groups, especially among Hispanic and Black
communities. The gender balance also leans more towards men, and there's
a noticeable portion of the data where gender details are missing. It
would be beneficial to delve into why that's the case and work towards a
more balanced representation.

When it comes to the advice network within the organization, it's
fascinating to see how knowledge and support flow between colleagues.
Many have a small, close-knit circle of contacts they interact with, but
a few key individuals stand out as central nodes, forming the backbone
of the network. These pivotal players are integral to the spread of
information, although it's worth noting that the overall connectivity
could be improved. The majority of examiners aren't regularly bridging
communication between others, with only a select few serving as frequent
connectors.

Overall, these insights highlight an opportunity to foster stronger
connections across the board, encouraging a richer tapestry of
interaction and collaboration. Enhancing the network and striving for
greater diversity could lead to a more dynamic and inclusive
environment, which I believe could greatly benefit our collective
creativity and efficiency.

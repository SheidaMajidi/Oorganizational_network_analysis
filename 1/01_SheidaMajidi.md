---
always_allow_html: true
author: Sheida Majidi
date: 2024-03-18
output: word_document
title: Exercise 1 - LinkedIn Connections
---

Introduction:

I am analyzing my LinkedIn connections to understand my network better,
focusing on the distribution of connections across different employers
and the network's structure.

`{r setup, include=FALSE} knitr::opts_chunk$set(echo = TRUE) options(repos = c(CRAN = "https://cran.rstudio.com/")) # installing packages library('tidyverse') install.packages("network") library(network) library(igraph) install.packages("visNetwork") install.packages("networkD3") library(visNetwork) library(networkD3) install.packages("igraph") library(igraph) install.packages("tidygraph") library(tidygraph) library(stringr) install.packages("ggraph") library(ggraph)`

Data loading:

Starting with loading my LinkedIn connections data and perform initial
cleaning and examination of the data.

\`\`\`{r data-loading} file_path =
"/Users/sheidamajidi/Desktop/Winter2024/Winter2024-2/ORGB672/Exercises/1/Connections.csv"
connections = read_csv(file_path)

head(connections)

    Data Analysis

    This is the analysis to count the number of contacts by their current employer and calculate the total number of contacts.

    ```{r data-analysis, echo=FALSE}

    # count contacts by company
    company_counts <- connections %>%
      group_by(Company) %>%
      summarise(Count = n())
    company_counts

Frequency table and bar chart

For a better visualizations, I want to create a frequency table of the
companies and visualize the top 15 companies with the most connections

\`\`\`{r frequency-table, echo=FALSE}

connections \<- connections %\>% select(Name = `First Name`, Company,
Position)

freq_table \<- table(connections\$Company) freq_table \<-
sort(freq_table, decreasing = TRUE)

# the top 15 companies

top15 \<- head(freq_table, n = 15)

# display the list as a table

knitr::kable(as.data.frame(top15), col.names = c("Company",
"Connections"))

# bar chart of the frequency table

barplot(top15, main = "Top 15 connections on LinkedIn")



    Network creation 

    Now I want to create nodes and edges for the network analysis. In this network, individuals are nodes and connections between individuals who work at the same company are edges.

    ```{r network-creation, echo=FALSE}

    #  nodes
    people = connections %>% distinct(Name) %>% rename(label = Name)
    companies = connections %>% distinct(Company) %>% rename(label = Company)
    nodes = full_join(people, companies, by = "label")
    nodes = rowid_to_column(nodes, "id")

    #  edges
    edges = connections %>% select(Name, Company)
    edges = edges %>%
      left_join(nodes, by = c("Name" = "label")) %>%
      rename(from = id)
    edges = edges %>%
      left_join(nodes, by = c("Company" = "label")) %>%
      rename(to = id) %>%
      distinct(from, to)

    # building network with igraph
    routes_igraph = graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

    # plotting with igraph
    plot(routes_igraph, vertex.size = 3, vertex.label.cex = 0.2, edge.arrow.size = 0.01)

    # using visNetwork for visualization
    visNetwork(nodes, edges)

\`\`\`{r network, echo=FALSE} library(ggraph)

# Convert igraph object to tbl_graph for ggraph

tbl_graph = as_tbl_graph(routes_igraph)

# Plotting with ggraph

ggraph(tbl_graph, layout = "fr") + geom_edge_link()



    ```{r visnetwork , echo=FALSE}
    ## visNetwork
    library(visNetwork)
    library(networkD3)
    visNetwork(nodes, edges)

\`\`\`{r coloring-me-myclassmates , echo=FALSE}

# color attribute to nodes based on the McGill colors

nodes$color = ifelse(nodes$label == "McGill University - Desautels
Faculty of Management", "red", "gray")

library(visNetwork)

nodes_vis = nodes %\>% select(id, label, color) %\>% mutate(title =
label)

edges_vis \<- edges %\>% select(from, to)

visNetwork(nodes_vis, edges_vis) %\>% visNodes(color = list(background =
nodes_vis\$color, border = "#2b2b2b")) %\>% visEdges(arrows = 'to') %\>%
visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

\`\`\`

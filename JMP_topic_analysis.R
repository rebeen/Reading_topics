# Topic analysis in JMP:
# https://www.jmp.com/support/help/en/16.1/index.shtml#page/jmp/topic-analysis.shtml

setwd("H:/Downloads/R-code")

library(tidyverse)
library(RColorBrewer)
library(wordcloud2)

# Topic analysis in JMP is equivalent to a varimax-rotated principal component analysis (PCA)
# Terms in each topic are those with largest loadings by absolute value after rotation.

# Function to get rotated loadings sorted by absolute value
get_top_loadings = function(topic_number, loadings_obj, num_terms){
  
  rotated_loadings = loadings_obj[, topic_number]
  top_loadings = sort(abs(rotated_loadings), decreasing = TRUE)[1:num_terms]
  
  return(top_loadings)
}


# Function to run varimax-rotated PCA
run_rotated_PCA = function(input_data, num_topics, num_terms){
  
  # Remove any all-zero columns (these cause NaNs when scaling)
  input_data = input_data[, colSums(input_data) != 0]
  
  # Run PCA
  pca = prcomp(input_data, center = TRUE, scale = TRUE)

  # Get rotation matrix for required number of principal components
  rotation_mat = pca$rotation[, 1:num_topics]
  
  # Get standard deviations of the principal components
  sdev_mat = diag(pca$sdev, num_topics, num_topics)
  
  # Multiply to get raw loadings
  raw_loadings = rotation_mat %*% sdev_mat
  
  # Varimax rotation
  rotated_loadings = varimax(raw_loadings)$loadings
  
  # Get a list of topics
  topic_list = lapply(1:num_topics, get_top_loadings, rotated_loadings, num_terms)
  
  return(topic_list)
}

# Function to draw a wordcloud
get_word_cloud = function(topic_vector, word_size = 1){
  
  topic_df = data.frame(term = names(topic_vector), loadings = topic_vector * 100)
  
  wordcloud2(topic_df, size = word_size, color = brewer.pal(8, "Dark2"), rotateRatio = 0, gridSize = 0)
}


# Example:

# The input is a binary matrix with patients as rows and terms as columns
ltc_example = read_tsv("H:/Downloads/R-code/ltc_matrix_binary_mm4.tsv") %>% column_to_rownames(var = "patient_id")

# Get a list of 30 topics, each with 20 terms
topics = run_rotated_PCA(input_data = ltc_example, num_topics = 5, num_terms = 20)

# Example topics
topics[[1]]
topics[[2]]
topics[[4]]

# Example wordclouds
topics[[1]] %>% get_word_cloud(word_size = 0.5)
topics[[2]] %>% get_word_cloud(word_size = 0.5)
topics[[4]] %>% get_word_cloud(word_size = 0.5)



sink("topic1.txt")
print(topics[[1]])
sink()
sink("topic2.txt")
print(topics[[2]])
sink()

sink("topic3.txt")
print(topics[[3]])
sink

sink("topic4.txt")
print(topics[[4]])
sink()

sink("topic5.txt")
print(topics[[5]])
sink()

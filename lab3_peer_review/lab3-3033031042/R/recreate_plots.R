# This file provides the functionality to recreate Figure 3 from
# the Ben-Hur et. al. paper.

load <- function(fname) {
  # A function to read in the data.
  #
  # Arguments: fname (filename to read in)
  # Returns: R dataframe with the specified filename

  df <- read.csv(fname, header=TRUE, row.names = NULL)
  return(df)
}
# 

plot_similarity_histogram <- function(ki) {
  # A function to plot the histogram of similarity
  # scores for a given value of k.
  #
  # Arguments: k (the number of clusters to plot for)
  # Returns: ggplot object with histogram of similarity scores

    plt <- ggplot(subset(result_df, k==ki)) +
            geom_histogram(aes(x=similarity)) +
            ggtitle(paste0("k=",ki)) +
            # we want all scales to be the same
            scale_x_continuous(limits = c(0.4, 1)) +
            # set theme
            theme_bw() + 
            theme(axis.title.x = element_text(size = 8),
                  axis.title.y = element_text(size = 8),
                  plot.title = element_text(size=8, face="bold"),
                  legend.position ="right",
                  legend.title = element_text(size=8),
                  legend.text = element_text(size=8))
    return(plt)
}

plot_cdf <- function() {
  # A function to plot the CDF of correlation similarity
  # scores for a given value of k.
  #
  # Arguments: k (the number of clusters to plot for)
  # Returns: ggplot object with CDF of similarity scores
  
  plt <- ggplot(result_df, aes(x=similarity)) +
    stat_ecdf(aes(group=factor(k), color=factor(k))) +
    ggtitle("Cumulative distributions for increasing values of k") +
    # set theme
    guides(color=guide_legend(title="k")) +
    theme_bw() + 
    theme(axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          plot.title = element_text(size=8, face="bold"),
          legend.position ="right",
          legend.title = element_text(size=8),
          legend.text = element_text(size=8))
  return(plt)
}
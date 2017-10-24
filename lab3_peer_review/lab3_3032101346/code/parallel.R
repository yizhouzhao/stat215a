# R script to run on the SCF cluster - run the model explorer algorithm
# of Ben-Hur et al (2002) for each value of k in {2,...,10} in parallel

# IMPORTANT - when running on SCF cluster, set working 
# directory to top-level directory of this repository
setwd("..")

library(foreach)
library(doParallel)
library(tidyverse)

# load similarity measure CorrCPP
library('Rcpp')
sourceCpp('code/Corr.cpp')

# load in data and calc_sim
source('code/ClusterSim.R')

# set values of k to use in outer loop
kmax <- 10

# set fraction m of data to subsample
m <- 0.8

# set number of repeated iterations N
N <- 100

# set the number of cores to size of outer loop
nCores <- kmax - 1
registerDoParallel(nCores) 

# a list with result for each k
result <- foreach(k = 2:kmax) %dopar% {
  # calculate correlation N times with k clusters 
  # on a fraction m of the dataset ling
  calc_sim(ling, CorrCPP, m, k, N)
}

# write results to output file!
save(result, file = 'outputs/cluster_sim.RData')

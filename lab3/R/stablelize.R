library(Rcpp)
library(dplyr)
library(foreach)
library(doParallel)
library(parallel) 
library(iterators)
library(microbenchmark)

#Set working directory
getwd()
load("data/lingBinary.RData")

#load cpp file
cpp_directory = ("/accounts/grad/yizhou_zhao/Stat215")
sourceCpp(file.path(cpp_directory, 'cor_sim.cpp')) 


stabalize <- function(Df,k,N,ratio,method = "corSim"){
  #Funition to get the stablized K
  #Input:
  # Df: data; K: number of clusters; N: number of iterations; 
  #           ratio: sampling ratio; method = "corSim","matchingCoeff" or "jaccardCoeff"
  similarityList = vector()
    for(i in 1:N){
      
      #sample by fractions and kmeans
      sample1 = sample_frac(Df,size = ratio, replace = F)
      sample2 = sample_frac(Df,size = ratio, replace = F)
      k.means1 = kmeans(sample1[7:ncol(Df)], centers = k, iter.max = 5000)$cluster
      k.means2 = kmeans(sample2[7:ncol(Df)], centers = k, iter.max =5000)$cluster
      
      #find intersection by ID
      id <- as.vector(t(arrange(inner_join(sample1, sample2, by= 'ID') %>% select(ID)))) 
      id = as.factor(id)
      
      #get the similarity metric from cpp funtions
      if(method == "corSim"){
        similarityList[i] = corSim(k.means1[id],k.means2[id])
      }
      else if(method == "matchCoeff"){
        similarityList[i] = matchCoeff(k.means1[id],k.means2[id])
      }
      else if(method == "jaccardCoeff"){
        similarityList[i] = jaccardCoeff(k.means1[id],k.means2[id])
      }
    }
  
  colName = paste0("cluster_",as.character(k))
  return(data.frame(colName = similarityList))
}
    
#Parallel computing part
nCores <- detectCores()
registerDoParallel(nCores)

#set up parameters
k.max = 10
num.samples = 100
sample.ratio = 0.7
    
#foreach part for parallel computing
parallel.results <- foreach(i = 2:k.max) %dopar% { 
  set.seed(i)
  column <- stabalize(lingBinary, i, num.samples, sample.ratio,method = "jaccardCoeff") 
  filename <- paste0("cluster",i,".csv")
  write.csv(column, file=filename,
                quote=F, row.names=F)
  return(filename)
}


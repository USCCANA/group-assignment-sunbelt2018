library(igraph)

# Reading the data in
leaders_ga <- readRDS("simulations/group_assignment.rds")
leaders_kp <- readRDS("simulations/keyplayer.rds")
leaders_id <- readRDS("simulations/indegree.rds")

dat_attributes <- readRDS("simulations/dat_attributes.rds")
dat_networks   <- readRDS("simulations/dat_networks.rds")
dat_nleaders   <- readRDS("simulations/dat_nleaders.rds")

# Preprocessing
leaders_kp <- lapply(leaders_kp, function(y) lapply(y, function(x) {
  
  if (length(x) == 1 && is.na(x))
    return(x)
  
  as.vector(x)

  }))
leaders_ga <- lapply(leaders_ga, function(y) lapply(y, function(x) {
  
  if (length(x) == 1 && is.na(x))
    return(x)
  
  unique(x[-length(x)])
  }))

# Function to compute overlap of two sets
jaccard <- function(a, b) {
  
  if (any(is.na(c(a, b))))
    return(NA)
  
  length(intersect(a, b)) / 
    length(unique(c(a,b)))
  
}

# KP vs Indegree
overlap_kp_id <- structure(vector("list", 3), names = names(dat_networks))
for (n in names(overlap_kp_id)) {
  
  overlap_kp_id[[n]] <- structure(vector("double", length(dat_nleaders)))
  
  for (i in seq_along(overlap_kp_id[[n]]))
    overlap_kp_id[[n]][[i]] <- jaccard(leaders_kp[[n]][[i]],leaders_id[[n]][[i]])
  
}

# KP vs GA
overlap_kp_ga <- structure(vector("list", 3), names = names(dat_networks))
for (n in names(overlap_kp_ga)) {
  
  overlap_kp_ga[[n]] <- structure(vector("double", length(dat_nleaders)))
  
  for (i in seq_along(overlap_kp_ga[[n]]))
    overlap_kp_ga[[n]][[i]] <- jaccard(leaders_kp[[n]][[i]],leaders_ga[[n]][[i]])
  
}

# Indegree vs GA
overlap_id_ga <- structure(vector("list", 3), names = names(dat_networks))
for (n in names(overlap_id_ga)) {
  
  overlap_id_ga[[n]] <- structure(vector("double", length(dat_nleaders)))
  
  for (i in seq_along(overlap_id_ga[[n]]))
    overlap_id_ga[[n]][[i]] <- jaccard(leaders_id[[n]][[i]],leaders_ga[[n]][[i]])
  
}

J <- rbind(
  `KP vs GA`       = sapply(overlap_kp_ga, mean, na.rm = TRUE),
  `KP vs Indegree` = sapply(overlap_kp_id, mean, na.rm = TRUE),
  `Indegree vs GA` = sapply(overlap_id_ga, mean, na.rm = TRUE)
)


write.csv(J, "simulations/overlap.csv")

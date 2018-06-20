library(igraph)
library(magrittr)

membership_gn <- readRDS("simulations/leaders-membership-girvan-newman.rds")
membership_mm <- readRDS("simulations/leaders-membership-mentor-match.rds")
membership_rg <- readRDS("simulations/leaders-membership-random-groups.rds")

dat_attributes <- readRDS("simulations/dat_attributes.rds")
dat_networks   <- readRDS("simulations/dat_networks.rds")
dat_nleaders   <- readRDS("simulations/dat_nleaders.rds")

# Function to compute overlap of two sets
hamming_dist <- function(a, b) {
  
  a <- as.matrix(dist(cbind(a)))
  a[] <- as.integer(a == 0)
  diag(a) <- 0
  
  b <- as.matrix(dist(cbind(b)))
  b[] <- as.integer(b == 0)
  diag(b) <- 0
  
  
  n <- nrow(a)
  
  sum(abs(a - b))/(n*(n-1))
  
}

results <- ls(pattern = "membership_[a-zA-Z0-9]+")
combinations <- combn(results, 2, simplify = FALSE)

for (g in combinations) {
  
  # Creating the output
  ans <- structure(vector("list", 3), names = names(dat_networks))
  for (n in names(ans)) {
    
    ans[[n]] <- structure(vector("double", length(dat_nleaders)))
    
    # Computing the jaccard index
    for (i in seq_along(ans[[n]]))
      ans[[n]][[i]] <- hamming_dist(get(g[1])[[n]][[i]],get(g[2])[[n]][[i]])
    
  }
  
  # Saving the result
  assign(
    paste0(
      "overlap_",
      gsub("^membership_", "", g[1]), "_",
      gsub("^membership_", "", g[2])
    ),
    ans,
    envir = .GlobalEnv
  )
  
}

# Computing means
J <- lapply(mget(ls(pattern="^overlap")), sapply, mean, na.rm=TRUE) %>%
  do.call(rbind, .)

# Updating rownames
rownames(J) <- gsub("overlap_([a-zA-Z]+)_([a-zA-Z]+)", "\\1 vs \\2", rownames(J)) %>%
  toupper

J <- J[order(-J[,1], -J[,2], -J[,3]),]

rownames(J) <- gsub("ID", "Indegree", rownames(J))
rownames(J) <- gsub("MM", "Mentor Matching", rownames(J))
rownames(J) <- gsub("KP", "Key Players", rownames(J))
rownames(J) <- gsub("GA", "Group Assignment", rownames(J))

rownames(J) <- gsub("RG", "Random Groups", rownames(J))
rownames(J) <- gsub("GN", "Girvan-Newman", rownames(J))

# Writing the output
write.csv(J, "simulations/group-overlap.csv")

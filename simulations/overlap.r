library(igraph)

# Reading the data in
leaders_ga <- readRDS("simulations/leaders-group-assignment.rds")
leaders_kp <- readRDS("simulations/leaders-keyplayer.rds")
leaders_id <- readRDS("simulations/leaders-indegree.rds")
leaders_mm <- readRDS("simulations/leaders-mentor-match.rds")
leaders_gn <- readRDS("simulations/leaders-girvan-newman.rds")
leaders_rg <- readRDS("simulations/leaders-random-groups.rds")

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

results <- ls(pattern = "leaders_[a-zA-Z0-9]+")
combinations <- combn(results, 2, simplify = FALSE)

for (g in combinations) {
  
  # Creating the output
  ans <- structure(vector("list", 3), names = names(dat_networks))
  for (n in names(ans)) {
    
    ans[[n]] <- structure(vector("double", length(dat_nleaders)))
    
    # Computing the jaccard index
    for (i in seq_along(ans[[n]]))
      ans[[n]][[i]] <- jaccard(get(g[1])[[n]][[i]],get(g[2])[[n]][[i]])
    
  }
  
  # Saving the result
  assign(
    paste0(
      "overlap_",
      gsub("^leaders_", "", g[1]), "_",
      gsub("^leaders_", "", g[2])
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
write.csv(J, "simulations/overlap.csv")


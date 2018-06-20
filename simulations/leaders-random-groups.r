library(igraph)
library(dplyr)
library(magrittr)
library(netdiffuseR)

dat_attributes <- readRDS("simulations/dat_attributes.rds")
dat_networks   <- readRDS("simulations/dat_networks.rds")
dat_nleaders   <- readRDS("simulations/dat_nleaders.rds")

# Computing assignments --------------------------------------------------------

ans <- vector("list", length(dat_networks))
names(ans) <- names(dat_networks)

# Looping through networks
set.seed(1)
for (n in names(ans)) {
  
  ans[[n]] <- vector("list", length(dat_networks[[n]]))
  
  for (i in seq_along(ans[[n]])) {
    
    # Random groups assignment
    ans[[n]][[i]] <- suppressWarnings({
      matrix(1:dat_nleaders[i], nrow = nrow(dat_attributes[[i]]), ncol = 1) %>%
      as.vector %>% sample})
    
    # Creating the data and sorting by indegree and betweenness
    ans[[n]][[i]] <- cbind(dat_attributes[[i]], mem = ans[[n]][[i]]) %>%
      mutate(id = 1:n()) 
    ans[[n]][[i]] <- ans[[n]][[i]][
      order(
        -ans[[n]][[i]][[paste0("indegree_", n)]],
        -ans[[n]][[i]][[paste0("betweenness_", n)]]
      ),]
    
    # Choosing the one with the highest indegree
    ans[[n]][[i]] <- ans[[n]][[i]] %>% 
      group_by(mem) %>%
      filter(1:n() <= 1) %>%
      ungroup %>%
      select(id)  %>% 
      unclass %>% unname %>% "[["(1)
    
  }
  
  message("Network ", n, " done.")
}

saveRDS(
  ans,
  file = "simulations/leaders-random-groups.rds"
)

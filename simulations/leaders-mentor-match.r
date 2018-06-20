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
  
  for (i in seq_along(ans[[n]]))
    ans[[n]][[i]] <- mentor_matching(
      dat_networks[[n]][[i]],
      dat_nleaders[i],
      geodist.args = list(n=10)) %>% 
      filter(isleader) %>%
      select(name) %>%
      unlist %>% unname %>% as.integer

  message("Network ", n, " done.")
}

saveRDS(
  ans,
  file = "simulations/leaders-mentor-match.rds"
)

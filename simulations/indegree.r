library(igraph)
library(dplyr)
library(magrittr)

dat_attributes <- readRDS("simulations/dat_attributes.rds")
dat_networks   <- readRDS("simulations/dat_networks.rds")
dat_nleaders   <- readRDS("simulations/dat_nleaders.rds")

indegree <- vector("list")

# Computing assignments --------------------------------------------------------

# Scale free
indegree_sf <- vector("list", length(dat_nleaders))

# Looping through networks
for (i in seq_along(indegree_sf))
  indegree_sf[[i]] <- dat_attributes[[i]] %>%
  mutate(ord = 1:n()) %>%
  as_tibble %>%
  arrange(-indegree_scale_free, -betweenness_scale_free) %>%
  select(ord) %>%
  unclass %>%
  unlist %>%
  head(dat_nleaders[i]) %>%
  unname


# Small world
indegree_sw <- vector("list", length(dat_nleaders))

# Looping through networks
for (i in seq_along(indegree_sw))
  indegree_sw[[i]] <- dat_attributes[[i]] %>%
  mutate(ord = 1:n()) %>%
  as_tibble %>%
  arrange(-indegree_small_world, -betweenness_small_world) %>%
  select(ord) %>%
  unclass %>%
  unlist %>%
  head(dat_nleaders[i]) %>%
  unname


# Scale free
indegree_sf_h <- vector("list", length(dat_nleaders))

# Looping through networks
for (i in seq_along(indegree_sf_h))
  indegree_sf_h[[i]] <- dat_attributes[[i]] %>%
  mutate(ord = 1:n()) %>%
  as_tibble %>%
  arrange(-indegree_sf_homophilic, -betweenness_sf_homophilic) %>%
  select(ord) %>%
  unclass %>%
  unlist %>%
  head(dat_nleaders[i]) %>%
  unname

saveRDS(
  list(
    scale_free    = indegree_sf,
    small_world   = indegree_sw,
    sf_homophilic = indegree_sf_h
  ),
  file = "simulations/indegree.rds"
)

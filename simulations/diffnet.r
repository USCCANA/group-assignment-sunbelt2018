library(netdiffuseR)

# Reading the data in
dat_networks <- readRDS("simulations/dat_networks.rds")

leaders_ga <- readRDS("simulations/leaders-group-assignment.rds")
leaders_kp <- readRDS("simulations/leaders-keyplayer.rds")
leaders_id <- readRDS("simulations/leaders-indegree.rds")
leaders_mm <- readRDS("simulations/leaders-mentor-match.rds")
leaders_gn <- readRDS("simulations/leaders-girvan-newman.rds")
leaders_rg <- readRDS("simulations/leaders-random-groups.rds")

leaders <- mget(ls(pattern="leaders_"))

# Contagion process simulation
models <- list(
  thr30pcentbeta = list(
    thr  = function(n) rbeta(n, 3, 7),
    desc = "Threshold distributed as a Beta(3, 7) (0.3 average threshold)",
    exposure.args = list()
    ),
  thr2count = list(
    thr  = function(n) rep(2, n),
    desc = "Exposure to 2 individuals",
    exposure.args = list(normalized=FALSE)
  ),
  thrUnif = list(
    thr  = function(n) runif(n),
    desc = "Uniform threshold U(0,1).",
    exposure.args = list()
  )
)

# This function creates a named vector of type list
namedvector <- function(x) structure(vector("list", length(x)), names=names(x))

ans <- namedvector(models)
count <- 0

set.seed(1)
for (m in names(models)) {
  
  # Vector of types
  ans[[m]] <- namedvector(dat_networks)
  
  for (n in names(dat_networks)) {
    
    # Vector of networks
    ans[[m]][[n]] <- namedvector(leaders)
    
    for (i in seq_along(dat_networks[[n]])) {
      
      thr <- models[[m]]$thr(nrow(dat_networks[[n]][[i]]))
      
      for (l in names(leaders)) {
        
        # Simulating diffusion process
        ans[[m]][[n]][[l]][[i]] <- do.call(
          rdiffnet,
          list(
            seed.graph     = dat_networks[[n]][[i]],
            threshold.dist = thr,
            t              = 10L,
            stop.no.diff   = FALSE,
            exposure.args  = models[[m]]$exposure.args
          )
        )
        
        # Increasing the counter to watch progress
        count <- count + 1
        
        # Are we there yet?
        if (!(count %% 20)) {
          message(sprintf(
            "Leaders %s in network %s done (%4i/%04i), model %s.", l, n,
            count,
            length(models)*length(leaders)*length(dat_networks[[1]])*length(dat_networks),
            m
          ))
          
          # Saving
          saveRDS(ans, "simulations/diffnet.rds")
        }
      }
      
    }
    
  }
    
}


# Saving
message("Done")
saveRDS(ans, "simulations/diffnet.rds")
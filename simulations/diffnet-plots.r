library(netdiffuseR)
library(magrittr)

dat <- readRDS("simulations/diffnet.rds")

# This function creates a named vector of type list
namedvector <- function(x) structure(vector("list", length(x)), names=names(x))

# Fancier names
fancy_name <- function(x) {
  
  if (length(x) > 1)
    return(sapply(x, fancy_name))
  
  switch (gsub(".+_", "", x),
    id = "Indegree",
    mm = "Mentor Matching",
    kp = "Key Players",
    ga = "Group Assignment",
    rg = "Random groups",
    gn = "Girvan-Newman"
  )
  
}

for (m in names(dat)) {
  
  for (n in names(dat[[m]])) {
    
    ans <- namedvector(dat[[m]][[n]])
    
    for (l in names(dat[[m]][[n]])) {
      
      ans[[l]] <- sapply(
        dat[[m]][[n]][[l]],
        function(x) {
          cumulative_adopt_count(x)[2,]
        }) %>% t      
      
    }
    
    # Creating the object
    assign(paste(m,n,sep="_"), ans, envir = .GlobalEnv)
    
    # Creating a plot of all 
    ans <- lapply(ans, colMeans)
  
    
    # Plotting
    png(sprintf("simulations/diffnet-plots-%s-%s.png", m, n))
    plot(
      NA, xlim = c(0, 10), ylim = c(0,max(unlist(ans)) + .1),
      xlab = "Time", ylab = "Adoption",
      main = sprintf(
        "Diffusion Threshold: %s\nNetwork type: %s", m, n
      )
    )
    
    # Picking colors and linetypes
    cols <- RColorBrewer::brewer.pal(length(ans), "Dark2")
    ltys <- seq_along(ans)
    
    Map(function(n, Col, Lty) {
      lines(n, col = Col, lty=Lty, lwd=2)
      },
      n   = ans,
      Col = cols,
      Lty = ltys
      )
    
    legend(
      "topleft",
      legend = fancy_name(names(ans)),
      lwd    = 2,
      col    = cols, lty=ltys,
      bty    = "n"
    )
    
    dev.off()
    
  }
      
      
  
}
  




# Estimate the mean and summary for Water level:
indicator <- function( ht, wl){
  t.25 = ht*0.25
  t.50= ht*0.5
  t.75 = ht*0.75
  t.100= ht*1
  test.wl <- wl %>% as.data.frame()

  test.wl$indicator <- NA
  test.wl$indicator[test.wl$. <= 0] <- 0  #"dry" # below  soil surface
  test.wl$indicator[test.wl$. > 0 & test.wl$.  <= t.25] <- 0.25  #"25% coverage" 
  test.wl$indicator[test.wl$. > t.25 & test.wl$. <= t.50] <- 0.50 #"50% coverage" 
  test.wl$indicator[test.wl$. > t.50 &  test.wl$. <= t.75 ] <- 0.75 #">50% coverage"
  test.wl$indicator[test.wl$. > t.75 ] <- 1.00 #">50% coverage"
  test.wl$indicator[is.na(test.wl$indicator)]
  
  test.wl$indicator <- test.wl$indicator %>% as.factor 
  return(test.wl$indicator )
}

## A function to implement the transfer procedures
transferArea <- function(strata, rr){
  
  ## We'll track transfers using the rules template
  transferList <- list()
  
  ## Loop over rules order
  for (i in seq_along(rr$Order)){
    
    ## Calculate departure for giving/receiving sclasses
    give <- strata %>%
      filter(SCLASS == rr$Donate[i]) %>%
      mutate(departure = case_when(AREA < low ~ AREA - low,
                                   AREA > high ~ AREA - high,
                                   TRUE ~ 0))
    receive <- strata %>%
      filter(SCLASS == rr$Receive[i]) %>%
      mutate(departure = case_when(AREA < low ~ AREA - low,
                                   AREA > high ~ AREA - high,
                                   TRUE ~ 0))
    
    ## Overabundant s-classes can ONLY give to underabundant s-classes
    if (give$departure > 0 & receive$departure < 0) {
      
      ## Our transfer tracker
      transfer <- rr[i,]
      
      ## Transfer the "minimum" of departed acres
      transfer$REST_ACRES <- min(give$departure, abs(receive$departure))
      
      ## Update strata areas
      strata[strata$SCLASS == give$SCLASS,"AREA"] <- give$AREA - transfer$REST_ACRES
      strata[strata$SCLASS == receive$SCLASS,"AREA"] <- receive$AREA + transfer$REST_ACRES
      
      ## Log the transfer
      transferList[[i]] <- transfer 
    } 
  } # End order loop
  
  ## Get our list of transfers back to data.frame
  transfer <- bind_rows(transferList)
  
  
  ## Check departure again, sometimes we need to loop through twice becuase of 
  ## the order of transfers listed in restRules
  strata <- strata %>%
    mutate(departure = case_when(AREA < low ~ AREA - low,
                                 AREA > high ~ AREA - high,
                                 TRUE ~ 0))
  
  return(list(transfer = transfer, strata = strata))
}


## For each group defined by grpBy and strata, balance the acreage to fall
## within NRV reference conditions based on the rules set provided in
## 'restRules'
## This could be done much more elegantly, but who cares
restArea <- function(x, stratArea, restRules){
  
  ## Strata area info
  strata <- dplyr::filter(stratArea, strataID == x)
  
  ## Restoration ruleset for the BPS
  rr <- dplyr::filter(restRules, BpS_Code %in% strata$BpS_Code)
  
  
  ## Now we loop over the order of "treatments", track our 
  ## transfers, and update strata departures sequentially
  out <- transferArea(strata, rr)
  
  
  ## Add the landscape unit ID  
  out$transfer[['strataID']] <- x
  
  ## Save the tracked transfers
  return(list(transfer = out$transfer, strata = out$strata))
}

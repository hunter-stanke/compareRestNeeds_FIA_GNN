##=====================================================
##=====================================================
##
## Estimate restoration need from s-class distributions
## and reference conditions defined by biophysical 
## settings. Just an implementation of the rulesets
## provided in Haugo et al and DeMeo et al.
##
## Created:       30 October 2020 - Hunter Stanke
## Last modified: 9 December 2020 - Hunter Stanke
##
##====================================================
##====================================================


## Function to generate restoration need from S-class areas and ref conditions
## ----------------------------------------------------------------------------
## dirResults (character): directory where results are output
## dirRefCon (character):  directory where reference conidition files live
## cores (numeric):        number of physical cores to use
estimate_rest_needs <- function(dirResults = here::here('results/GNN/'),
                                year = year,
                                dirRefCon = here::here('data/refCon/'),
                                cores = 1) {
  
  ## Reference conditions - NRV +/- 2 SD
  refCon <- read.csv(paste0(dirRefCon, '/R6RefCon.txt')) %>%
    dplyr::select(BpS_Code = LF_BpS_Code, SCLASS = Sclass, avg = Avg_, low = Minus_2_SD, high = Plus_2_SD) %>%
    dplyr::mutate(BpS = stringr::str_split(BpS_Code, '_', simplify = TRUE)[,1]) %>%
    dplyr::select(-c(BpS_Code)) %>%
    dplyr::distinct()
  
  ## "Order of operations" for transferring land area between S-classes
  rules <- read.csv(paste0(dirRefCon, '/restorationRules.csv')) %>%
    dplyr::select(BpS_Code = LF_BpS_Code, Order:PassiveOnly) %>%
    dplyr::mutate(BpS = stringr::str_split(BpS_Code, '_', simplify = TRUE)[,1]) %>%
    dplyr::select(-c(BpS_Code)) %>%
    dplyr::distinct()
  
  
  ## Now process results for each of our 'strata'
  ## BPS
  processStrata(dirResults = dirResults,
                dat = read.csv(paste0(dirResults, '/sclass/annual/ORWA_BPS_', year, '.csv')),
                refcon = refCon,
                rules = rules,
                sclass = sclass,
                bps = BpS,
                prefix = paste0('annual/ORWA_BPS_', year),
                cores = cores,
                YEAR, BpS_Code)
  
  ## BPS x state
  processStrata(dirResults = dirResults,
                dat = read.csv(paste0(dirResults, '/sclass/annual/STATE_BPS_', year, '.csv')),
                refcon = refCon,
                rules = rules,
                sclass = sclass,
                bps = BpS,
                prefix = paste0('annual/STATE_BPS_', year),
                cores = cores,
                YEAR, STATE, BpS_Code)
  
  ## BPS x EW
  processStrata(dirResults = dirResults,
                dat = read.csv(paste0(dirResults, '/sclass/annual/ORWA_EW_BPS_', year, '.csv')),
                refcon = refCon,
                rules = rules,
                sclass = sclass,
                bps = BpS,
                prefix = paste0('annual/ORWA_EW_BPS_', year),
                cores = cores,
                YEAR, ew, BpS_Code)
  
  ## BPS x EW x STATE
  processStrata(dirResults = dirResults,
                dat = read.csv(paste0(dirResults, '/sclass/annual/STATE_EW_BPS_', year, '.csv')),
                refcon = refCon,
                rules = rules,
                sclass = sclass,
                bps = BpS,
                prefix = paste0('annual/STATE_EW_BPS_', year),
                cores = cores,
                YEAR, STATE, ew, BpS_Code)
  
  ## BPS x LLID
  processStrata(dirResults = dirResults,
                dat = read.csv(paste0(dirResults, '/sclass/annual/ORWA_BPS_LLID_', year, '.csv')),
                refcon = refCon,
                rules = rules,
                sclass = sclass,
                bps = BpS,
                prefix = paste0('annual/ORWA_BPS_LLID_', year),
                cores = cores,
                YEAR, BpS_Code, BPS_LLID)
  
  ## BPS x LLID x state
  processStrata(dirResults = dirResults,
                dat = read.csv(paste0(dirResults, '/sclass/annual/STATE_BPS_LLID_', year, '.csv')),
                refcon = refCon,
                rules = rules,
                sclass = sclass,
                bps = BpS,
                prefix = paste0('annual/STATE_BPS_LLID_', year),
                cores = cores,
                YEAR, STATE, BpS_Code, BPS_LLID)
  
  ## BPS x LLID x EW
  # processStrata(dirResults = dirResults,
  #               dat = read.csv(paste0(dirResults, '/sclass/annual/ORWA_EW_BPS_LLID', year, '.csv')),
  #               refcon = refCon,
  #               rules = rules,
  #               sclass = sclass,
  #               bps = BpS,
  #               prefix = paste0('annual/ORWA_EW_BPS_LLID', year),
  #               cores = cores,
  #               YEAR, ew, BpS_Code, BPS_LLID)
  # 
  # ## BPS x LLID x EW x STATE
  # processStrata(dirResults = dirResults,
  #               dat = read.csv(paste0(dirResults, '/sclass/annual/STATE_EW_BPS_LLID', year, '.csv')),
  #               refcon = refCon,
  #               rules = rules,
  #               sclass = sclass,
  #               bps = BpS,
  #               prefix = paste0('annual/STATE_EW_BPS_LLID', year),
  #               cores = cores,
  #               YEAR, STATE, ew, BpS_Code, BPS_LLID)
  
  
  
  
  # ## TI BPS
  # processStrata(dirResults = dirResults,
  #               dat = read.csv(paste0(dirResults, '/sclass/BPS_', year, '.csv')),
  #               refcon = refCon,
  #               rules = rules,
  #               sclass = sclass,
  #               bps = BpS,
  #               prefix = paste0('BPS_', year),
  #               cores = cores,
  #               BpS)
  # 
  # ## Annual BPS LLID
  # processStrata(dirResults = dirResults,
  #               dat = read.csv(paste0(dirResults, '/sclass/BPS_LLID_', year, '.csv')),
  #               refcon = refCon,
  #               rules = rules,
  #               sclass = sclass,
  #               bps = BpS,
  #               prefix = paste0('BPS_LLID_', year),
  #               cores = cores,
  #               BpS, BPS_LLID)
  
  
  cat('Restoration need estimates complete ...\n')
  cat('All done!\n')
  
}

# A helper function for above
processStrata <- function(dirResults, dat, refcon, rules, sclass, bps, prefix, cores, ...){
  
  ## ID all the columns we need to define strata
  strata <- dplyr::enquos(...)
  dat <- tidyr::unite(dat, col = 'strataID', !!! strata, remove = FALSE)
  
  ## ID our BPS col and sclass column
  sclass <- dplyr::enquo(sclass)
  bps <- dplyr::enquo(bps)
  dat <- dat %>%
    dplyr::mutate(SCLASS = !! sclass,
                  BpS = !! bps)
  
  ## Append reference conditions
  dat <- dat %>%
    # Make S-class NA's explicit
    tidyr::complete(tidyr::nesting(strataID, BpS), SCLASS, 
                    fill = list(AREA_TOTAL=0)) %>%
    dplyr::group_by(strataID) %>%
    ## Total forested area within the strata
    dplyr::mutate(AREA_STRATA = sum(AREA_TOTAL, na.rm = TRUE)) %>%
    ## Dropping non-forested strata
    dplyr::filter(AREA_STRATA > 0) %>%    
    dplyr::ungroup() %>%
    ## Join reference conditions
    dplyr::left_join(refcon, by = c('BpS', 'SCLASS')) %>%
    dplyr::mutate(dplyr::across(c(avg:high), .fns = function(x, total) {x * .01 * total}, total = .$AREA_STRATA)) 
  

  ## An iterator that identifies each "strata"
  x <- unique(dat$strataID)
  
  
  ## Run the algorithm in parallel, overkill for large units
  if (Sys.info()['sysname'] == 'Windows'){
    cl <- parallel::makeCluster(cores)
    parallel::clusterEvalQ(cl, {
      library(dplyr)
    })
    out <- parallel::parLapply(cl, X = x, fun = restArea, stratArea = dat, 
                               restRules = rules)
    parallel::stopCluster(cl) # Kill the cluster
    
  } else { # Multicore systems
    out <- parallel::mclapply(X = x, FUN = restArea, stratArea = dat, 
                              restRules = rules, mc.cores = cores)
  }
  

  ## Bring our results back into data.frames
  out <- unlist(out, recursive = FALSE)
  transfer <- dplyr::bind_rows(out[names(out) == 'transfer'])
  strata_new <- dplyr::bind_rows(out[names(out) == 'strata'])


  ## Save it all
  write.csv(transfer, paste0(dirResults,'restNeed/', prefix, '_transfers.csv'), row.names = FALSE)
  write.csv(strata_new, paste0(dirResults,'restNeed/', prefix, '_postRest.csv'), row.names = FALSE)
  
  suppressMessages({
    ## Make a cleaner dataset that shows prop need by type
    rn <- transfer %>%
      ungroup() %>%
      dplyr::mutate(type = dplyr::case_when(ActiveOnly == 1 ~ 'active',
                                            PassiveOnly == 1 ~ 'passive',
                                            TRUE ~ 'both')) %>%
      dplyr::select(strataID, type, REST_ACRES) %>%
      dplyr::group_by(strataID, type) %>%
      dplyr::summarize(REST_ACRES = sum(REST_ACRES, na.rm = TRUE)) %>%
      dplyr::left_join(dplyr::distinct(dplyr::select(dat, strataID, !!! strata, AREA_STRATA)), by = c('strataID')) %>%
      dplyr::mutate(REST_PCT = REST_ACRES / AREA_STRATA) %>%
      dplyr::ungroup() %>%
      tidyr::drop_na() %>%
      dplyr::select(-c(strataID))
  })

  write.csv(rn, paste0(dirResults,'/restNeed/', prefix, '_restNeed.csv'), row.names = FALSE)
  
}




## A function to implement the transfer procedures
transferArea <- function(strata, rr){
  
  ## We'll track transfers using the rules template
  transferList <- list()
  
  ## Loop over rules order
  for (i in seq_along(rr$Order)){
    
    ## Calculate departure for giving/receiving sclasses
    give <- strata %>%
      dplyr::filter(SCLASS == rr$Donate[i]) %>%
      dplyr::mutate(departure = dplyr::case_when(AREA_TOTAL < low ~ AREA_TOTAL - low,
                                                AREA_TOTAL > high ~ AREA_TOTAL - high,
                                                TRUE ~ 0))
    receive <- strata %>%
      dplyr::filter(SCLASS == rr$Receive[i]) %>%
      dplyr::mutate(departure = dplyr::case_when(AREA_TOTAL < low ~ AREA_TOTAL - low,
                                                 AREA_TOTAL > high ~ AREA_TOTAL - high,
                                                 TRUE ~ 0))
    
    ## Overabundant s-classes can ONLY give to underabundant s-classes
    if (give$departure > 0 & receive$departure < 0) {
      
      ## Our transfer tracker
      transfer <- rr[i,]
      
      ## Transfer the "minimum" of departed acres
      transfer$REST_ACRES <- min(give$departure, abs(receive$departure))
      
      ## Update strata areas
      strata[strata$SCLASS == give$SCLASS,"AREA_TOTAL"] <- give$AREA_TOTAL - transfer$REST_ACRES
      strata[strata$SCLASS == receive$SCLASS,"AREA_TOTAL"] <- receive$AREA_TOTAL + transfer$REST_ACRES
      
      ## Log the transfer
      transferList[[i]] <- transfer 
    } 
  } # End order loop
  
  ## Get our list of transfers back to data.frame
  transfer <- dplyr::bind_rows(transferList)
  
  
  ## Check departure again, sometimes we need to loop through twice becuase of 
  ## the order of transfers listed in restRules
  strata <- strata %>%
    dplyr::mutate(departure = dplyr::case_when(AREA_TOTAL < low ~ AREA_TOTAL - low,
                                               AREA_TOTAL > high ~ AREA_TOTAL - high,
                                               TRUE ~ 0))
  
  return(list(transfer = transfer, strata = strata))
}


## For each group defined by grpBy and strata, balance the acreage to fall
## within NRV reference conditions based on the rules set provided in
## 'restRules'
## This could be done much more elegantly, but who cares
restArea <- function(x, stratArea, restRules){
  
  ## This is dumb, but so is Windows. Can't find functions from global
  source(here::here('src/FIA/estimateRestNeed.R')) # Loading functions to pass to cluster
  
  ## Strata area info
  strata <- dplyr::filter(stratArea, strataID == x)
  
  ## Restoration ruleset for the BPS
  rr <- dplyr::filter(restRules, BpS %in% strata$BpS)
  
  
  ## Now we loop over the order of "treatments", track our 
  ## transfers, and update strata departures sequentially
  out <- transferArea(strata, rr)
  
  
  ## Add the landscape unit ID  
  out$transfer[['strataID']] <- x
  
  ## Save the tracked transfers
  return(list(transfer = out$transfer, strata = out$strata))
}


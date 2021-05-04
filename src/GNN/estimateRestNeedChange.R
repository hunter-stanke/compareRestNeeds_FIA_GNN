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
estimate_rest_needs_chng <- function(dirResults = here::here('results/FIA/'),
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
  
  
  
  ## Now process results for each of our 'strata' and estimation types
  ## BPS
  diff_rn_strata(dirResults = dirResults,
                 dat = read.csv(paste0(dirResults, '/sclassChng/ORWA_BPS_net.csv')),
                 refcon = refCon,
                 rules = rules,
                 sclass = sclass,
                 bps = BpS,
                 prefix = 'ORWA_BPS',
                 cores = cores,
                 YEAR, BpS)
  
  ## BPS x LLID
  diff_rn_strata(dirResults = dirResults,
                 dat = read.csv(paste0(dirResults, '/sclassChng/ORWA_BPS_LLID_net.csv')),
                 refcon = refCon,
                 rules = rules,
                 sclass = sclass,
                 bps = BpS,
                 prefix = 'ORWA_BPS_LLID',
                 cores = cores,
                 YEAR, BpS, BPS_LLID)
  
  ## BPS x state
  diff_rn_strata(dirResults = dirResults,
                 dat = read.csv(paste0(dirResults, '/sclassChng/STATE_BPS_net.csv')),
                 refcon = refCon,
                 rules = rules,
                 sclass = sclass,
                 bps = BpS,
                 prefix = 'STATE_BPS',
                 cores = cores,
                 YEAR, STATE, BpS)
  
  ## BPS x LLID x state
  diff_rn_strata(dirResults = dirResults,
                 dat = read.csv(paste0(dirResults, '/sclassChng/STATE_BPS_LLID_net.csv')),
                 refcon = refCon,
                 rules = rules,
                 sclass = sclass,
                 bps = BpS,
                 prefix = 'STATE_BPS_LLID',
                 cores = cores,
                 YEAR, STATE, BpS, BPS_LLID)

  
  
  cat('Restoration need estimates complete ...\n')
  cat('All done!\n')
  
}

diff_rn_strata <- function(dirResults, dat, refcon, rules, sclass, bps, prefix, cores, ...){
  
  ## The grouping columns
  strata <- dplyr::enquos(..., .named = TRUE)
  
  
  ## T1
  rn1 <- processStrata_chng(dirResults = dirResults,
                            dat = dat,
                            refcon = refcon,
                            rules = rules,
                            sclass = sclass,
                            bps = BpS,
                            prefix = prefix,
                            cores = cores,
                            areacol = PREV_AREA,
                            strata = strata)
  
  ## T2
  rn2 <- processStrata_chng(dirResults = dirResults,
                            dat = dat,
                            refcon = refcon,
                            rules = rules,
                            sclass = sclass,
                            bps = BpS,
                            prefix = prefix,
                            cores = cores,
                            areacol = (AREA_CHNG*18) + PREV_AREA,
                            strata = strata)
  
  ## Join cols
  jcols = c(names(strata), 'type')
  
  return(list(rn1, rn2, jcols))
  
  ## Compute change
  out <- dplyr::full_join(rn1, rn2, by = jcols, suffix = c('1', '2')) %>%
    ## Replace any NAs w/ 0
    dplyr::mutate(dplyr::across(.cols = everything(), .fns = tidyr::replace_na, 0)) #%>%
    dplyr::mutate(CHNG_REST_ACRES = REST_ACRES2 - REST_ACRES1,
                  CHNG_REST_PCT = REST_PCT2 - REST_PCT1,
                  CHNG_FOR_ACRES = AREA_STRATA2 - AREA_STRATA1) %>%
    select(!!! strata, type, CHNG_REST_ACRES, CHNG_REST_PCT, CHNG_FOR_ACRES,
           PREV_REST_ACRES = REST_ACRES1, PREV_REST_PCT = REST_PCT1,
           PREV_FOR_ACRES = AREA_STRATA1)
  
  
  ## Save the result  
  write.csv(out, paste0(dirResults,'/restNeedChng/', prefix, '_restNeed.csv'), row.names = FALSE)
  
}

# A helper function for above
processStrata_chng <- function(dirResults, dat, refcon, rules, sclass, bps, prefix, areacol, cores, strata){
  
  ## ID all the columns we need to define strata
  areacol <- dplyr::enquo(areacol)
  dat <- tidyr::unite(dat, col = 'strataID', !!! strata, remove = FALSE) %>%
    mutate(AREA_TOTAL = !! areacol)
  

  
  ## ID our BPS col and sclass column
  sclass <- dplyr::enquo(sclass)
  bps <- dplyr::enquo(bps)
  dat <- dat %>%
    dplyr::mutate(SCLASS = !! sclass,
                  BpS = !! bps)
  
  ## Append reference conditions
  dat <- dat %>%
    # Make S-class NA's explicit
    tidyr::complete(tidyr::nesting(strataID, YEAR, BpS), SCLASS, 
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
    out <- parallel::parLapply(cl, X = x, fun = restArea_chng, stratArea = dat, 
                               restRules = rules)
    parallel::stopCluster(cl) # Kill the cluster
    
  } else { # Multicore systems
    out <- parallel::mclapply(X = x, FUN = restArea_chng, stratArea = dat, 
                              restRules = rules, mc.cores = cores)
  }
  
  
  ## Bring our results back into data.frames
  out <- unlist(out, recursive = FALSE)
  transfer <- dplyr::bind_rows(out[names(out) == 'transfer'])
  strata_new <- dplyr::bind_rows(out[names(out) == 'strata'])
  
  
  ## Save it all
  #write.csv(transfer, paste0(dirResults,'/restNeed/', prefix, '_transfers.csv'), row.names = FALSE)
  #write.csv(strata_new, paste0(dirResults,'/restNeed/', prefix, '_postRest.csv'), row.names = FALSE)
  
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
  
  #write.csv(rn, paste0(dirResults,'/restNeed/', prefix, '_restNeed.csv'), row.names = FALSE)
  
  return(rn)
}




## A function to implement the transfer procedures
transferArea_chng <- function(strata, rr){
  
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
restArea_chng <- function(x, stratArea, restRules){
  
  ## This is dumb, but so is Windows. Can't find functions from global
  source(here::here('src/FIA/estimateRestNeedChange.R')) # Loading functions to pass to cluster
  
  ## Strata area info
  strata <- dplyr::filter(stratArea, strataID == x)
  
  ## Restoration ruleset for the BPS
  rr <- dplyr::filter(restRules, BpS %in% strata$BpS)
  
  
  ## Now we loop over the order of "treatments", track our 
  ## transfers, and update strata departures sequentially
  out <- transferArea_chng(strata, rr)
  
  
  ## Add the landscape unit ID  
  out$transfer[['strataID']] <- x
  
  ## Save the tracked transfers
  return(list(transfer = out$transfer, strata = out$strata))
}



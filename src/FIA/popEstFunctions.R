##=====================================================
##=====================================================
##
## Develop an rFIA-style function to estimate total
## forest land area within our 5 forest structural
## classes along with associated proportions and
## variances. Adapted directly from "area" in rFIA.
##
## Created:       28 October 2020 - Hunter Stanke
## Last modified: 28 October 2020 - Hunter Stanke
##
##====================================================
##====================================================

library(rFIA)
library(dplyr)
library(stringr)
library(tidyr)
library(parallel)
library(sp)
library(sf)

## Parallelizable spatial intersection
areal_par <- function(x, pltSF, polys){
  pltSF <- st_intersection(pltSF, polys[[x]]) %>%
    as.data.frame() %>%
    select(-c('geometry')) # removes artifact of SF object
}


## Helper for plot-level estimates
scHelper1 <- function(x, plts, db, grpBy){

  ## Selecting the plots for one county
  db$PLOT <- plts[[x]]


  ## Which grpByNames are in which table? Helps us subset below
  grpP <- names(db$PLOT)[names(db$PLOT) %in% grpBy]
  grpC <- names(db$COND)[names(db$COND) %in% grpBy & names(db$COND) %in% grpP == FALSE]

  ### Only joining tables necessary to produce plot level estimates, adjusted for non-response
  data <- db$PLOT %>%
    left_join(db$COND, by = c('PLT_CN'))

  ## Comprehensive indicator function
  data$aDI <- data$aD_p * data$aD_c * data$sp * data$landD

  suppressMessages({
    ### Plot-level estimates
    t <- data %>%
      ungroup() %>%
      ## Adding PROP_BASIS so we can handle adjustment factors at strata level
      distinct(PLT_CN, CONDID, .keep_all = TRUE) %>%
      group_by(PLT_CN, PROP_BASIS, sclass, .dots = grpBy) %>%
      summarize(fa = sum(CONDPROP_UNADJ * aDI, na.rm = TRUE),
                plotIn = ifelse(sum(aDI >  0, na.rm = TRUE), 1,0)) %>%
      ungroup() %>%
      mutate(scA = case_when(sclass == 'A' ~ fa, TRUE ~ 0),
             scB = case_when(sclass == 'B' ~ fa, TRUE ~ 0),
             scC = case_when(sclass == 'C' ~ fa, TRUE ~ 0),
             scD = case_when(sclass == 'D' ~ fa, TRUE ~ 0),
             scE = case_when(sclass == 'E' ~ fa, TRUE ~ 0),
             fa = case_when(is.na(sclass) ~ 0, TRUE ~ fa),
             plotIn = case_when(is.na(sclass) ~ 0, TRUE ~ plotIn)) %>%
      select(-c(sclass))
  })


  pltOut <- list(t = t)
  return(pltOut)

}


# Helper to take plot level to population level
scHelper2 <- function(x, popState, t, grpBy, method){

  ## DOES NOT MODIFY OUTSIDE ENVIRONMENT
  if (str_to_upper(method) %in% c("SMA", 'EMA', 'LMA', 'ANNUAL')) {
    grpBy <- c(grpBy, 'INVYR')
    #aGrpBy <- c(aGrpBy, 'INVYR')
    popState[[x]]$P2POINTCNT <- popState[[x]]$P2POINTCNT_INVYR
    popState[[x]]$p2eu <- popState[[x]]$p2eu_INVYR

  }

  suppressMessages({
    
    popX <- popState[[x]] %>%
      select(-c(STATECD)) %>%
      distinct(ESTN_UNIT_CN, STRATUM_CN, PLT_CN, .keep_all = TRUE)

    ## Total forest land area and associated variance, N
    tEst <- t %>%
      ungroup() %>%
      ## Rejoin with population tables
      right_join(popX, by = 'PLT_CN') %>%
      mutate(
        ## AREA
        aAdj = case_when(
          ## When NA, stay NA
          is.na(PROP_BASIS) ~ NA_real_,
          ## If the proportion was measured for a macroplot,
          ## use the macroplot value
          PROP_BASIS == 'MACR' ~ as.numeric(ADJ_FACTOR_MACR),
          ## Otherwise, use the subpplot value
          PROP_BASIS == 'SUBP' ~ ADJ_FACTOR_SUBP),
        fa = fa * aAdj,
        scA = scA * aAdj,
        scB = scB * aAdj,
        scC = scC * aAdj,
        scD = scD * aAdj,
        scE = scE * aAdj) %>%
      select(PLT_CN, all_of(grpBy), fa:scE, ESTN_UNIT_CN, STRATUM_CN, P2POINTCNT,
             AREA_USED, P1POINTCNT, P1PNTCNT_EU, p2eu) %>%
      dplyr::distinct() %>%
      ## FIA strata
      group_by(ESTN_UNIT_CN, STRATUM_CN, .dots = grpBy) %>%
      summarize(nh = first(P2POINTCNT),
                ## First portion of totals
                aStrat = sum(fa, na.rm = TRUE),
                scA_strat = sum(scA, na.rm = TRUE),
                scB_strat = sum(scB, na.rm = TRUE),
                scC_strat = sum(scC, na.rm = TRUE),
                scD_strat = sum(scD, na.rm = TRUE),
                scE_strat = sum(scE, na.rm = TRUE),
                ## Survey params
                plotIn_AREA = sum(plotIn, na.rm = TRUE),
                a = first(AREA_USED),
                w = first(P1POINTCNT) / first(P1PNTCNT_EU),
                p2eu = first(p2eu),
                ## First portion of variance
                av = sum(fa^2, na.rm = TRUE),
                scAv = sum(scA^2, na.rm = TRUE),
                scBv = sum(scB^2, na.rm = TRUE),
                scCv = sum(scC^2, na.rm = TRUE),
                scDv = sum(scD^2, na.rm = TRUE),
                scEv = sum(scE^2, na.rm = TRUE),
                # First portion of covariance
                scAcv = sum(scA*fa, na.rm = TRUE),
                scBcv = sum(scB*fa, na.rm = TRUE),
                scCcv = sum(scC*fa, na.rm = TRUE),
                scDcv = sum(scD*fa, na.rm = TRUE),
                scEcv = sum(scE*fa, na.rm = TRUE)) %>%
      # Total
      mutate(aStrat = aStrat / nh,
             scA = scA_strat / nh,
             scB = scB_strat / nh,
             scC = scC_strat / nh,
             scD = scD_strat / nh,
             scE = scE_strat / nh,
             # Variance
             adj = nh * (nh-1),
             av = (av - (nh * aStrat^2)) / adj,
             scAv = (scAv - (nh * scA^2)) / adj,
             scBv = (scBv - (nh * scB^2)) / adj,
             scCv = (scCv - (nh * scC^2)) / adj,
             scDv = (scDv - (nh * scD^2)) / adj,
             scEv = (scEv - (nh * scE^2)) / adj,
             # Covariance
             scAcv = (scAcv - (nh * scA * aStrat)) / adj,
             scBcv = (scBcv - (nh * scB * aStrat)) / adj,
             scCcv = (scCcv - (nh * scC * aStrat)) / adj,
             scDcv = (scDcv - (nh * scD * aStrat)) / adj,
             scEcv = (scEcv - (nh * scE * aStrat)) / adj) %>%
      ## Estimation unit
      group_by(ESTN_UNIT_CN, .dots = grpBy) %>%
      summarize(N = first(p2eu),
                ## Totals
                aEst = sum(aStrat * w, na.rm = TRUE) * a,
                scA = sum(scA * w, na.rm = TRUE) * a,
                scB = sum(scB * w, na.rm = TRUE) * a,
                scC = sum(scC * w, na.rm = TRUE) * a,
                scD = sum(scD * w, na.rm = TRUE) * a,
                scE = sum(scE * w, na.rm = TRUE) * a,
                ## Variances
                aVar = ((first(a)^2)/N) * (sum(w*nh*av, na.rm = TRUE) + sum((1-w)*(nh/N)*av, na.rm = TRUE)),
                scAv = ((first(a)^2)/N) * (sum(w*nh*scAv, na.rm = TRUE) + sum((1-w)*(nh/N)*scAv, na.rm = TRUE)),
                scBv = ((first(a)^2)/N) * (sum(w*nh*scBv, na.rm = TRUE) + sum((1-w)*(nh/N)*scBv, na.rm = TRUE)),
                scCv = ((first(a)^2)/N) * (sum(w*nh*scCv, na.rm = TRUE) + sum((1-w)*(nh/N)*scCv, na.rm = TRUE)),
                scDv = ((first(a)^2)/N) * (sum(w*nh*scDv, na.rm = TRUE) + sum((1-w)*(nh/N)*scDv, na.rm = TRUE)),
                scEv = ((first(a)^2)/N) * (sum(w*nh*scEv, na.rm = TRUE) + sum((1-w)*(nh/N)*scEv, na.rm = TRUE)),
                ## Covariances
                scAcv = ((first(a)^2)/N) * (sum(w*nh*scAcv, na.rm = TRUE) + sum((1-w)*(nh/N)*scAcv, na.rm = TRUE)),
                scBcv = ((first(a)^2)/N) * (sum(w*nh*scBcv, na.rm = TRUE) + sum((1-w)*(nh/N)*scBcv, na.rm = TRUE)),
                scCcv = ((first(a)^2)/N) * (sum(w*nh*scCcv, na.rm = TRUE) + sum((1-w)*(nh/N)*scCcv, na.rm = TRUE)),
                scDcv = ((first(a)^2)/N) * (sum(w*nh*scDcv, na.rm = TRUE) + sum((1-w)*(nh/N)*scDcv, na.rm = TRUE)),
                scEcv = ((first(a)^2)/N) * (sum(w*nh*scEcv, na.rm = TRUE) + sum((1-w)*(nh/N)*scEcv, na.rm = TRUE)),
                plotIn_AREA = sum(plotIn_AREA, na.rm = TRUE)) %>%
      distinct()
  })


  out <- list(tEst = tEst)

  return(out)
}


## Prep function that allows us to run remote objects
scStarter <- function(x,
                        db,
                        grpBy_quo = NULL,
                        sclass_quo = NULL,
                        polys = NULL,
                        returnSpatial = FALSE,
                        method = 'TI',
                        lambda = .5,
                        areaDomain = NULL,
                        nCores = 1,
                        remote,
                        mr){

  reqTables <- c('PLOT', 'COND', 'POP_PLOT_STRATUM_ASSGN', 'POP_ESTN_UNIT', 'POP_EVAL',
                 'POP_STRATUM', 'POP_EVAL_TYP', 'POP_EVAL_GRP')

  if (remote){
    ## Store the original parameters here
    params <- db

    ## Read in one state at a time
    db <- readFIA(dir = db$dir, common = db$common,
                  tables = reqTables, states = x, ## x is the vector of state names
                  nCores = nCores)

    ## If a clip was specified, run it now
    if ('mostRecent' %in% names(params)){
      db <- clipFIA(db, mostRecent = params$mostRecent,
                    mask = params$mask, matchEval = params$matchEval,
                    evalid = params$evalid, designCD = params$designCD,
                    nCores = nCores)
    }

  } else {

    ## Really only want the required tables
    db <- db[names(db) %in% reqTables]

  }

  ## Need a plotCN, and a new ID
  db$PLOT <- db$PLOT %>% mutate(PLT_CN = CN,
                                pltID = paste(UNITCD, STATECD, COUNTYCD, PLOT, sep = '_'),
                                sclass = !!sclass_quo)

  ##  don't have to change original code
  #grpBy_quo <- enquo(grpBy)

  # Probably cheating, but it works
  if (quo_name(grpBy_quo) != 'NULL'){

    ## Have to join tables to run select with this object type
    plt_quo <- filter(db$PLOT, !is.na(PLT_CN))
    ## We want a unique error message here to tell us when columns are not present in data
    d_quo <- tryCatch(
      error = function(cnd) {
        return(0)
      },
      plt_quo[10,] %>% # Just the first row
        left_join(select(db$COND, PLT_CN, names(db$COND)[names(db$COND) %in% names(db$PLOT) == FALSE]), by = 'PLT_CN') %>%
        select(!!grpBy_quo)
    )


    # If column doesnt exist, just returns 0, not a dataframe
    if (is.null(nrow(d_quo))){
      grpName <- quo_name(grpBy_quo)
      stop(paste('Columns', grpName, 'not found in PLOT, TREE, or COND tables. Did you accidentally quote the variables names? e.g. use grpBy = ECOSUBCD (correct) instead of grpBy = "ECOSUBCD". ', collapse = ', '))
    } else {
      # Convert to character
      grpBy <- names(d_quo)
    }

  } else {
    grpBy <- NULL
  }

  reqTables <- c('PLOT', 'COND', 'POP_PLOT_STRATUM_ASSGN', 'POP_ESTN_UNIT', 'POP_EVAL',
                 'POP_STRATUM', 'POP_EVAL_TYP', 'POP_EVAL_GRP')

  if (!is.null(polys) & first(class(polys)) %in% c('sf', 'SpatialPolygons', 'SpatialPolygonsDataFrame') == FALSE){
    stop('polys must be spatial polygons object of class sp or sf. ')
  }

  if (any(reqTables %in% names(db) == FALSE)){
    missT <- reqTables[reqTables %in% names(db) == FALSE]
    stop(paste('Tables', paste (as.character(missT), collapse = ', '), 'not found in object db.'))
  }
  if (str_to_upper(method) %in% c('TI', 'SMA', 'LMA', 'EMA', 'ANNUAL') == FALSE) {
    warning(paste('Method', method, 'unknown. Defaulting to Temporally Indifferent (TI).'))
  }


  # Save original grpBy for pretty return with spatial objects
  grpByOrig <- grpBy

  ## IF the object was clipped
  if ('prev' %in% names(db$PLOT)){
    ## Only want the current plots, no grm
    db$PLOT <- filter(db$PLOT, prev == 0)
  }


  ### AREAL SUMMARY PREP
  if(!is.null(polys)) {

    # # Add shapefile names to grpBy
    grpBy = c(grpBy, 'polyID')

    ## Make plot data spatial, projected same as polygon layer
    pltSF <- select(db$PLOT, c('LON', 'LAT', pltID)) %>%
      filter(!is.na(LAT) & !is.na(LON)) %>%
      distinct(pltID, .keep_all = TRUE)
    coordinates(pltSF) <- ~LON+LAT
    proj4string(pltSF) <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
    pltSF <- as(pltSF, 'sf') %>%
      st_transform(crs = st_crs(polys))

    ## Split up polys
    polyList <- split(polys, as.factor(polys$polyID))
    suppressWarnings({suppressMessages({
      ## Compute estimates in parallel -- Clusters in windows, forking otherwise
      if (Sys.info()['sysname'] == 'Windows'){
        cl <- makeCluster(nCores)
        clusterEvalQ(cl, {
          library(dplyr)
          library(stringr)
          library(rFIA)
        })
        out <- parLapply(cl, X = names(polyList), fun = areal_par, pltSF, polyList)
        #stopCluster(cl) # Keep the cluster active for the next run
      } else { # Unix systems
        out <- mclapply(names(polyList), FUN = areal_par, pltSF, polyList, mc.cores = nCores)
      }
    })})
    pltSF <- bind_rows(out)

    # A warning
    if (length(unique(pltSF$pltID)) < 1){
      stop('No plots in db overlap with polys.')
    }
    ## Add polygon names to PLOT
    db$PLOT <- db$PLOT %>%
      left_join(select(pltSF, polyID, pltID), by = 'pltID')

    # Test if any polygons cross state boundaries w/ different recent inventory years (continued w/in loop)
    if ('mostRecent' %in% names(db) & length(unique(db$POP_EVAL$STATECD)) > 1){
      mergeYears <- pltSF %>%
        right_join(select(db$PLOT, PLT_CN, pltID), by = 'pltID') %>%
        inner_join(select(db$POP_PLOT_STRATUM_ASSGN, c('PLT_CN', 'EVALID', 'STATECD')), by = 'PLT_CN') %>%
        inner_join(select(db$POP_EVAL, c('EVALID', 'END_INVYR')), by = 'EVALID') %>%
        group_by(polyID) %>%
        summarize(maxYear = max(END_INVYR, na.rm = TRUE))
    }
  }

  # update spatial domain indicator
  if(!is.null(polys)){
    db$PLOT$sp <- ifelse(db$PLOT$pltID %in% pltSF$pltID, 1, 0)
  } else {
    db$PLOT$sp <- 1
  }

  ## Forestland only
  db$COND$landD <- ifelse(db$COND$COND_STATUS_CD == 1, 1, 0)


  # User defined domain indicator for area (ex. specific forest type)
  pcEval <- left_join(db$PLOT, select(db$COND, -c('STATECD', 'UNITCD', 'COUNTYCD', 'INVYR', 'PLOT')), by = 'PLT_CN')
  #areaDomain <- substitute(areaDomain)
  pcEval$aD <- rlang::eval_tidy(areaDomain, pcEval) ## LOGICAL, THIS IS THE DOMAIN INDICATOR
  if(!is.null(pcEval$aD)) pcEval$aD[is.na(pcEval$aD)] <- 0 # Make NAs 0s. Causes bugs otherwise
  if(is.null(pcEval$aD)) pcEval$aD <- 1 # IF NULL IS GIVEN, THEN ALL VALUES TRUE
  pcEval$aD <- as.numeric(pcEval$aD)
  db$COND <- left_join(db$COND, select(pcEval, c('PLT_CN', 'CONDID', 'aD')), by = c('PLT_CN', 'CONDID')) %>%
    mutate(aD_c = aD)
  aD_p <- pcEval %>%
    group_by(PLT_CN) %>%
    summarize(aD_p = as.numeric(any(aD > 0)))
  db$PLOT <- left_join(db$PLOT, aD_p, by = 'PLT_CN')
  rm(pcEval)

  
  ### Snag the EVALIDs that are needed
  db$POP_EVAL <- db$POP_EVAL %>%
    select('CN', 'END_INVYR', 'EVALID', 'ESTN_METHOD', 'STATECD') %>%
    inner_join(select(db$POP_EVAL_TYP, c('EVAL_CN', 'EVAL_TYP')), by = c('CN' = 'EVAL_CN')) %>%
    filter( EVAL_TYP == 'EXPCURR') %>%
    filter(!is.na(END_INVYR) & !is.na(EVALID) & END_INVYR >= 2003) %>%
    distinct(END_INVYR, EVALID, .keep_all = TRUE)

  ## If a most-recent subset, make sure that we don't get two reporting years in
  ## western states
  if (mr) {
    db$POP_EVAL <- db$POP_EVAL %>%
      group_by(EVAL_TYP, STATECD) %>%
      filter(END_INVYR == max(END_INVYR, na.rm = TRUE)) %>%
      ungroup()
  }

  ## Cut STATECD
  db$POP_EVAL <- select(db$POP_EVAL, -c(STATECD))

  ### The population tables
  pops <- select(db$POP_EVAL, c('EVALID', 'ESTN_METHOD', 'CN', 'END_INVYR')) %>%
    rename(EVAL_CN = CN) %>%
    left_join(select(db$POP_ESTN_UNIT, c('CN', 'EVAL_CN', 'AREA_USED', 'P1PNTCNT_EU')), by = c('EVAL_CN')) %>%
    rename(ESTN_UNIT_CN = CN) %>%
    left_join(select(db$POP_STRATUM, c('ESTN_UNIT_CN', 'EXPNS', 'P2POINTCNT', 'CN', 'P1POINTCNT', 'ADJ_FACTOR_SUBP', 'ADJ_FACTOR_MICR', "ADJ_FACTOR_MACR")), by = c('ESTN_UNIT_CN')) %>%
    rename(STRATUM_CN = CN) %>%
    left_join(select(db$POP_PLOT_STRATUM_ASSGN, c('STRATUM_CN', 'PLT_CN', 'INVYR', 'STATECD')), by = 'STRATUM_CN') %>%
    mutate_if(is.factor,
              as.character) %>%
    dplyr::distinct()

  ### Which estimator to use?
  if (str_to_upper(method) %in% c('ANNUAL')){
    ## Want to use the year where plots are measured, no repeats
    ## Breaking this up into pre and post reporting becuase
    ## Estimation units get weird on us otherwise
    popOrig <- pops
    pops <- pops %>%
      group_by(STATECD) %>%
      filter(END_INVYR == INVYR) %>%
      ungroup()

    prePops <- popOrig %>%
      group_by(STATECD) %>%
      filter(INVYR < min(END_INVYR, na.rm = TRUE)) %>%
      distinct(PLT_CN, .keep_all = TRUE) %>%
      ungroup()

    pops <- bind_rows(pops, prePops) %>%
      mutate(YEAR = INVYR)

  } else {     # Otherwise temporally indifferent
    pops <- rename(pops, YEAR = END_INVYR)
  }

  ## P2POINTCNT column is NOT consistent for annual estimates, plots
  ## within individual strata and est units are related to different INVYRs
  p2_INVYR <- pops %>%
    ungroup() %>%
    group_by(ESTN_UNIT_CN, STRATUM_CN, INVYR) %>%
    summarize(P2POINTCNT_INVYR = length(unique(PLT_CN))) %>%
    ungroup()
  ## Want a count of p2 points / eu, gets screwed up with grouping below
  p2eu_INVYR <- p2_INVYR %>%
    ungroup() %>%
    distinct(ESTN_UNIT_CN, STRATUM_CN, INVYR, .keep_all = TRUE) %>%
    group_by(ESTN_UNIT_CN, INVYR) %>%
    summarize(p2eu_INVYR = sum(P2POINTCNT_INVYR, na.rm = TRUE)) %>%
    ungroup()
  p2eu <- pops %>%
    ungroup() %>%
    distinct(ESTN_UNIT_CN, STRATUM_CN, .keep_all = TRUE) %>%
    group_by(ESTN_UNIT_CN) %>%
    summarize(p2eu = sum(P2POINTCNT, na.rm = TRUE)) %>%
    ungroup()

  ## Rejoin
  pops <- pops %>%
    left_join(p2_INVYR, by = c('ESTN_UNIT_CN', 'STRATUM_CN', 'INVYR')) %>%
    left_join(p2eu_INVYR, by = c('ESTN_UNIT_CN', 'INVYR')) %>%
    left_join(p2eu, by = 'ESTN_UNIT_CN')


  ## Recode a few of the estimation methods to make things easier below
  pops$ESTN_METHOD = recode(.x = pops$ESTN_METHOD,
                            `Post-Stratification` = 'strat',
                            `Stratified random sampling` = 'strat',
                            `Double sampling for stratification` = 'double',
                            `Simple random sampling` = 'simple',
                            `Subsampling units of unequal size` = 'simple')
  
  
  ## When something other than temporally indifferent is used, we may need to merge small strata
  ## There is no great way to go about this, but very important we do it for variance issues. 
  ## So, what we will do is: 
  ## (1) Identify strata/INVYR pairs with less than 6 ground plots
  ## (2) For each of those pairs, identify their most similar neighbor based on fuzzy string matching of STRATUM Descriptions
  ## (3) Combine the small strata with their most similar pairs until every stratum/INVYR pair reaches the minimum sample size
  
  if (str_to_upper(method) != 'TI'){
    
    ## Stratum year pairs
    stratYr <- pops %>%
      left_join(select(db$POP_STRATUM, CN, STRATUM_DESCR), by = c('STRATUM_CN' = 'CN')) %>%
      distinct(ESTN_UNIT_CN, STRATUM_CN, STRATUM_DESCR, INVYR, P2POINTCNT_INVYR) %>%
      ## If buffer is present in the name, then the stratum has a different intensity
      ## than other strata in the same estimation unit, which is bullshit, but it's 
      ## what has been done in the PNW. Only combine buffer w/ buffer
      mutate(buff = str_detect(STRATUM_DESCR, 'buff'))
    
    ## Small strata
    strat <- stratYr %>%
      group_by(ESTN_UNIT_CN, STRATUM_CN, STRATUM_DESCR, buff) %>%
      summarise(small = if_else(any(P2POINTCNT_INVYR < 6), 1, 0)) %>%
      ungroup()
    
    
    ## Return STRATUM_CN of most similar neighbor
    datList <- list()
    for ( i in strat$STRATUM_CN ) {
      
      ## Subset the row
      dat <- filter(strat, STRATUM_CN == i)
      
      if (dat$small == 1) {
        
        ## Find its nearest neighbor of those in the same estimation
        ## unit and sampling intensity
        neighbors <- strat %>%
          filter(ESTN_UNIT_CN == dat$ESTN_UNIT_CN) %>%
          filter(buff == dat$buff) %>%
          filter(STRATUM_CN != i)
        
        if (nrow(neighbors) > 1) {
          ## Find the most similar neighbor in terms of stratum description
          msn <- adist(dat$STRATUM_DESCR, neighbors$STRATUM_DESCR)
          
          dat$new <- neighbors$STRATUM_CN[which.min(msn)]
          
        } else {
          dat$new <- NA
        }
        

      } else {
        dat$new <- NA
      }
      
      datList[[as.character(i)]] <- dat
    }
    
    test <- bind_rows(datList)

      
    ## Find the most similar pair for every plot
    
    ## Get all pairs, their counts by INVYR and counts per cycle
    allPairs <- pops %>%
      left_join(select(db$PLOT, PLT_CN, INTENSITY), by = c('PLT_CN')) %>%
      left_join(select(db$POP_STRATUM, CN, STRATUM_DESCR), by = c('STRATUM_CN' = 'CN')) %>%
      distinct(ESTN_UNIT_CN, STRATUM_CN, INVYR, P2POINTCNT, P2POINTCNT_INVYR, INTENSITY, STRATUM_DESCR) %>%
      mutate(newStrat = STRATUM_CN,
             newP2 = P2POINTCNT_INVYR,
             ## PNW is weird, so handle the buffer
             ## Here we're assuming everything other than 1 and 201 is the 
             ## same intensity. This shouldn't matter becuase intensity is the 
             ## same within estimation units outside of NFS lands
             INTENSITY = case_when(INTENSITY %in% c(1, 201) ~ 1,
                                   TRUE ~ 2))
    
    out <- select(allPairs, STRATUM_CN, INVYR, newStrat, newP2)
    
    while (any(out$newP2 < 6)) {
      
      ## These are the strata that we need to rename
      stratID <- unique(out$newStrat[out$newP2 < 6])
      newID <- c()
      
      ## Loop over small strata and merge
      for (i in stratID) {
        
        dat <- filter(allPairs, newStrat == i)
        
        ## Find its nearest neighbor of those in the same estimation
        ## unit and sampling intensity
        neighbors <- allPairs %>%
          distinct(ESTN_UNIT_CN, newStrat, INTENSITY, STRATUM_DESCR) %>%
          filter(ESTN_UNIT_CN == dat$ESTN_UNIT_CN[1]) %>%
          filter(INTENSITY == dat$INTENSITY[1]) %>%
          filter(newStrat != i)
        
        ## Break the loop if things go bad
        if (nrow(neighbors) < 1) {
          
          ## Ignore intensity and see if that fixes
          ## This is NOT the right way of going about it. Temporary fix with minor consequences
          neighbors <- allPairs %>%
            filter(ESTN_UNIT_CN == dat$ESTN_UNIT_CN[1]) #%>%
          #filter(INTENSITY == dat$INTENSITY) %>%
          filter(newStrat != dat$newStrat[1])
          
          ## If still a no go, things are bad
          if (nrow(neighbors) < 1) {
            stop('Stratum/INVYR sample size too low. No strata to combine with same intensity.')
          }
        }
        
        ## Otherwise, find the most similar neighbor in terms of stratum description
        msn <- adist(dat$STRATUM_DESCR[1], neighbors$STRATUM_DESCR)
        
        ## Want to save the new assignment
        newID <- c(newID, neighbors$newStrat[which.min(msn)])
        
      }
      
      test <- data.frame(old = stratID, new = newID)
      
      
      test1 <- out %>%
        left_join(test, by = c('newStrat' = 'old')) %>%
        mutate(newStrat = case_when(is.na(new) ~ newStrat,
                                    TRUE ~ new)) %>%
        select(-c(new)) %>%
        left_join(select(out, -c(STRATUM_CN)))
      
      
      old <- filter(out, newStrat %in% stratID)
      new <- filter()
      
    }
    
    ## Update new stratum cn and P2point count
    ## Make sure we update all cases in the stratum
    updateStrat <- allPairs %>%
      ungroup() %>%
      filter(newStrat == neighbors$newStrat[which.min(msn)]) %>%
      dplyr::select(newStrat, INVYR, newP2.update = newP2)
    
    dat$newStrat <- updateStrat$newStrat[1]
    
    
    
    
    iter = 1 ## This will be used to loop over the rows in allPairs
    stratID <- unique(allPairs$STRATUM_CN)
    
    
    
    ## Using a while loop while we combine strata
    while (any(allPairs$newP2 < 6)) {
      
      ## Grab just the STRATUM of interest
      dat <- filter(allPairs, newStrat == stratID[iter])
      
      ## Does this one need a new home?
      if (any(dat$newP2 < 6) & nrow(dat) > 0) {
        
        ## If yes, find its nearest neighbor of those in the same estimation
        ## unit and sampling intensity
        neighbors <- allPairs %>%
          distinct(ESTN_UNIT_CN, newStrat, INTENSITY, STRATUM_DESCR) %>%
          filter(ESTN_UNIT_CN == dat$ESTN_UNIT_CN[1]) %>%
          filter(INTENSITY == dat$INTENSITY[1]) %>%
          filter(newStrat != dat$newStrat[1])
          
        
        ## Break the loop if things go bad
        if (nrow(neighbors) < 1) {
          
          ## Ignore intensity and see if that fixes
          ## This is NOT the right way of going about it. Temporary fix with minor consequences
          neighbors <- allPairs %>%
            filter(ESTN_UNIT_CN == dat$ESTN_UNIT_CN[1]) #%>%
          #filter(INTENSITY == dat$INTENSITY) %>%
          filter(newStrat != dat$newStrat[1])
          
          ## If still a no go, things are bad
          if (nrow(neighbors) < 1) {
            stop('Stratum/INVYR sample size too low. No strata to combine with same intensity.')
          }
        }
        
        ## Otherwise, find the most similar neighbor in terms of stratum description
        msn <- adist(dat$STRATUM_DESCR[1], neighbors$STRATUM_DESCR)
        
        ## Update new stratum cn and P2point count
        ## Make sure we update all cases in the stratum
        updateStrat <- allPairs %>%
          ungroup() %>%
          filter(newStrat == neighbors$newStrat[which.min(msn)]) %>%
          dplyr::select(newStrat, INVYR, newP2.update = newP2)
        
        ## Do the update
        dat <- dat %>%
          mutate(newStrat = updateStrat$newStrat[1]) %>%
          left_join(updateStrat, by = c('newStrat', 'INVYR')) %>%
          mutate(newP2 = replace_na(.$newP2, 0),
                 newP2.update = replace_na(.$newP2.update, 0),
                 newP2 = newP2 + newP2.update) %>%
          select(-c(newP2.update))
        
        ## Now update allPairs
        allPairs <- allPairs %>%
          filter(newStrat != stratID[iter]) %>%
          bind_rows(dat)
      
        
      } # Otherwise, go to the next row
      
      
      ## If at the end of the list and need to re-loop, fix the iterator
      iter <- ifelse(iter < length(stratID), iter + 1, 1)

    }
    
    ## Now we need to update pops based on our new combinations of strata
    test <- pops %>%
      left_join(distinct(allPairs, STRATUM_CN, newStrat), by = c('STRATUM_CN'))
  }



  ## Only the necessary plots for EVAL of interest
  db$PLOT <- filter(db$PLOT, PLT_CN %in% pops$PLT_CN)



  ## Which grpByNames are in which table? Helps us subset below
  grpP <- names(db$PLOT)[names(db$PLOT) %in% grpBy]
  grpC <- names(db$COND)[names(db$COND) %in% grpBy & names(db$COND) %in% grpP == FALSE]
  grpT <- names(db$TREE)[names(db$TREE) %in% grpBy & names(db$TREE) %in% c(grpP, grpC) == FALSE]

  ### Only joining tables necessary to produce plot level estimates, adjusted for non-response
  db$PLOT <- select(db$PLOT, c('PLT_CN', 'STATECD', 'COUNTYCD', 'MACRO_BREAKPOINT_DIA', 'INVYR', 'MEASYEAR', 'PLOT_STATUS_CD', grpP, 'aD_p', 'sp', sclass))
  db$COND <- select(db$COND, c('PLT_CN', 'CONDPROP_UNADJ', 'PROP_BASIS', 'COND_STATUS_CD', 'CONDID', grpC, 'aD_c', landD))

  ## Merging state and county codes
  plts <- split(db$PLOT, as.factor(paste(db$PLOT$COUNTYCD, db$PLOT$STATECD, sep = '_')))

  suppressWarnings({
    ## Compute estimates in parallel -- Clusters in windows, forking otherwise
    if (Sys.info()['sysname'] == 'Windows'){
      cl <- makeCluster(nCores)
      clusterEvalQ(cl, {
        library(dplyr)
        library(stringr)
        library(rFIA)
      })
      out <- parLapply(cl, X = names(plts), fun = scHelper1, plts, db, grpBy)
      #stopCluster(cl) # Keep the cluster active for the next run
    } else { # Unix systems
      out <- mclapply(names(plts), FUN = scHelper1, plts, db, grpBy, mc.cores = nCores)
    }
  })


  ## back to dataframes
  out <- unlist(out, recursive = FALSE)
  t <- bind_rows(out[names(out) == 't'])


  ## Adding YEAR to groups
  grpBy <- c('YEAR', grpBy)


  popState <- split(pops, as.factor(pops$STATECD))

  suppressWarnings({
    ## Compute estimates in parallel -- Clusters in windows, forking otherwise
    if (Sys.info()['sysname'] == 'Windows'){
      out <- parLapply(cl, X = names(popState), fun = scHelper2, popState, t, grpBy, method)
      stopCluster(cl)
    } else { # Unix systems
      out <- mclapply(names(popState), FUN = scHelper2, popState, t, grpBy, method, mc.cores = nCores)
    }
  })
  ## back to dataframes
  out <- unlist(out, recursive = FALSE)
  tEst <- bind_rows(out[names(out) == 'tEst'])


  ##### ----------------- MOVING AVERAGES
  if (str_to_upper(method) %in% c("SMA", 'EMA', 'LMA')){
    ### ---- SIMPLE MOVING AVERAGE
    if (str_to_upper(method) == 'SMA'){
      ## Assuming a uniform weighting scheme
      wgts <- pops %>%
        group_by(ESTN_UNIT_CN) %>%
        summarize(wgt = 1 / length(unique(INVYR)))

      tEst <- left_join(tEst, wgts, by = 'ESTN_UNIT_CN')

      #### ----- Linear MOVING AVERAGE
    } else if (str_to_upper(method) == 'LMA'){
      wgts <- pops %>%
        distinct(YEAR, ESTN_UNIT_CN, INVYR, .keep_all = TRUE) %>%
        arrange(YEAR, ESTN_UNIT_CN, INVYR) %>%
        group_by(as.factor(YEAR), as.factor(ESTN_UNIT_CN)) %>%
        mutate(rank = min_rank(INVYR))

      ## Want a number of INVYRs per EU
      neu <- wgts %>%
        group_by(ESTN_UNIT_CN) %>%
        summarize(n = sum(rank, na.rm = TRUE))

      ## Rejoining and computing wgts
      wgts <- wgts %>%
        left_join(neu, by = 'ESTN_UNIT_CN') %>%
        mutate(wgt = rank / n) %>%
        ungroup() %>%
        select(ESTN_UNIT_CN, INVYR, wgt)

      tEst <- left_join(tEst, wgts, by = c('ESTN_UNIT_CN', 'INVYR'))

      #### ----- EXPONENTIAL MOVING AVERAGE
    } else if (str_to_upper(method) == 'EMA'){
      wgts <- pops %>%
        distinct(YEAR, ESTN_UNIT_CN, INVYR, .keep_all = TRUE) %>%
        arrange(YEAR, ESTN_UNIT_CN, INVYR) %>%
        group_by(as.factor(YEAR), as.factor(ESTN_UNIT_CN)) %>%
        mutate(rank = min_rank(INVYR))


      if (length(lambda) < 2){
        ## Want sum of weighitng functions
        neu <- wgts %>%
          mutate(l = lambda) %>%
          group_by(ESTN_UNIT_CN) %>%
          summarize(l = 1-first(lambda),
                    sumwgt = sum(l*(1-l)^(1-rank), na.rm = TRUE))

        ## Rejoining and computing wgts
        wgts <- wgts %>%
          left_join(neu, by = 'ESTN_UNIT_CN') %>%
          mutate(wgt = l*(1-l)^(1-rank) / sumwgt) %>%
          ungroup() %>%
          select(ESTN_UNIT_CN, INVYR, wgt)
      } else {
        grpBy <- c('lambda', grpBy)
        #aGrpBy <- c('lambda', aGrpBy)
        ## Duplicate weights for each level of lambda
        yrWgts <- list()
        for (i in 1:length(unique(lambda))) {
          yrWgts[[i]] <- mutate(wgts, lambda = lambda[i])
        }
        wgts <- bind_rows(yrWgts)
        ## Want sum of weighitng functions
        neu <- wgts %>%
          group_by(lambda, ESTN_UNIT_CN) %>%
          summarize(l = 1-first(lambda),
                    sumwgt = sum(l*(1-l)^(1-rank), na.rm = TRUE))

        ## Rejoining and computing wgts
        wgts <- wgts %>%
          left_join(neu, by = c('lambda', 'ESTN_UNIT_CN')) %>%
          mutate(wgt = l*(1-l)^(1-rank) / sumwgt) %>%
          ungroup() %>%
          select(lambda, ESTN_UNIT_CN, INVYR, wgt)
      }

      tEst <- left_join(tEst, wgts, by = c('ESTN_UNIT_CN', 'INVYR'))

    }

    ### Applying the weights
    tEst <- tEst %>%
      mutate_at(vars(aEst:scE), ~(.*wgt)) %>%
      mutate_at(vars(aVar:scEcv), ~(.*(wgt^2))) %>%
      group_by(ESTN_UNIT_CN, .dots = grpBy) %>%
      summarize_at(vars(aEst:plotIn_AREA), sum, na.rm = TRUE)
  }

  out <- list(tEst = tEst, grpBy = grpBy, grpByOrig = grpByOrig)


  return(out)

}



## Main function that should be used
scArea <- function(db,
                   grpBy = NULL,
                   sclass = NULL,
                   polys = NULL,
                   returnSpatial = FALSE,
                   method = 'TI',
                   lambda = .5,
                   areaDomain = NULL,
                   nCores = 1) {

  ##  don't have to change original code
  grpBy_quo <- rlang::enquo(grpBy)
  areaDomain <- rlang::enquo(areaDomain)
  sclass_quo <- rlang::enquo(sclass)

  ### Is DB remote?
  remote <- ifelse(class(db) == 'Remote.FIA.Database', 1, 0)
  if (remote){

    iter <- db$states

    ## In memory
  } else {
    ## Some warnings
    if (class(db) != "FIA.Database"){
      stop('db must be of class "FIA.Database". Use readFIA() to load your FIA data.')
    }

    ## an iterator for remote
    iter <- 1

  }

  ## Check for a most recent subset
  if (remote){
    if ('mostRecent' %in% names(db)){
      mr = db$mostRecent # logical
    } else {
      mr = FALSE
    }
    ## In-memory
  } else {
    if ('mostRecent' %in% names(db)){
      mr = TRUE
    } else {
      mr = FALSE
    }
  }

  ### AREAL SUMMARY PREP
  if(!is.null(polys)) {
    # Convert polygons to an sf object
    polys <- polys %>%
      as('sf')%>%
      mutate_if(is.factor,
                as.character)
    ## A unique ID
    polys$polyID <- 1:nrow(polys)
  }



  ## Run the main portion
  out <- lapply(X = iter, FUN = scStarter, db,
                grpBy_quo = grpBy_quo, sclass_quo, polys, returnSpatial,
                method, lambda, areaDomain,
                nCores, remote, mr)
  ## Bring the results back
  out <- unlist(out, recursive = FALSE)
  tEst <- bind_rows(out[names(out) == 'tEst'])
  grpBy <- out[names(out) == 'grpBy'][[1]]
  grpByOrig <- out[names(out) == 'grpByOrig'][[1]]



  suppressMessages({suppressWarnings({
    ## If a clip was specified, handle the reporting years
    if (mr){
      ## If a most recent subset, ignore differences in reporting years across states
      ## instead combine most recent information from each state
      # ID mr years by group
      maxyearsT <- tEst %>%
        select(grpBy) %>%
        group_by(.dots = grpBy[!c(grpBy %in% 'YEAR')]) %>%
        summarise(YEAR = max(YEAR, na.rm = TRUE))

      # Combine estimates
      tEst <- tEst %>%
        ungroup() %>%
        select(-c(YEAR)) %>%
        left_join(maxyearsT, by = grpBy[!c(grpBy %in% 'YEAR')])

    }
  })})


  ##---------------------  TOTALS and RATIOS
  # Area
  tTotal <- tEst %>%
    ungroup() %>%
    group_by(.dots = grpBy) %>%
    summarize_all(sum,na.rm = TRUE)





  suppressWarnings({
    ## Bring them together
    tOut <- tTotal %>%
      mutate(AREA_TOTAL = aEst,
             AREA_TOTAL_VAR = aVar,
             A_TOTAL = scA,
             B_TOTAL = scB,
             C_TOTAL = scC,
             D_TOTAL = scD,
             E_TOTAL = scE,
             A_VAR = scAv,
             B_VAR = scBv,
             C_VAR = scCv,
             D_VAR = scDv,
             E_VAR = scEv,
             A_PCT = scA / aEst,
             B_PCT = scB / aEst,
             C_PCT = scC / aEst,
             D_PCT = scD / aEst,
             E_PCT = scE / aEst,
             A_PCT_VAR = (1/AREA_TOTAL^2) * (A_VAR + (A_PCT^2 * aVar) - (2 * A_PCT * scAcv)),
             B_PCT_VAR = (1/AREA_TOTAL^2) * (B_VAR + (B_PCT^2 * aVar) - (2 * B_PCT * scBcv)),
             C_PCT_VAR = (1/AREA_TOTAL^2) * (C_VAR + (C_PCT^2 * aVar) - (2 * C_PCT * scCcv)),
             D_PCT_VAR = (1/AREA_TOTAL^2) * (D_VAR + (D_PCT^2 * aVar) - (2 * D_PCT * scDcv)),
             E_PCT_VAR = (1/AREA_TOTAL^2) * (E_VAR + (E_PCT^2 * aVar) - (2 * E_PCT * scEcv)),
             nPlots = plotIn_AREA) %>%
      select(grpBy, AREA_TOTAL: nPlots, N)
  })


  ## Would prefer output in a tidy format, so this is a but clunky but whatever
  aBase <- select(tOut, YEAR, AREA_TOTAL, AREA_TOTAL_VAR, nPlots, N, all_of(grpBy))
  sTotal <- select(tOut, YEAR, all_of(grpBy), A_TOTAL:E_TOTAL) %>%
    tidyr::pivot_longer(A_TOTAL:E_TOTAL, names_to = 'SCLASS', values_to = 'AREA') %>%
    mutate(SCLASS = stringr::str_split(SCLASS, '_', simplify = TRUE)[,1])
  sTotalVar <- select(tOut, YEAR, all_of(grpBy), A_VAR:E_VAR)  %>%
    tidyr::pivot_longer(A_VAR:E_VAR, names_to = 'SCLASS', values_to = 'AREA_VAR') %>%
    mutate(SCLASS = stringr::str_split(SCLASS, '_', simplify = TRUE)[,1])
  sPct <- select(tOut, YEAR, all_of(grpBy), A_PCT:E_PCT) %>%
    tidyr::pivot_longer(A_PCT:E_PCT, names_to = 'SCLASS', values_to = 'AREA_PCT') %>%
    mutate(SCLASS = stringr::str_split(SCLASS, '_', simplify = TRUE)[,1])
  sPctVar <- select(tOut, YEAR, all_of(grpBy), A_PCT_VAR:E_PCT_VAR) %>%
    tidyr::pivot_longer(A_PCT_VAR:E_PCT_VAR, names_to = 'SCLASS', values_to = 'AREA_PCT_VAR') %>%
    mutate(SCLASS = stringr::str_split(SCLASS, '_', simplify = TRUE)[,1])
  tOut <- sTotal %>%
    left_join(sTotalVar, by = c('YEAR', 'SCLASS', grpBy[grpBy != 'YEAR'])) %>%
    left_join(sPct, by = c('YEAR', 'SCLASS', grpBy[grpBy != 'YEAR'])) %>%
    left_join(sPctVar, by = c('YEAR', 'SCLASS', grpBy[grpBy != 'YEAR'])) %>%
    left_join(aBase, by = c('YEAR', grpBy[grpBy != 'YEAR'])) %>%
    mutate(AREA_PCT_PM = qt(.975, df = N - 1) * (sqrt(AREA_PCT_VAR) / sqrt(N)),
           AREA_PM = qt(.975, df = N - 1) * (sqrt(AREA_VAR) / sqrt(N)),
           AREA_TOTAL_PM = qt(.975, df = N - 1) * (sqrt(AREA_TOTAL_VAR) / sqrt(N))) %>%
    select(YEAR, all_of(grpBy), SCLASS,
           AREA_PCT, AREA, AREA_TOTAL,
           AREA_PCT_PM, AREA_PM, AREA_TOTAL_PM,
           AREA_PCT_VAR, AREA_VAR, AREA_TOTAL_VAR,
           nPlots, N)


  # Snag the names
  tNames <- names(tOut)[names(tOut) %in% grpBy == FALSE]

  ## Pretty output
  tOut <- tOut %>%
    ungroup() %>%
    mutate_if(is.factor, as.character) %>%
    drop_na(grpBy) %>%
    arrange(YEAR) %>%
    as_tibble()

  # Return a spatial object
  if (!is.null(polys)) {
    ## NO IMPLICIT NA
    nospGrp <- unique(grpBy[grpBy %in% c('SPCD', 'SYMBOL', 'COMMON_NAME', 'SCIENTIFIC_NAME') == FALSE])
    nospSym <- syms(nospGrp)
    tOut <- complete(tOut, !!!nospSym)
    ## If species, we don't want unique combos of variables related to same species
    ## but we do want NAs in polys where species are present
    if (length(nospGrp) < length(grpBy)){
      spGrp <- unique(grpBy[grpBy %in% c('SPCD', 'SYMBOL', 'COMMON_NAME', 'SCIENTIFIC_NAME')])
      spSym <- syms(spGrp)
      tOut <- complete(tOut, nesting(!!!nospSym))
    }

    suppressMessages({suppressWarnings({tOut <- left_join(tOut, polys) %>%
      select(c('YEAR', grpByOrig, tNames, names(polys))) %>%
      filter(!is.na(polyID) & !is.na(nPlots))})})

    ## Makes it horrible to work with as a dataframe
    if (returnSpatial == FALSE) tOut <- select(tOut, -c(geometry))
  }


  ## Above converts to tibble
  if (returnSpatial) tOut <- st_sf(tOut)

  return(tOut)
}

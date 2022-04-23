
## Simple function to merge annual GNN files into a single file
## ----------------------------------------------------------------------------
## dirResults (character): directory where results are output
merge_annual_files <- function(dirResults = here::here('results/GNN/')) {
  

  ## SCLASS --------------------------------------------------------------------
  
  # All files in the directory
  files <- list.files(here::here(paste0(dirResults, 'sclass/annual/')))
  sub.files <- stringr::str_sub(files, 1, -10)
  
  # All unique file prefixes
  prefix <- c('ORWA', 'ORWA_EW', 'ORWA_BPS', 'ORWA_BPS_LLID',
              'STATE', 'STATE_EW', 'STATE_BPS', 'STATE_BPS_LLID')
  
  for (p in prefix) {
    
    ## Which files are associated w/ each prefix
    p.files <- files[sub.files == p]
    
    ## Read them in, store in list, merge, and save
    out.list <- list()
    for (f in p.files) {
      out.list[[f]] <- read.csv(here::here(paste0(dirResults, 'sclass/annual/', f)))
    }
    out <- bind_rows(out.list)
    write.csv(out, here::here(paste0(dirResults, 'sclass/', p, '.csv')), row.names = FALSE)
  }
  
  
  
  ## RN ------------------------------------------------------------------------
  
  # All files in the directory
  files <- list.files(here::here(paste0(dirResults, 'restNeed/annual/')))

  # All unique file prefixes
  prefix <- c('ORWA_BPS', 'ORWA_EW_BPS', 'ORWA_BPS_LLID',
              'STATE_BPS', 'STATE_EW_BPS', 'STATE_BPS_LLID')
  suffix <- c('_postRest.csv', '_restNeed.csv', '_transfers.csv')
  
  for (s in suffix) {
    
    ## Drop suffix, cut wrong suffix, and drop year to select files
    sub.files <- stringr::str_remove(files, s)
    sub.files <- stringr::str_sub(sub.files, 1, -6)
    
    for (p in prefix) {
      
      ## Which files are associated w/ each prefix
      p.files <- files[sub.files == p]
      
      ## Read them in, store in list, merge, and save
      out.list <- list()
      for (f in p.files) {
        out.list[[f]] <- read.csv(here::here(paste0(dirResults, 'restNeed/annual/', f)))
      }
      out <- bind_rows(out.list)
      write.csv(out, here::here(paste0(dirResults, 'restNeed/', p, s)), row.names = FALSE)
    }
  }
}


rm(list = ls())

{
  load("C:/Users/wjlove/My Projects/contact/data-raw/calves2018_raw.RData")
  
} # LOAD RAW DATA

{
  master_function <- function(
    X, #Location data
    agg_t #Time interval to aggregate position data
    )
  {
    {
      calc_dist_matrix <- function() {
        fxn_expr <- expression({
          for(t in levels(X_cleaned$t_bin)) {
            
            loc.df <- merge(
              data.frame(ID = id_set),
              X_cleaned[X_cleaned$t_bin == t,c(1,3,4)],
              all.x = T
            )
            
            xloc_mat <- matrix(loc.df$xloc, ncol = dim(loc.df)[1], nrow = dim(loc.df)[1], dimnames = list(loc.df[[1]], loc.df[[1]]))
            yloc_mat <- matrix(loc.df$yloc, ncol = dim(loc.df)[1], nrow = dim(loc.df)[1], dimnames = list(loc.df[[1]], loc.df[[1]]))

            dt_array[,,t] <- sqrt((xloc_mat - t(xloc_mat))^2 + (yloc_mat - t(yloc_mat))^2)
            browser()
          }
        })
        
        eval(fxn_expr, envir = parent.frame())
      }
      
      
    } # DEFINE INNER FUNCTIONS
    
    X <- within(
      X,
      {
        t <- as.numeric(difftime(dateTime, as.POSIXct('2018-06-01 00:00:00', tz = 'America/Denver'), 'secs'))
        t_bin <- cut(t, seq(-1, max(t), agg_t))
      })
    
    X_cleaned <- aggregate(cbind(xloc, yloc) ~ ID + t_bin, data = X, FUN = mean)
    
    id_set <- sort(unique(X_cleaned$ID))
    
    dt_array <- array(
      numeric(1), 
      dim = c(
        length(id_set), 
        length(id_set), 
        length(levels(X_cleaned$t_bin))
      ),
      dimnames = list(
        id_set,
        id_set,
        levels(X_cleaned$t_bin)
      )
    )
    
    array_t <- system.time(calc_dist_matrix())
    
    browser()
    
    
    
    
    
    
    
  }
  
} #DEFINE FUNCTIONS

master_function(X = calves2018, agg_t = 60)




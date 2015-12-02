library(dplyr)
library(magrittr)
library(ggplot2)

dis <- function(df, center){
    result <- matrix(0, nrow(df), nrow(center))
    for (i in 1:nrow(df)) {
        result[i, ] <- apply(center, 1, function(x){
            sqrt(sum((df[i, ] - x) ^ 2))
        })
    }
    colnames(result) <- 1:nrow(center)
    return(result)
}

centerNew <- function(df_with_cat){
    cat <- unique(df_with_cat[, ncol(df_with_cat)])
    result <- matrix(NA, length(cat), ncol(df_with_cat) - 1)
    for (i in 1:nrow(result)) {
        result[i, ] <- tryCatch(
            {apply(df_with_cat[df_with_cat[, ncol(df_with_cat)] == cat[i], -ncol(df_with_cat)], 2, mean)}, 
            error = function(e){
                return(df_with_cat[df_with_cat[, ncol(df_with_cat)] == cat[i], -ncol(df_with_cat)])
            }
        ) 
    }
    return(result)
}

km <- function(df, k = 2, thr = 0.95, n = 1000){
    result <- list()
    N <- 0
    n_row <- dim(df)[1]
    n_col <- dim(df)[2]
    
    limit_max <- apply(df, 2, max)
    limit_min <- apply(df, 2, min)
    
    center <- k * n_col %>% 
        runif(limit_min, limit_max) %>% 
        matrix(nrow = k, ncol = n_col) %>% 
        dis(df, .) %>% 
        apply(1, which.min)
    
    while (TRUE) {
        category_temp <- df %>% 
            cbind(category) %>% 
            centerNew() %>% 
            dis(df, .) %>% 
            apply(1, which.min)
        N <- N + 1
        
        if (mean(category == category_temp) > thr | N > n) {
            break
        } else {
            category <- category_temp
        }
    }
    result[['cluster']] <- category
    result[['center']] <- centerNew(cbind(df, category))
    return(result)
}
















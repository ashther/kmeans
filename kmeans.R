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
    cat_col <- df_with_cat[, ncol(df_with_cat)]
    cat_unique <- unique(cat_col)
    n_col <- ncol(df_with_cat)
    
    result <- matrix(NA, length(cat_unique), n_col - 1)
    
    for (i in 1:nrow(result)) {
        result[i, ] <- tryCatch(
            {colMeans(df_with_cat[cat_col == cat_unique[i], -n_col])}, 
            error = function(e){
                return(df_with_cat[cat_col == cat_unique[i], -n_col])
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
    
    category <- k * n_col %>% 
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

df1 <- matrix(rnorm(50, 0.25, 0.1), 25, 2)
df2 <- matrix(rnorm(50, 0.75, 0.1), 25, 2)
df <- rbind(df1, df2)
result <- km(df)
qplot(df[, 1], df[, 2], color = result$cluster)














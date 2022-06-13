#'k_fold_lmm_split
#'
#'
k_fold_lmm_split <- function(data,
                             k = 5,
                             identifiers){

        unique_ids <- unique(data[[paste(identifiers)]])

        folds <- createFolds(unique(unique_ids), k = k)

        folds
}

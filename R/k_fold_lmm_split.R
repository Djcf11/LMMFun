#'k_fold_lmm_split
#' Creates k-folds for model training
#'
k_fold_lmm_split <- function(data,
                             k = 5,
                             identifiers){

        unique_ids <- unique(data[[paste(identifiers)]])

        folds <- caret::createFolds(unique(unique_ids), k = k)

        folds
}

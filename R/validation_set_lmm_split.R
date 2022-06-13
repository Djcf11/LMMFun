#'validation_set_lmm_split
#'
#'
validation_set_lmm_split <- function(data, identifiers, train_split = .7){
        # Default is 70 30 split

        # Extract unique ids
        unique_ids <- unique(data[[paste(identifiers)]])

        # Train split

        train_sample <- sample(1:length(unique_ids), size = round(.7*10))

        train_sample
}

#'lmm_k_fold_validation
#'
#'

lmm_k_fold_validation <- function(data,
                                  identifiers,
                                  training_folds,
                                  outcome,
                                  fixed_effect,
                                  random_slope = NULL,
                                  random_intercept
){
        # extract unique identifiers

        unique_ids <- unique(data[[paste(identifiers)]])

        # extract training data

        # select ids with found in the training set
        subject_vector <- data[, random_intercept] %in% unique_ids[train_set]

        #initialize error vector
        errors <- c()

        for(i in seq_along(training_folds)){

                # create subject vector from fold indices
                subject_vector <- data[, random_intercept] %in% unique_ids[training_folds[[i]]]

                # extract training set
                lmm_train_data <- data[subject_vector,]

                # extract test data
                lmm_test_data <- data[!subject_vector,]

                # create random effect formula
                if(is.null(random_slope)){
                        random_effect_form <- paste("(", 1, "|", random_intercept,")")
                } else {
                        random_effect_form <-  paste("(", random_slope, "|", random_intercept,")")
                }

                # create fixed effect formula
                if(length(fixed_effect) == 1){
                        fixed_effect_form <- paste(fixed_effect)
                } else{
                        fixed_effect_form <- paste(fixed_effect, collapse = "+")}

                # create full linear mixed model formula
                lmm_formula <- paste(outcome, "~", fixed_effect_form, "+", random_effect_form)

                if(nrow(unique(lmm_train_data[random_intercept]))>1){

                        #fit linear mixed model
                        lmm_train_obj <- lmer(formula(lmm_formula), data = lmm_train_data)

                        # use trained model and test data to output predictions

                        lmm_test_data$predicted <-  predict(lmm_train_obj,
                                                            newdata = lmm_test_data[,
                                                                                    c(fixed_effect,random_intercept)],
                                                            allow.new.levels = TRUE)

                        # calculate mean squared errors
                        current_cycle_mse <-  mse(lmm_test_data[, outcome],
                                                  lmm_test_data[, "predicted"])

                        # append mse errors
                        errors <- c(errors,
                                    current_cycle_mse)
                } else{
                        warning("Fold only has 1 subject. Can not fit Linear Mixed Model.")
                        next
                }
        }
        output <- list(lmm_formula = lmm_formula,
                       mean_test_error = mean(errors),
                       fold_errors = errors,
                       fold_count = length(training_folds))

        output
}

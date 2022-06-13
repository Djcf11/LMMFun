#'lmm_validation_set
#'
#'

lmm_validation_set <- function(data,
                               identifiers,
                               training_index,
                               outcome,
                               fixed_effect,
                               random_slope = NULL,
                               random_intercept){

        unique_ids <- unique(data[[paste(identifiers)]])
        # extract training data

        # select ids with found in the training set
        subject_vector <- data[, random_intercept] %in% unique_ids[train_set]

        # extract training set
        lmm_train_data <- data[subject_vector,]

        # extract test data
        lmm_test_data <- data[!subject_vector,]

        # create random effect formula
        if(is.null(random_slope)){
                random_effect_form <- paste("(", 1, "|",random_intercept,")")
        } else {
                random_effect_form <-  paste("(", random_slope, "|",random_intercept,")")
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
                lmm_train_obj <- lme4::lmer(formula(lmm_formula), data = lmm_train_data)

                # use trained model and test data to output predictions
                lmm_test_data$predicted <-  predict(lmm_train_obj,
                                                    newdata = lmm_test_data[, c(fixed_effect,  random_intercept)], allow.new.levels = TRUE)

                # calculate mean squared errors
                test_set_mse <-  ModelMetrics::mse(lmm_test_data[,"predicted"], lmm_test_data[,outcome])

        } else{
                stop("Only one subject in train set. Cannot fit linear mixed model on one subject.")
        }

        output <- list(lmm_object = lmm_train_obj,
                       lmm_formula = lmm_formula,
                       test_mse = test_set_mse
        )

        output
}

# FSaudit 0.2.2
* Fizes calculation sample size CVS

# FSaudit 0.2.1
* Fixes dafa tests

# FSaudit 0.2
* Fixes unstratified implementation, adds support to define cvs_obj and immediately run selection

# FSaudit 0.1.9
* Fixes unstratified implementation to match calculations in Data Science for Auditors book

# FSaudit 0.1.6
* Refactored attribute sampling
* Refactored MUS
* Refactored cvs
* Refactored cvs_stratify_population()
* Standard Error in regression estimate is calculated based on Cochran's book.
* Pass seed as a parameter.

# FSaudit 0.1.5
* Modified mus functionalities for negative values
* Added function to split book values

# FSaudit 0.1.4
* replaced the selection S3 object to select.
* replaced the sampleSize S3 object to getSampleSize.
* Added item identifier in stratified sample.
* Changed samplePopulation to sample
* Modified hyper_lower()
* Modified hyper_upper()


# FSaudit 0.1.3
* added high error rate computation

# FSaudit 0.1.2
* Modified the sampleSize.attStateObject() and added the test cases for the same

# FSaudit 0.1.1
* Removed the round() used in test-CvsSamp and used tolerance in it. Also removed the ifelse() used for calculating the df in cvs_stratified_eval().
* Changed the ifelse in cvs_stratified_eval() and added required test cases.
* Improved code coverage
* Added documentation and test functions for unstratified cvs samples selection 
* Added extend sample size function for unstratified cvs
* Modified extend sample size function for stratified cvs
* Modified stratified cvs function
* Modified the MUS_sample_sizes.Rmd
* Modified MUS_Selection_methods vignette 
* Modified MUS_evaluation_strategies vignette
* Added test functions for object types
* Added S3 classes documentation
* Added test functions for attribute sampling



# FSaudit 0.1.0
* Added the function to stratify a population according to equal recorded value boundaries

# FSaudit 0.0.2.9000
* Added mus_eval
* Added bound calculations

# FSaudit 0.0.1.

* Added a `NEWS.md` file to track changes to the package.
* Added binomial, hypergeometric lower bound functions

# FSAudit 0.0.5.9000

* Export all functions in attribute sampling.


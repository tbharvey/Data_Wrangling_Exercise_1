library("dplyr")
library("tidyr")

find_replace <- function(find_vector, replace_vector, x) {
  i <- 1
  for (pattern in find_vector) {
    x <- sub(pattern, replace_vector[i], x)
    i <- i+1
  }
  return(x)
}

# 0: Load the data in RStudio
# Save the data set as a CSV file called refine_original.csv and load it in RStudio into a data frame.

ex1_frame <- read.csv("refine_original.csv")

# 1: Clean up brand names
# Clean up the 'company' column so all of the misspellings of the brand names
# are standardized. For example, you can transform the values in the column to
# be: philips, akzo, van houten and unilever (all lowercase).

company_in <- c("^.*r$", "^.*[sS]$", "^.*n$", "^.*[0Oo]$")
company_out <- c("Unilever", "Philips", "Van Houten", "Akzo")

ex1_frame$company <- find_replace(company_in, company_out, ex1_frame$company)

# 2: Separate product code and number
# Separate the product code and product number into separate columns i.e. add
# two new columns called product_code and product_number, containing the product
# code and number respectively

ex1_frame <- separate(ex1_frame, "Product.code...number", into = c("product_code", "product_number"), sep = "-", remove = FALSE)

# 3: Add product categories
# You learn that the product codes actually represent the following product categories:
# p = Smartphone
# v = TV
# x = Laptop
# q = Tablet
# In order to make the data more readable, add a column with the product category for each record.

code_in <- c("p", "v", "x", "q")
code_out <- c("Smartphone", "TV", "Laptop", "Tablet")

ex1_frame$product_category <- find_replace(code_in, code_out, ex1_frame$product_code)

# 4: Add full address for geocoding
# You'd like to view the customer information on a map. In order to do that, the
# addresses need to be in a form that can be easily geocoded. Create a new
# column full_address that concatenates the three address fields (address, city,
# country), separated by commas.

ex1_frame$full_address <- paste(ex1_frame$address, ex1_frame$city, ex1_frame$country, sep = ", " )

# 5: Create dummy variables for company and product category
# Both the company name and product category are categorical variables i.e. they
# take only a fixed set of values. In order to use them in further analysis you
# need to create dummy variables. Create dummy binary variables for each of them
# with the prefix company_ and product_ i.e.,
# 
# Add four binary (1 or 0) columns for company: company_philips, company_akzo,
# company_van_houten and company_unilever.

# Add four binary (1 or 0) columns for product category: product_smartphone,
# product_tv, product_laptop and product_tablet.

ex1_frame$spread_company <- ex1_frame$company

ex1_frame <- spread(ex1_frame, spread_company, spread_company, fill = "0")
colnames(ex1_frame)[11:14] <- c("company_akzo", "company_philips", "company_unilever", "company_vanhouten")

ex1_frame$spread_product_category <- ex1_frame$product_category

ex1_frame <- spread(ex1_frame, spread_product_category, spread_product_category, fill = "0")
colnames(ex1_frame)[15:18] <- c("product_laptop", "product_smartphone", "product_tablet", "product_tv")

ex1_frame[,11:18] <- lapply(ex1_frame[,11:18], function(x) sub("^[A-Za-z ]*$", "1", x))

write.csv(ex1_frame, file = "refine_clean.csv")

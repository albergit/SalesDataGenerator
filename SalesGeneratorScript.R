##### EDIT WITH YOUR INFORMATION (a = Vendors, b = Territories, c = Products, d = Prices) #####

a <- c("Maria Canell", "Eric Baviera", "Joan Velasco")
b <- c("Andalusia", "Catalonia", "Madrid", "Valencian Community", "Galicia", "Castile and Leon", "Basque Country",
       "Castilla-La Mancha", "Region of Murcia", "Aragon", "Extremadura", "Balearic Islands",
       "Principality of Asturias", "Navarre", "Cantabria", "Rioja")
c <- c("Accessories", "Headphones", "Phone", "Laptop")
d <- c(4.88, 22.55, 129.99, 599.99)

##### CUSTOM CODE (DO NOT TOUCH WITHOUT UNDERESTANDING) #####

# Load Libraries and set seed

library(dplyr)
library(xlsx)

set.seed(05112016)

# Custom Functions

VendorSelector <- function(n) {
  sample(a, n, replace = TRUE)
}

Community <- function(n) {
  sample(b, n, replace = TRUE)
}

ProductSelector <- function(n) {
  x = c
  px = c(0.45, 0.23, 0.18, 0.14)
  sample(x, size = n, replace = TRUE, prob = px)
}

ProductQuantity <- function(n) {
  x = c(1, 2, 3, 4)
  px = c(0.70, 0.17, 0.1, 0.03)
  sample(x, size = n, replace = TRUE, prob = px)
}

GenerateData <- function(n){
  data <- data.frame(Date = seq(as.Date('2017-01-1'), by = "day", length.out = n),
                     Vendor = VendorSelector(n),
                     Community = Community(n),
                     Product = ProductSelector(n),
                     Quantity = ProductQuantity(n),
                     Price = NA)
  
  return (data)
}

# Generate the data and applying prices

data <- GenerateData(1095)

data <- data %>%
    mutate(Price = case_when(
      data$Product == c[1] ~ d[1],
      data$Product == c[2] ~ d[2],
      data$Product == c[3] ~ d[3],
      data$Product == c[4] ~ d[4])) %>%
  mutate(Revenue = Price*Quantity)

# Save as xlsx

write.xlsx(data, file = "salesdata.xlsx",
           sheetName = "SalesData", append = FALSE)




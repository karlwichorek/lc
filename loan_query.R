library(httr)

# This method now deprecated due to LendingClub package
rr <- GET("https://api.lendingclub.com/api/investor/v1/loans/listing", 
    add_headers(Authorization = ""))

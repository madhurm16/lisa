# Function to round in solver
ceiling_digit = function(x, level=1) round(x+ 5*10^(-level-1), level)
flooring_digit = function(x, level=1) round(x - 5*10^(-level-1), level)
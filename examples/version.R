library(BoonAmber) 

amber <- AmberClient$new("cloud", "~/.Amber.license")
print(amber$get_version())
library(magrittr)
reticulate::source_python("Scripts/fetch_registrants.py")
registered_traders <- jsonlite::fromJSON(fetch_registrants())

print(registered_traders)
View(registered_traders$Entries)

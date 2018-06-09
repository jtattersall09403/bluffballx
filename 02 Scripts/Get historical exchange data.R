# Read historical exchange data
library(data.table)
library(jsonlite)

path <- './01 Data/exchangedata sep15 mar16/C_/data/xds/historic/BASIC/'

file <- list.files(paste0(path, '27041861'))

data <- readLines(paste0(path, "27041861/", file))
df <- fromJSON(data[2], flatten = TRUE, simplifyDataFrame = TRUE)
data.2 <- jsonlite::stream_in(textConnection(gsub("\\n", "", data)), flatten = TRUE)
data.3 <- data.2$mc
data.3[[1]]$marketDefinition$runners
data.3[[1]]$marketDefinition$


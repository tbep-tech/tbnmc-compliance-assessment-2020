library(tbeptools)
library(here)
library(dplyr)

# wq data -----------------------------------------------------------------

# local file path
xlsx <- here::here('data/data-raw', 'wq_data.xls')

# import and download if new
wqdat <- read_importwq(xlsx, download_latest = T)

save(wqdat, file = here::here('data', 'wqdat.RData'), compress = 'xz')

# algae data --------------------------------------------------------------

# file path
xlsx <- here::here('data/data-raw', '/phyto_data.xlsx')

# load and assign to object
algdat <- read_importphyto(xlsx, download_latest = T) %>%
  filter(yr < 2021)

save(algdat, file = here::here('data', 'algdat.RData'), compress = 'xz')

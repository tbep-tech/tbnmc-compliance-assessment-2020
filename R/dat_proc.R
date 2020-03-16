library(tbeptools)
library(here)
library(dplyr)

# wq data -----------------------------------------------------------------

# local file path
xlsx <- here::here('data/data-raw', 'wq_data.xls')

# import and download if new
wqdat <- read_importwq(xlsx, download_latest_epchc = T, tryurl = T, connecttimeout = 20)

save(wqdat, file = here::here('data', 'wqdat.RData'), compress = 'xz')

# algae data --------------------------------------------------------------

# file path
xlsx <- here::here('data/data-raw', '/phyto_data.xlsx')

# load and assign to object
algdat <- read_importphyto(xlsx, download_latest_epchc = T, tryurl = T) %>%
  filter(yr < 2020)

save(algdat, file = here::here('data', 'algdat.RData'), compress = 'xz')

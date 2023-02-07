library(tbeptools)
library(here)
library(dplyr)

# wq data -----------------------------------------------------------------

# local file path
# xlsx <- here::here('data/data-raw', 'wq_data.xls')
xlsx <- here::here('data/data-raw', 'Results_Provisional.xlsx')

# import and download if new
wqdat <- read_importwq(xlsx, download_latest = T)
# wqdat <- read_importwq(xlsx, download_latest = F)

save(wqdat, file = here::here('data', 'wqdat.RData'), compress = 'xz')

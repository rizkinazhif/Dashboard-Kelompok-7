library(dplyr)                   # this packages is use for data preparation (edit, remove, mutate, etc)
library(stringr)                 # all functions deal with "NA"'s and zero length vectors
library(purrr)                   # requirement packages for Functional Programming Tools
library(rlang)                   # requirement packages for Rmarkdown
library(DT)                      # interface to the JavaScript library DataTables (https://datatables.net/)
library(r2d3)                    # D3 visualization
library(shinydashboard)
library(shiny)
library(readxl)
library(plotly)
library(leaflet)
library(leaflet.providers)
library(rgdal)
library(sf)
library(viridis)
library(hrbrthemes)
library(htmltools)

jmlkejahatan.res <- read_excel("jumlahkejahatan.xlsx")
jmlkejahatan <- jmlkejahatan.res[-12]

year_list <- jmlkejahatan %>%
  select(tahun) %>%
  unique()
year_list <- as.list(c(year_list$tahun, "all_year")) %>%
  set_names(2017, 2018, 2019, "Semua")

crime_list <- as.list(c(colnames(jmlkejahatan)[-c(1, 2)], "all_crime")) %>%
  set_names("Kejahatan terhadap Nyawa",
            "Kejahatan terhadap Fisik/ Badan",
            "Kejahatan terhadap Kesusilaan",
            "Kejahatan terhadap Kemerdekaan Orang",
            "Kejahatan terhadap Hak Milik/ Barang dengan Penggunaan Kekerasan",
            "Kejahatan terhadap Hak Milik/ Barang",
            "Kejahatan Terkait Narkotika",
            "Kejahatan Terkait Penipuan, Penggelapan, dan Korupsi",
            "Kejahatan terhadap Ketertiban Umum",
            "Semua")
#MAP
data2<-readOGR(dsn=".",layer="INDO_PROV_2016")
## FILTER 2018 DAN UBAH PROVINSI JADI HURUF BESAR BIAR BISA DI JOIN
jumlahKejahatan2018<-jmlkejahatan.res %>%filter(tahun==2019)
jumlahKejahatan2018$provinsi<-toupper(jumlahKejahatan2018$provinsi)
data2018<-merge(data2,jumlahKejahatan2018,by.x="PROVINSI",by.y="provinsi")

labels <- sprintf(
  "<strong>%s</strong><br/>%g Jumlah Kejahatan ",
  data2018@data$PROVINSI, data2018@data$total
) %>% lapply(htmltools::HTML)


dt <- read_excel("jumlahkejahatan.xlsx", sheet = "Sheet2")


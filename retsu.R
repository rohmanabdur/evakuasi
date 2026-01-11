fpaket = function(paket1, ...) {
  nama_paket = c(paket1, ...)
  for (k in seq_along(nama_paket)) {
    if (isFALSE(suppressWarnings(require(nama_paket[k], character.only = TRUE)))) {
      install.packages(nama_paket[k], repos = "https://cloud.r-project.org")
      library(nama_paket[k], character.only = TRUE)
    }
  }
}

fpaket(
  "shiny", "sf", "leaflet", "leaflet.extras", "osrm", "leafgl", 
  "ggrepel", "gt", "gtExtras", "bslib"
)

runApp("C:\\Users\\Laptop ASUS\\Documents\\04_software\\R_projects\\evakuasi_tsunami\\retsu_posit\\app.R")

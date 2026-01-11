fpaket <- function(paket1, ...) {
  nama_paket <- c(paket1, ...)
  for (k in seq_along(nama_paket)) {
    if (isFALSE(suppressWarnings(require(nama_paket[k],
                                         character.only = TRUE
    )))) {
      install.packages(nama_paket[k])
      library(nama_paket[k], character.only = TRUE)
    }
  }
}

fpaket("sf","osrm", "shiny", "bslib", "gt", "gtExtras", "leaflet", "leafgl",
       "leaflet.extras")
tabel_tes_baru = readRDS("Tabel_TES_Baru.rds")
tes_baru = read_sf("tes_baru.kml") |> st_zm()
koordinat_tes_baru = st_coordinates(tes_baru)
rute_terdekat = function(lintang, bujur) {
  stopifnot("Lintang dan Bujur harus berupa bilangan" = 
              is.numeric(lintang) && is.numeric(bujur))
  semua_jarak = osrmTable(
    src = data.frame(long = bujur, lat = lintang),
    dst = koordinat_tes_baru, measure = "distance"
  )
  jarak_terdekat = min(semua_jarak$distances)
  posisi_jarak_terdekat = which.min(semua_jarak$distances)
  tes_terdekat = tabel_tes_baru[posisi_jarak_terdekat, ]
  hasil = list(
    nama_tes = tabel_tes_baru[posisi_jarak_terdekat, ],
    hasil_rute = osrmRoute(
      src = c(bujur, lintang),
      dst = tes_terdekat[,2:3],
      osrm.profile = "foot"
    )
  )
}

antarmuka = page_fillable(
  tags$h2(tags$b("Rute Evakuasi Tsunami"), class = "text-center"),
  title = "Rute Evakuasi Tsunami",
  fillable_mobile = TRUE, 
  tags$style(
    HTML("
       body, html {
       .tombol_rute{
        position: absolute;
        z: 1000;
        bottom: 21px;
        left: 21px;
        display: flex;
        align-items: center;
        justify-content: center;
        border-radius: 18px;
        background-color: #323EEC;
        font-family: 'Helvetica';
        font-size: 21px;
        color: white;
       }
        .tombol_rute:hover {
        background-color: #3A3966;
        font-family: 'Helvetica';
        font-size: 21px;
        color: white;
       }
       .nama_tes{
        font-family: 'Helvetica';
        font-weight: bold
        color: #DBEC60
       }
       .keterangan_rute{
        background-color: #FAF6D4;
       }
      ")
  ),
  layout_sidebar(
    border = FALSE, fillable = FALSE, 
    sidebar = sidebar(
      position = "right", open = FALSE, width = 600,
      title = "Kapasitas Tempat Evakuasi",
      gt_output("kapasitas_tes")
    ),
    card(min_height = "310px",
         class = "border-0", full_screen = TRUE,
         leafglOutput("peta", width = "auto"),
         input_task_button("rute_tes_terdekat",
                           "Cek Rute Evakuasi",
                           icon = icon("route"),
                           class = "tombol_rute", 
                           label_busy = "mohon tunggu..."
         )
    ),
    card(
      height = "70px", fill = TRUE, full_screen = TRUE, 
      class = "keterangan_rute",
      htmlOutput("tes_terdekat")
    )
  )
)

pelayan = function(input, output, session) {
  onSessionEnded(function() {
    stopApp()
  })
  tampilkan = function(bujurku, lintangku) {
    leaflet() |>
      addTiles() |> addControlGPS(
        options = gpsOptions(activate = TRUE, maxZoom = 16, autoCenter = TRUE)
      ) |>
      addAwesomeMarkers(
        data = tabel_tes_baru, lng = ~bujur, lat = ~lintang,
        popup = ~nama,
        label = ~nama,
        icon = makeAwesomeIcon(
          icon = "home",
          library = "fa",
          markerColor = "green",
          iconColor = "black"
        ),
        labelOptions = labelOptions(
          textsize = "12px",
          className = "nama_tes",
          noHide = TRUE,
          textOnly = TRUE,
          sticky = FALSE
        )
      ) |>
      setView(
        lng = ifelse(is.null(bujurku), 113.35, bujurku),
        lat = ifelse(is.null(lintangku), -8.35, lintangku), zoom = 14
      )
  }
  titik_koma = function(angka) {
    hasil = gsub("\\.", ",", x = angka)
    return(hasil)
  }
  waktu_tempuh = function(jarak_tempuh) {
    laju_jalan_kaki = 3.7 # km/jam
    stopifnot(is.numeric(jarak_tempuh) && length(jarak_tempuh) == 1)
    waktu = jarak_tempuh / laju_jalan_kaki
    if (waktu < 1) {
      laporan = paste(round(waktu * 60), "menit.")
    } else {
      jam = floor(waktu)
      menit = ceiling(60 * (waktu - floor(waktu)))
      laporan = paste(jam, "jam", titik_koma(menit), "menit.")
    }
    return(laporan)
  }
 

 
  observe({
    req(input$peta_click)
    rute <<- reactive(rute_terdekat(
      input$peta_click$lat,
      input$peta_click$lng
    )) |> bindCache(input$peta_click)
  }) |> bindEvent(input$rute_tes_terdekat, input$peta_click)

  observe({
      if (!is.null(input$peta_click)) {
        output$tes_terdekat = renderUI({
          HTML(
             paste("<p>Tempat Evakuasi Anda:",
              "<strong>", rute()$nama_tes$nama, "</strong>",
              ". Koordinat:", paste0("(", rute()$nama_tes$lintang, ",",
              rute()$nama_tes$bujur, ")"),
              ". Perkiraan Jarak Tempuh:<strong>",
              titik_koma(round(rute()$hasil_rute$distance, digits = 2)),
              "kilometer.", "</strong>",
              "Perkiraan Waktu Tempuh (Jalan Kaki):<strong>",
              waktu_tempuh(rute()$hasil_rute$distance), "</strong></p>"
            )
          )
        }) |> bindCache(input$peta_click)
        
       leafletProxy("peta",
          data = rute()$hasil_rute
        ) |> addPolylines()
      }
    }
  ) |> bindEvent(input$rute_tes_terdekat, ignoreNULL = FALSE)
 
  data_kapasitas_tes = data.frame(
    `Nama Tempat Evakuasi` = tabel_tes_baru$nama, 
    `Kapasitas Maksimal (orang)` = tabel_tes_baru$kapasitas,
    `Sudah Terisi (orang)` = round(runif(nrow(tabel_tes_baru), 
                                         min = 50, max = 200)),
    `Masih Tersisa (orang)` = rep(0, nrow(tabel_tes_baru)),
    `Persen Tersisa` = rep(0, nrow(tabel_tes_baru)),
    check.names = FALSE
  )
  data_kapasitas_tes$`Masih Tersisa (orang)` = 
    data_kapasitas_tes$`Kapasitas Maksimal (orang)` - 
    data_kapasitas_tes$`Sudah Terisi (orang)`
  data_kapasitas_tes$`Persen Tersisa` = round(100 * 
    (data_kapasitas_tes$`Masih Tersisa (orang)`/  
       data_kapasitas_tes$`Kapasitas Maksimal (orang)`), 
    digits = 1)
  output$kapasitas_tes = render_gt({
      data_kapasitas_tes |> gt() |> 
      gt_plt_bar_pct(column =  `Persen Tersisa` , scaled = TRUE,
                     height = 21, background = "lightblue", label_cutoff = 0,
                     labels = TRUE, fill = "orange") |>
      tab_options(column_labels.background.color = "tan")
  
  })
  output$peta = renderLeaflet({
    if (!is.null(input$peta_click)) {
      leafletProxy("peta", session) |>
        addAwesomeMarkers(
          lng = input$peta_click$lng,
          lat = input$peta_click$lat,
          icon = makeAwesomeIcon("person-running",
            library = "fa", markerColor = "tomato",
            iconColor = "snow"
          ),
          popup = paste(input$peta_click$lat,
            input$peta_click$lng,
            sep = ","
          )
        )
    }
    tampilkan(input$peta_click$lng, input$peta_click$lat)
  })
}  |> bindCache(input$peta_click)

shinyApp(ui = antarmuka, server = pelayan, options = list(launch.browser = TRUE))

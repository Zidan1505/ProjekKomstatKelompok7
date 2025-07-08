# Projek Komstat Kelompok 7

# Monitor Iklim Indonesia - Dashboard Analisis Suhu

 **Monitor Iklim Indonesia** adalah aplikasi web interaktif berbasis R Shiny yang dirancang untuk memvisualisasikan, menganalisis, dan memprediksi data suhu di seluruh provinsi di Indonesia. Dashboard ini menyediakan antarmuka yang modern dan responsif untuk menjelajahi tren iklim historis dan proyeksi masa depan.

## Fitur Utama

  - **Beranda Informatif**: Menampilkan statistik utama secara *real-time* seperti suhu tertinggi, rata-rata nasional, dan anomali suhu. Dilengkapi dengan peta pratinjau dan ringkasan grafik tren nasional, perbandingan regional, serta pola musiman.
  - **Analisis Tren Mendalam**: Visualisasi detail tren suhu nasional dari waktu ke waktu, lengkap dengan model regresi linear untuk menunjukkan laju pemanasan, kekuatan tren (R²), dan total perubahan suhu selama periode data.
  - **Peta Iklim Interaktif**: Peta geospasial (Leaflet) yang memungkinkan pengguna untuk memvisualisasikan berbagai parameter iklim seperti suhu saat ini, rata-rata historis, anomali suhu, dan skor risiko iklim untuk setiap provinsi.
  - **Prediksi Time Series**: Modul peramalan menggunakan model **ARIMA (Autoregressive Integrated Moving Average)** untuk memproyeksikan tren suhu bulanan di masa depan. Pengguna dapat memilih horizon prediksi dan melihat metrik performa model (MAE, RMSE, AIC).
  - **Tabel Data Dinamis**: Menampilkan data mentah dan data olahan dalam format tabel interaktif (DT) yang dapat dicari, diurutkan, dan difilter.
  - **Desain Modern & Responsif**: Antarmuka pengguna (UI) yang dirancang khusus dengan CSS untuk pengalaman pengguna yang intuitif dan menarik di berbagai perangkat.

## Teknologi & Dependensi

Proyek ini dibangun menggunakan **R** dan framework **Shiny**. Paket R utama yang digunakan antara lain:

  - `shiny` & `shinyjs`: Framework aplikasi web dan interaktivitas JavaScript.
  - `dplyr` & `lubridate`: Manipulasi dan pengolahan data.
  - `leaflet`: Pembuatan peta interaktif.
  - `plotly`: Grafik dan visualisasi data interaktif.
  - `forecast`: Untuk pemodelan dan prediksi time series ARIMA.
  - `DT`: Menampilkan tabel data HTML yang interaktif.
  - `readxl`: Membaca data dari file Excel.

## Struktur Proyek

Proyek ini diorganisir ke dalam beberapa file untuk memisahkan logika, antarmuka, dan pemrosesan data:

```
.
├── app.R               # File utama untuk menjalankan aplikasi Shiny
├── global.R            # Skrip untuk memuat library dan memproses data awal
├── ui.R                # Mendefinisikan struktur dan tata letak antarmuka pengguna
├── server.R            # Mengandung logika server, reaktivitas, dan rendering output
├── styles.css          # File CSS kustom untuk styling tampilan dashboard
└── data/
    ├── data_suhu_lengkap.xlsx  # (Diperlukan) Dataset utama suhu provinsi
    └── indonesia-prov.geojson    # (Diperlukan) Data geospasial untuk peta
```

### Penjelasan File

  - **`global.R`**: Bertanggung jawab untuk memuat semua *library* yang dibutuhkan. Skrip ini juga memuat dataset utama (`data_suhu_lengkap.xlsx`), melakukan pembersihan dan transformasi data, serta membuat data frame agregat yang akan digunakan di seluruh aplikasi (misalnya, tren nasional, data bulanan, data per provinsi).
  - **`ui.R`**: Berisi kode yang membangun seluruh tampilan antarmuka aplikasi. Ini termasuk *navbar* (panel navigasi), *layout* untuk setiap tab (Beranda, Tren, Peta, Prediksi, Tentang), serta penempatan semua elemen input (tombol, *dropdown*) dan output (grafik, peta, tabel). File ini terhubung dengan `styles.css` untuk kustomisasi visual.
  - **`server.R`**: Inti dari aplikasi. File ini berisi semua logika reaktif. Ia "mendengarkan" input dari `ui.R` (misalnya, klik tombol atau perubahan *dropdown*), melakukan perhitungan yang diperlukan (seperti menjalankan model ARIMA atau memfilter data), dan kemudian merender output (seperti `renderPlotly`, `renderLeaflet`, `renderDataTable`) yang akan ditampilkan kembali di `ui.R`.
  - **`app.R`**: File yang sangat sederhana yang berfungsi sebagai titik masuk aplikasi. Ia memanggil `source()` untuk memuat file `global.R`, `ui.R`, dan `server.R`, lalu menggabungkannya menjadi satu aplikasi utuh menggunakan fungsi `shinyApp()`.
  - **`styles.css`**: Memberikan "sentuhan akhir" pada aplikasi. File ini berisi semua aturan CSS kustom untuk mengubah skema warna, tipografi, jarak, bayangan, dan membuat desain dashboard menjadi lebih profesional dan menarik daripada tampilan default Shiny.

## Instalasi dan Cara Menjalankan

Untuk menjalankan dashboard ini di komputer lokal Anda, ikuti langkah-langkah berikut:

1.  **Clone Repositori**

    ```bash
    git clone (https://github.com/Zidan1505/ProjekKomstatKelompok7/tree/56e19abaeda922facf2ca00ecb48373c4b9d9ce3)
    ```

2.  **Siapkan Struktur Direktori**

      - Download file yang terintegrasi `Dashboard Monitor Iklim Indonesia.R` atau file terpisah `app.R`dan lain-lain.
      - Download juga data excel & geojson.
      - Buat folder bernama `data` di dalam direktori utama proyek.
      - Letakkan file `data_suhu_lengkap.xlsx` dan `indonesia-prov.geojson` di dalam folder `data/` tersebut.

3.  **Sesuaikan file**

      - Sesuaikan direktori `setwd("C:/Tugas Zidan/Sem 4/Komstat/Projek/deploy")` di `app.R`.
      - Lakukan hal yang sama untuk data excel dan geojson_path.

4.  **Instal Paket R yang Dibutuhkan**
    Buka R atau RStudio dan jalankan skrip berikut di konsol untuk menginstal semua dependensi:

    ```r
    install.packages(c("shiny", "shinyjs", "leaflet", "plotly", "DT", "dplyr", 
                       "forecast", "lubridate", "readxl", "stringr", "sf", 
                       "geojsonsf", "jsonlite"))
    ```

5.  **Jalankan Aplikasi**

      - Pastikan direktori kerja R Anda adalah direktori utama proyek.
      - Jalankan aplikasi dengan membuka file `app.R` atau `Dashboard Monitor Iklim Indonesia.R` dan mengklik "Run App" di RStudio, atau jalankan perintah berikut di konsol R:

    <!-- end list -->

    ```r
    shiny::runApp('app.R')
    ```

Aplikasi akan terbuka di jendela baru atau di browser web Anda.

## Kelompok 7
1. Evelyn Tan Eldisha Nawa (222313067)
2. Farhan Kadhafi Azuansyah (222313079)
3. Naufal Dzaki Zaidan (222313290)

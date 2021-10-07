# Download data for `WA_cd_prelim` analysis
# © October 2021

# Download necessary files for analysis

# compile raw data into a final shapefile for analysis
download = function() {
    path = download_redistricting_file("WA", "data-raw/WA")

    plans = list(
        dem_house = "data-raw/WA/dem_house.zip",
        rep_house = "data-raw/WA/rep_house.zip",
        dem_senate = "data-raw/WA/dem_senate.zip",
        rep_senate = "data-raw/WA/rep_senate.zip"
    )
    plan_urls =  list(
        dem_house = "https://drive.google.com/file/d/18IliSHAPYSG1rXGxZE6hp-Xtm-DlncRN/view",
        rep_house = "https://drive.google.com/file/d/1UHmYbJ666igAqHcqTzwKXxotvVs8Mu5z/view",
        dem_senate = "https://drive.google.com/file/d/1m5cet9xqEo5AfPNKhThTbeh20vLxBaAZ/view",
        rep_senate = "https://drive.google.com/file/d/114JF0TtkQUwcdTc65r56g1s0N4QT2UFr/view"
    )

    download_gd = function(url, path) {
        id = googledrive::as_id(url)
        url = str_glue("https://drive.google.com/u/0/uc?id={id}&export=download")
        download.file(url, path)
    }

    purrr::walk(names(plans), function(name) {
        zippath = here(str_c(plans[[name]], ".zip"))
        download_gd(plan_urls[[name]], zippath)
        unzip(zippath, exdir=here("data-raw/WA"))
        dirnm = strsplit(unzip(zippath, list=TRUE)$Name[1], "/")[[1]][1]
        unlink(zippath)
        file.rename(here("data-raw/WA", dirnm), here("data-raw/WA", name))
    })

    # return list of paths to downloaded file
    list(data=path, plans=purrr::map(plans, ~ str_remove(., ".zip")))
}

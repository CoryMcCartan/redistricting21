# Download data for ```SLUG``` analysis
# ``COPYRIGHT``

# Download necessary files for analysis

# compile raw data into a final shapefile for analysis
download = function() {
    shp_url = ""
    shp_path = "data-raw/``STATE``/" # don't use here()
    download(shp_url, here(shp_path))

    baf_url = ""
    baf_path = "data-raw/``STATE``/"
    download(baf_url, here(baf_path))

    # return list of paths to downloaded file
    list(shp=shp_path, baf=baf_path)
}

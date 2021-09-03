# Download data for ```SLUG``` analysis
# ``COPYRIGHT``

# Download necessary files for analysis

# compile raw data into a final shapefile for analysis
download = function() {
    path = download_redistricting_file("``STATE``", "data-raw/``STATE``/")

    # return list of paths to downloaded file
    list(data=path)
}

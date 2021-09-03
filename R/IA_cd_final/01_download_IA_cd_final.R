# Download data for `IA_cd_final` analysis
# © August 2021

# Download necessary files for analysis

# compile raw data into a final shapefile for analysis
download = function() {
    path = download_redistricting_file("IA", "data-raw/IA/")

    # return list of paths to downloaded file
    list(data=path)
}

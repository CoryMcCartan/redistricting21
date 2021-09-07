# Download data for `CO_cd_staff` analysis
# © September 2021

# Download necessary files for analysis

# compile raw data into a final shapefile for analysis
download = function() {
    path = download_redistricting_file("CO", "data-raw/CO")

    baf = download.file("https://redistricting.colorado.gov/rails/active_storage/blobs/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBBaThDIiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--693d5491e718961d33c110b9d87d6b22618b87d5/First_Staff_Congressional_Final_20210902.txt",
                        "data-raw/CO/co_staff_baf.csv")

    # return list of paths to downloaded file
    list(data=path, baf=baf)
}

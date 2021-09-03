# Download data for `OR_cd_prelim` analysis
# © September 2021

# Download necessary files for analysis

# MANUALLY DOWNLOAD
# plan A and plan B from https://oregon-redistricting.esriemcs.com/portal/home/group.html?id=9d01f6f1ffc24cb7abbee52f4ce9aa56#overview

# compile raw data into a final shapefile for analysis
download = function() {
    block_path = "data-raw/OR/or_2020_block.csv"
    download.file("https://raw.githubusercontent.com/alarm-redist/census-2020/main/census-vest-2020/or_2020_block.csv",
                  block_path)

    # return list of paths to downloaded file
    list(blocks=block_path,
         plan_a="data-raw/OR/Congress - Plan A Public/Congress_-_Plan_A.txt",
         plan_b="data-raw/OR/Congress - Plan B Public/Congress_-_Plan_B.txt")
}

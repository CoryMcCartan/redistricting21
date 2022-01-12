# Download data for `NJ_cd_final` analysis
# © January 2022

# Download necessary files for analysis

# compile raw data into a final shapefile for analysis
download = function() {
    #path = download_redistricting_file("NJ", "data-raw/NJ")

    baf_path <- 'data-raw/NJ/baf.csv'
    baf <- download.file('https://www.njredistrictingcommission.org/documents/2021/Data2021/NJ_CONG_FINAL_REPORTS_block_assignment.txt',
                    baf_path)

    adj_pop_path <- 'data-raw/NJ/adj_pop.zip'
    adj_pop_url <- 'https://nj.gov/state/assets/2020-census-data/combined-2020-census-data.zip'
    adj_pop <- download.file(adj_pop_url, adj_pop_path)
    adj_pop <- unzip(adj_pop_path, exdir = 'data-raw/NJ')
    adj_pop <- fs::dir_ls('data-raw/NJ/Combined 2020 Census Data/New Jersey P.L.2019, c.385 Reallocated Redistricting Data/blocks')
    adj_pop <- lapply(adj_pop, \(x) readxl::read_xlsx(path = x, col_names =
                                                          c('state', 'county', 'municipality', 'tract',
                                                            'block_group', 'block', 'vtd', 'vtdi',
                                                            'county_name', 'muni_name', 'block_name', 'popa',
                                                            'adj_drop_one_race', 'popa_white', 'popa_black', 'popa_aian',
                                                            'popa_asian', 'popa_nhpi', 'popa_other', 'popa_two',
                                                            'popa_hisp', 'adj_drop_not_hisp',  'vapa')))

    # return list of paths to downloaded file
    list(#data=path,
         baf = baf_path,
         pop = adj_pop_path2)
}

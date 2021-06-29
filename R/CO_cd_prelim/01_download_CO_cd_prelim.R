# Download data for `CO_cd_prelim` analysis
# © June 2021

# Download necessary files for analysis

# compile raw data into a final shapefile for analysis
download_files = function() {
  # Download files
  shp_url <- 'https://redistricting.colorado.gov/rails/active_storage/blobs/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBBcU1CIiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--cc64f0d661b52f4e5fd422a5f8207e694520e006/CO_Congressional_Districts_Prelim_Final_SHP.zip'
  shp_path <- 'data-raw/CO/CO_Congressional_Districts_Prelim_Final_SHP/CO_Congressional_Districts_Prelim_Final_06_23_2021.shp'
  if (!file.exists(shp_path)) {
    td <- tempfile(fileext = '.zip')
    download(shp_url, td)
    zip::unzip(td, exdir = here('data-raw/CO'))
  }

  pop_url <- 'https://redistricting.colorado.gov/rails/active_storage/blobs/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBBcVlCIiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--cb7114124cc840a2533959c258c73d58e3b832eb/2020_Preliminary_Pop_Estimates_2010_Census_Blocks_06_23_2021_CSV.zip'
  pop_path <- 'data-raw/CO/Colorado_Redistricting_Preliminary_Population_Estimates_Final_06_23_2021.csv'
  if (!file.exists(here(pop_path))) {
    td <- tempfile(fileext = '.zip')
    download(pop_url, td)
    zip::unzip(td, exdir = here('data-raw/CO'))
  }

  vtd_20_url <- 'https://www2.census.gov/geo/tiger/TIGER2020PL/LAYER/VTD/2020/tl_2020_08_vtd20.zip'
  vtd_20_path <- 'data-raw/CO/tl_2020_08_vtd20.shp'
  if (!file.exists(here(vtd_20_path))) {
    td <- tempfile(fileext = '.zip')
    download(vtd_20_url, td)
    zip::unzip(td, exdir = here('data-raw/CO'))
  }

  co_2018_url <- 'https://doi.org/10.7910/DVN/UBKYRU/PPH2WE'
  co_2018_path <- 'data-raw/CO/co_2018.shp'
  if (!file.exists(here(co_2018_path))) {
    td <- tempfile(fileext = '.zip')
    writeBin(dataverse::get_file_by_doi(co_2018_url), con = td)
    zip::unzip(td, exdir = here('data-raw/CO'))
  }

  co_2016_url <- 'https://doi.org/10.7910/DVN/NH5S2I/XSXFA1'
  co_2016_path <- 'data-raw/CO/co_2016.shp'
  if (!file.exists(here(co_2016_path))) {
    td <- tempfile(fileext = '.zip')
    writeBin(dataverse::get_file_by_doi(co_2016_url), con = td)
    zip::unzip(td, exdir = here('data-raw/CO'))
  }

  # return list of paths to downloaded file
  list(
    shp = shp_path, pop = pop_path, vtd_20 = vtd_20_path,
    co_18 = co_2018_path, co_16 = co_2016_path
  )
}

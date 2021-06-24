# Prepare data for `CO_cd_prelim` analysis
# © June 2021

# Download necessary files for analysis
CO_cd_prelim_download <- function() {
  shp_url <- 'https://redistricting.colorado.gov/rails/active_storage/blobs/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBBcU1CIiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--cc64f0d661b52f4e5fd422a5f8207e694520e006/CO_Congressional_Districts_Prelim_Final_SHP.zip'
  shp_path <- 'data-raw/CO/CO_Congressional_Districts_Prelim_Final_SHP/CO_Congressional_Districts_Prelim_Final_06_23_2021.shp' # don't use here()
  if (!file.exists(here(shp_path))) {
    td <- tempfile(fileext = '.zip')
    download(shp_url, td)
    # here(shp_path)
    zip::unzip(td, exdir = here('data-raw/CO'))
  }

  pop_url <- 'https://redistricting.colorado.gov/rails/active_storage/blobs/eyJfcmFpbHMiOnsibWVzc2FnZSI6IkJBaHBBcVlCIiwiZXhwIjpudWxsLCJwdXIiOiJibG9iX2lkIn19--cb7114124cc840a2533959c258c73d58e3b832eb/2020_Preliminary_Pop_Estimates_2010_Census_Blocks_06_23_2021_CSV.zip'
  pop_path <- 'data-raw/CO/Colorado_Redistricting_Preliminary_Population_Estimates_Final_06_23_2021.csv'
  if (!file.exists(here(pop_path))) {
    td <- tempfile(fileext = '.zip')
    download(pop_url, td)
    zip::unzip(td, exdir = here('data-raw/CO'))
  }

  baf_url <- NULL
  baf_path <- NULL
  # download(baf_url, here(baf_path))

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


  # return a named vector of downloaded file paths
  c(
    shp = shp_path, baf = baf_path, pop = pop_path, vtd_20 = vtd_20_path,
    co_16 = co_2016_path, co_18 = co_2018_path
  )
}

# Compile raw data into a final shapefile for analysis
CO_cd_prelim_prepare <- function(paths) {
    
    # general vars ----
    state_abb <- 'CO'
    geo_year <- 2010
    path <- str_glue('data/{state_abb}/co_vtd_20.Rds')
    
    if (file.exists(here(path))) {
        return(c(shp = path))
    }
    
  co_shp <- read_sf(here(paths['shp'])) %>%
    ms_simplify(keep = 0.04, keep_shapes = TRUE)

  # preparation and processing code

  # libs ----
  library(sf)
  library(redist)
  library(tidyverse)
  library(geomander)
  library(blockpop)

  # speed ----
  sf::sf_use_s2(FALSE)



  # check out inputs ----
  prop <- st_read(paths['shp'])
  pop <- read_csv(file = paths['pop']) %>%
    slice(-201063) # removes a colsums final row

  # Get some geographies for blocks
  blk_geog <- create_block_table(state = state_abb, year = 2010)

  # connect data
  blk <- blk_geog %>% left_join(pop %>% rename(GEOID = GEOID10), by = 'GEOID')
  blk <- janitor::clean_names(blk)


  # transform projections
  blk <- blk %>% st_transform(st_crs(prop))

  # match! ----
  blk_dist_match <- geo_match(from = blk, to = prop, method = 'centroid')
  blk$cd <- blk_dist_match

  # voting districts:
  vtd <- tigris::voting_districts(state = state_abb)
  vtd <- vtd %>% st_transform(st_crs(prop))

  blk_vtd_match <- geo_match(from = blk, to = vtd, method = 'centroid')
  vtd_dist_match <- geo_match(from = vtd, to = prop, method = 'area')

  vtd20 <- st_read(paths['vtd_20']) %>% st_transform(st_crs(prop))
  blk_vtd20_match <- geo_match(from = blk, to = vtd20, method = 'centroid')
  vtd20_dist_match <- geo_match(from = vtd20, to = prop, method = 'area')


  # precincts

  # Voting and Election Science Team, 2018, "2016 Precinct-Level Election Results",
  # https://doi.org/10.7910/DVN/NH5S2I, Harvard Dataverse, V60
  prec16 <- st_read(str_glue(paths['co_16'])) %>%
    st_transform(st_crs(prop)) %>%
    rename(
      dem_16_pres = G16PREDCLI, rep_16_pres = G16PRERTRU,
      dem_16_sen = G16USSDBEN, rep_16_sen = G16USSRGLE
    )
  # Voting and Election Science Team, 2019, "2018 Precinct-Level Election Results",
  # https://doi.org/10.7910/DVN/UBKYRU, Harvard Dataverse, V39
  prec18 <- st_read(str_glue(paths['co_18'])) %>%
    st_transform(st_crs(prop)) %>%
    rename(
      dem_18_gov = G18GOVDPOL, rep_18_gov = G18GOVRSTA,
      dem_18_atg = G18ATGDWEI, rep_18_atg = G18ATGRBRA,
      dem_18_sos = G18SOSDGRI, rep_18_sos = G18SOSRWIL
    )

  prec16 <- prec16 %>%
    rowwise() %>%
    mutate(
      dem_16 = mean(c_across(cols = starts_with('dem_16'))),
      rep_16 = mean(c_across(cols = starts_with('rep_16')))
    ) %>%
    ungroup()

  prec18 <- prec18 %>%
    rowwise() %>%
    mutate(
      dem_18 = mean(c_across(cols = starts_with('dem_18'))),
      rep_18 = mean(c_across(cols = starts_with('rep_18')))
    ) %>%
    ungroup()

  blk_prec16_match <- geo_match(from = blk, to = prec16, method = 'centroid')
  blk_prec18_match <- geo_match(from = blk, to = prec18, method = 'centroid')

  blk <- blk %>% mutate(
    rep_16 = estimate_down(wts = blk$vap, value = prec16$rep_16, group = blk_prec16_match),
    dem_16 = estimate_down(wts = blk$vap, value = prec16$dem_16, group = blk_prec16_match),
    rep_18 = estimate_down(wts = blk$vap, value = prec18$rep_18, group = blk_prec18_match),
    dem_18 = estimate_down(wts = blk$vap, value = prec18$dem_18, group = blk_prec18_match)
  )


  # fcc ----
  fcc <- bl_load_state(state_abb)
  fcc_20 <- bl_est_2020(fcc)
  acs <- bl_download_acs_vars(state_abb)
  census <- bl_download_2010_vars(state_abb)
  harm <- bl_harmonize_vars(fcc_20, census, acs)
  harm <- harm %>% rename_with(.fn = ~ paste0('e_', .x), .cols = contains(c('2020', '_')))
  harm <- harm %>% select(-state, -pop2010, -vap2010)
  harm <- harm %>% rename_with(~ str_replace(.x, '2020', ''))
  harm <- harm %>% rename(geoid = block)

  # finalize ----
  blk <- blk %>% mutate(vtd20 = blk_vtd20_match)
  blk_harm <- blk %>% left_join(harm, by = 'geoid')
  blk_at_vtd <- blk_harm %>%
    st_drop_geometry() %>%
    group_by(vtd20) %>%
    summarize(across(
      .cols = starts_with(c('pop', 'vap', 'dem', 'rep', 'e_')) | contains('prelim'),
      .fns = ~ sum(.x, na.rm = TRUE)
    ))


  co_final_shp <- vtd20 %>%
    mutate(vtd20 = row_number()) %>%
    left_join(blk_at_vtd, by = 'vtd20') %>%
    mutate(cd = )

  path <- str_glue('data/{state_abb}/co_vtd_20.Rds')
  write_rds(co_final_shp, here(path), compress = 'xz')

  # return a named vector of processed file paths
  c(shp = path)
}

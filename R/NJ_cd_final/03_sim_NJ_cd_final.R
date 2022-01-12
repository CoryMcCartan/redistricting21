# Simulate plans for `NJ_cd_final`
# © January 2022

shp_path = "data/NJ/NJ_cd_final_vtd_20.rds"

# Set up the redistricting problem, including filtering, cores, and population tolerance
make_map = function(shp_path) {
    nj_shp = read_rds(here(shp_path))
    nj_map = redist_map(nj_shp, pop_tol=0.01,
                               existing_plan=cd, adj=nj_shp$adj)

    nj_map
}

# Simulate redistricting plans
# Analyze and summarize simulated plans
# Returns a simulation-free summary frame with all the necessary data for visualization
simulate = function(map) {
    plans1 = redist_smc(map, nsims=10e3, counties=county)

    plans = list(
        tol_01 = plans1
    )

    ndists = attr(map, "ndists")
    dvote = map$ndv
    rvote = map$nrv
    statewide = sum(dvote) / (sum(dvote) + sum(rvote))
    ker = function(x) pt((x-0.5)/0.035136, df=22)

    plans = purrr::imap(plans, function(p, name) {
        p = p %>%
            #pullback() %>%
            mutate(dev =  plan_parity(map),
                   comp = distr_compactness(map),
                   county_splits = county_splits(map, county),
                   dem = group_frac(map, ndv, ndv+nrv),
                   black = group_frac(map, pop_black),
                   hisp = group_frac(map, pop_hisp),
                   minority = group_frac(map, pop - pop_white))

        m_dem = ker(district_group(p, dem))

        p %>%
            mutate(represent = rep(as.numeric(dvote %*% m_dem + rvote %*% (1-m_dem)) /
                       sum(dvote + rvote), each=ndists)) %>%
            group_by(draw) %>%
            mutate(proportion = statewide - sum(ker(dem)) / ndists) %>%
            ungroup()
    })

    path = "data/NJ/NJ_cd_final_results.rds"
    write_rds(plans, here(path), compress="xz")

    plans
}


# Simulate plans for `CO_sld_prelim`
# © June 2021

# Set up the redistricting problem, including filtering, cores, and population tolerance
make_map = function(shp_path) {
    co_shp = read_rds(here(shp_path))

    co_map_shd = redist_map(co_shp, pop_tol=0.05,
                            existing_plan=shd, adj=co_shp$adj)
    co_map_ssd = redist_map(co_shp, pop_tol=0.05,
                            existing_plan=ssd, adj=co_shp$adj)

    list(shd=co_map_shd, ssd=co_map_ssd)
}

# Set up the redistricting problem, including filtering, cores, and population tolerance
# Simulate redistricting plans
# Analyze and summarize simulated plans
# Returns a simulation-free summary frame with all the necessary data for visualization
simulate = function(co_map) {
    plans_shd = redist_mergesplit_parallel(co_map$shd, nsims=40, chains=800,
                                           counties=COUNTYFP20,
                                           init_plan="sample", return_all=F,
                                           ncores=6, init_name=FALSE, silent=TRUE)
    plans_shd = add_reference(plans_shd, co_map$shd$shd, "shd")

    plans_ssd = redist_smc(co_map$ssd, nsims=3e3, counties=COUNTYFP20)

    plans = list(
        shd = plans_shd,
        ssd = plans_ssd
    )

    plans = purrr::imap(plans, function(p, nm) {
        p %>%
            mutate(dev =  plan_parity(co_map[[nm]]),
                   comp = distr_compactness(co_map[[nm]]),
                   county_splits = county_splits(co_map[[nm]], COUNTYFP20),
                   dem_16 = group_frac(co_map[[nm]], dem_16, dem_16 + rep_16),
                   dem_18 = group_frac(co_map[[nm]], dem_18, dem_18 + rep_18),
                   black = group_frac(co_map[[nm]], e_vap_black, e_vap),
                   hisp = group_frac(co_map[[nm]], e_vap_hisp, e_vap),
                   minority = group_frac(co_map[[nm]], e_vap - e_vap_white, e_vap))
    })

    path = "data/CO/CO_sld_prelim_results.rds"
    write_rds(plans, here(path), compress="xz")

    plans
}


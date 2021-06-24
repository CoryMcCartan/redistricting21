# Simulate plans for `CO_cd_prelim`
# © June 2021

# Set up the redistricting problem, including filtering, cores, and population tolerance
# Simulate redistricting plans
simulate = function(shp_path) {
    return(list(map=NULL, plans=list(tol_001=NULL)))
    co_shp = read_rds(here(shp_path))
    co_map = redist_map(co_shp, existing_plan=cd, pop_tol=0.001)

    plans = redist_smc(set_pop_tol(co_map, 0.01),
                       nsims=10e3, counties=county)

    # return a list of `redist_plans` objects
    list(
        map = map,
        plans = list(
            tol_01 = plans
        )
    )
}


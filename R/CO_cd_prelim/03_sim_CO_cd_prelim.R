# Simulate plans for `CO_cd_prelim`
# © June 2021

# Set up the redistricting problem, including filtering, cores, and population tolerance
# Simulate redistricting plans
simulate = function(shp_path) {
    co_map = read_rds(here(shp_path))

    plans = redist_smc(co_map, nsims=3e3, counties=COUNTYFP20)

    # return a list of `redist_plans` objects
    list(
        tol_001 = plans
    )
}


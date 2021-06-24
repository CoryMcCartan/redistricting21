# Simulate plans for ```SLUG```
# ``COPYRIGHT``

# Set up the redistricting problem, including filtering, cores, and population tolerance
# Simulate redistricting plans
simulate = function(path) {
    ``state``_shp = read_rds(path)
    ``state``_map = redist_map(``state``_shp, existing_plan=cd, pop_tol=0.01)

    plans = redist_smc(set_pop_tol(``state``_map, 0.01),
                       nsims=10e3, counties=county)

    # return a list of `redist_plans` objects
    list(
        map = map,
        plans = list(
            tol_01 = plans
        )
    )
}


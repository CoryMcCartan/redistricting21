# Simulate plans for `CO_cd_prelim`
# © June 2021

# Set up the redistricting problem, including filtering, cores, and population tolerance
# Simulate redistricting plans
simulate = function(shp_path) {
    co_shp = read_rds(here(shp_path))
    co_map = redist_map(co_shp, existing_plan=cd, pop_tol=0.001, total_pop=e_pop)

    plans = redist_smc(co_map, nsims=3e3, counties=COUNTYFP20)

    # return a list of `redist_plans` objects
    list(
        map = co_map,
        plans = list(
            tol_001 = plans
        )
    )
}

if (FALSE) {
plans %>%
    group_by(draw) %>%
    summarize(n_dem_16 = sum(dem_16 > 0.5),
              n_dem_18 = sum(dem_18 > 0.5)) %>%
    hist(n_dem_18)

p2 = plans %>%
    group_by(draw) %>%
    summarize(mm_16 = median(dem_16) - mean(dem_16),
              mm_18 = median(dem_18) - mean(dem_18)) %>%
    hist(mm_16, bins=30) +
    scale_x_continuous("Mean-median difference", labels=scales::percent) +
    labs(title="2018")
p1 + p2 + plot_layout(guides="collect")

opt = redist_shortburst(set_pop_tol(co_map, 0.03),
                        scorer_group_pct(co_map, dem_16, dem_16 + rep_16, 5) +
                            scorer_pop_dev(co_map),
                        counties=COUNTYFP20,
                        max_bursts=100)
}

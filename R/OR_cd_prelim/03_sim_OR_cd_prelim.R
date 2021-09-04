# Simulate plans for `OR_cd_prelim`
# © September 2021

# Set up the redistricting problem, including filtering, cores, and population tolerance
make_map = function(shp_path) {
    or_shp = read_rds(here(shp_path)) %>%
        mutate(ndv = coalesce(ndv, 0),
               nrv = coalesce(nrv, 0))
    or_map = redist_map(or_shp, pop_tol=0.01,
                        existing_plan=cd_a, adj=or_shp$adj)

    or_map
}



# Simulate redistricting plans
# Analyze and summarize simulated plans
# Returns a simulation-free summary frame with all the necessary data for visualization
simulate = function(map) {
    plans1 = redist_smc(map, nsims=10e3, counties=county)
    plans2 <- redist_shortburst(map, scorer_status_quo(map, map$cd_b),
                                init_plan = map$cd_a,
                                max_bursts = 2e4, stop_at = 1)

    plans = list(
        tol_01 = plans1,
        sb = plans2
    )

    plans = purrr::map(plans, function(p) {
        p %>%
            add_reference(map$cd_b, "cd_b") %>%
            mutate(dev =  plan_parity(map),
                   comp = distr_compactness(map),
                   county_splits = county_splits(map, county),
                   dem = group_frac(map, ndv, ndv+nrv),
                   black = group_frac(map, pop_black),
                   hisp = group_frac(map, pop_hisp),
                   minority = group_frac(map, pop - pop_white))
    })

    steps <- ((1:7)/8 *
        (1 - min(plans$sb$score, na.rm = TRUE)) +
        min(plans$sb$score, na.rm = TRUE))
    sub <- unlist(lapply(seq_len(length(steps)),
                         \(x) as.integer(plans$sb$draw[which.min(abs(plans$sb$score - steps[x]))])))
    plans$sb <- plans$sb %>% filter(draw %in% sub)

    # pl = do.call('rbind', plans)
    path = "data/OR/OR_cd_prelim_results.rds"
    write_rds(plans, here(path), compress="xz")

    pl
}


if (F) {
    rename(pl, dem_16=dem) %>%
        plot_dem_distr()
    m_dem = district_group(pl, dem)
    pr_dem = pt((m_dem[, -1:-2] - 0.5) / 0.035, 22)
    plot(map, pr_dem[, 1] - rowMeans(pr_dem[, -1:-2])) +
        scale_fill_party_c(midpoint=0, limits=c(-0.5, 0.5),
                           name="Relative likelihood of being represented by a Democrat") +
        labs(title="GOP Plan B")
    plot(map, pr_dem[, 2] - rowMeans(pr_dem[, -1:-2])) +
        scale_fill_party_c(midpoint=0, limits=c(-0.5, 0.5),
                           name="Relative likelihood of being represented by a Democrat") +
        labs(title="Dem. Plan A")
}

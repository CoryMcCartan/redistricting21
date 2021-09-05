# Simulate plans for `OR_cd_prelim`
# © September 2021

shp_path = 'data/OR/OR_cd_prelim_vtd_20.rds'

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

    steps <- ((1:7)/8 *
                  (1 - min(plans2$score, na.rm = TRUE)) +
                  min(plans2$score, na.rm = TRUE))
    sub <- unlist(lapply(seq_len(length(steps)),
                         \(x) as.integer(plans2$draw[which.min(abs(plans2$score - steps[x]))])))
    sb_sub <- plans2 %>% filter(draw %in% sub)

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
            subset_sampled() %>%
            add_reference(map$cd_b, "Plan B") %>%
            add_reference(map$cd_a, "Plan A") %>%
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

    plans$sb <- sb_sub %>%
        subset_sampled() %>%
        add_reference(map$cd_b, "Plan B") %>%
        add_reference(map$cd_a, "Plan A") %>%
        mutate(dem = group_frac(map, ndv, ndv+nrv))

    # pl = do.call('rbind', plans)
    path = "data/OR/OR_cd_prelim_results.rds"
    write_rds(plans, here(path), compress="xz")

    pl
}


if (FALSE) {
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

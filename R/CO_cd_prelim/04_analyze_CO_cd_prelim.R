# Analyze simulations for `CO_cd_prelim`
# © June 2021

# Analyze and summarize simulated plans
# Returns a simulation-free summary frame with all the necessary data for visualization
analyze = function(plans, map) {
    plans = purrr::map(plans, function(p) {
        p %>%
            mutate(dev =  plan_parity(map),
                   comp = distr_compactness(map),
                   county_splits = county_splits(map, COUNTYFP20),
                   dem_16 = group_frac(map, dem_16, dem_16 + rep_16),
                   dem_18 = group_frac(map, dem_18, dem_18 + rep_18),
                   black = group_frac(map, e_vap_black, e_vap),
                   hisp = group_frac(map, e_vap_hisp, e_vap),
                   minority = group_frac(map, e_vap - e_vap_white, e_vap))
    })

    pl = bind_rows(plans, .id="sim")
    path = "data/CO/CO_cd_prelim_results.rds"
    write_rds(pl, here(path), compress="xz")

    # return path to simulation summary files
    path
}

if (FALSE) {
pl %>%
    group_by(draw) %>%
    summarize(n_dem_16 = sum(dem_16 > 0.5),
              n_dem_18 = sum(dem_18 > 0.5)) %>%
    hist(n_dem_18)

plans %>%
    group_by(draw) %>%
    mutate(egap_16 = mean(if_else(dem_16 > 0.5, 1.5 - 2*dem_16, 0.5 - 2*dem_16)),
           egap_18 = mean(if_else(dem_18 > 0.5, 1.5 - 2*dem_18, 0.5 - 2*dem_18))) %>%
    hist(egap_16)

statewide_16 = with(co_map, sum(dem_16)/sum(dem_16 + rep_16))
statewide_18 = with(co_map, sum(dem_18)/sum(dem_18 + rep_18))

plans %>%
    group_by(draw) %>%
    summarize(effgap = mean(dem_16 - 0.5)) %>%
    hist(effgap)

plans %>%
    group_by(draw) %>%
    summarize(mm_16 = median(dem_16) - mean(dem_16),
              mm_18 = median(dem_18) - mean(dem_18)) %>%
    hist(mm_18, bins=30) +
    scale_x_continuous("Mean-median difference", labels=scales::percent) +
    labs(title="2018")

opt = redist_shortburst(set_pop_tol(co_map, 0.03),
                        scorer_group_pct(co_map, dem_16, dem_16 + rep_16, 5) +
                            scorer_pop_dev(co_map),
                        counties=COUNTYFP20,
                        max_bursts=100)
}

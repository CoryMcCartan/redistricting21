# Analyze simulations for `CO_cd_prelim`
# © June 2021

# Analyze and summarize simulated plans
# Returns a simulation-free summary frame with all the necessary data for visualization
analyze = function(out) {
    return(NULL)
    out$plans = map(out$plans, function(plans) {
        plans %>%
            mutate(dev =  plan_parity(out$map),
                   comp = distr_compactness(out$map),
                   county_splits = county_splits(out$map, COUNTYFP20),
                   dem_16 = group_frac(out$map, dem_16, dem_16 + rep_16),
                   dem_18 = group_frac(out$map, dem_18, dem_18 + rep_18),
                   black = group_frac(out$map, e_vap_black, e_vap),
                   hisp = group_frac(out$map, e_vap_hisp, e_vap),
                   minority = group_frac(out$map, e_vap - e_vap_white, e_vap)) #%>%
            #clean_plans()
    })

    plans = bind_rows(out$plans, .id="sim")
    path = "data/CO/CO_cd_prelim_results.rds"
    write_rds(plans, here(path), compress="xz")

    # return path to simulation summary files
    path
}

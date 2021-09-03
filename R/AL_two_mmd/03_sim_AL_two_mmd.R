# Simulate plans for `AL_vtd_20`
# © August 2021
shp_path = "data/AL/AL_vtd_20.rds"
# Set up the redistricting problem, including filtering, cores, and population tolerance
make_map = function(shp_path) {
    al_shp = read_rds(here(shp_path))
    al_map = redist_map(al_shp, pop_tol=0.01, total_pop=pop,
                        adj=al_shp$adj, ndists = 7)
    al_map
}

scorer_inc_seats <- function(map, dvote, rvote) {
    dvote <- rlang::eval_tidy(rlang::enquo(dvote), map)
    rvote <- rlang::eval_tidy(rlang::enquo(rvote), map)
    nd <- attr(map, "ndists")

    fn <- function(plans) {
        rcounts <- redist:::agg_p2d(vote = rvote, dm = plans, nd = nd)
        dcounts <- redist:::agg_p2d(vote = dvote, dm = plans, nd = nd)
        dseat_vec <- redist:::dseats(dm = plans, rcounts = rcounts, dcounts = dcounts, nd = nd)
        dvs <- redist:::DVS(dcounts = dcounts, rcounts = rcounts)


        dseat_vec + next_seat(dvs)

    }

    class(fn) <- c("redist_scorer", "function")
    fn
}

next_seat <- function(dvs) {
    apply(dvs, 2, function(x){max(x[x < 0.5], min(x))})
}


# Simulate redistricting plans
simulate = function(al_map) {
    plans = redist_shortburst(al_map, score_fn = scorer_inc_seats(al_map, dvote = vap_black, vap - vap_black),
                              counties = county, max_bursts = 1e5, stop_at = 2)

    pl = plans %>%
        mutate(sim = "tol_001", .before="draw") %>%
        mutate(dev =  plan_parity(al_map),
               comp = distr_compactness(al_map),
               county_splits = county_splits(al_map, county),
               dvs_16 = group_frac(al_map, adv_16, adv_16 + arv_16),
               dvs_18 = group_frac(al_map, adv_18, adv_18 + arv_18),
               black = group_frac(al_map, vap_black, vap),
               hisp = group_frac(al_map, vap_hisp, vap),
               minority = group_frac(al_map, vap - vap_white, vap))

    path = "data/AL/al_two_mmd.rds"
    write_rds(pl, here(path), compress="xz")

    pl
}

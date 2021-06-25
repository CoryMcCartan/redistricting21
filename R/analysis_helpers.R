

eff_gap_calc = function(pl, shifts=seq(-0.1, 0.1, by=0.01)) {
    d_egap = pl %>%
        select(sim, draw, district, starts_with("dem_")) %>%
        pivot_longer(starts_with("dem_"), names_to="year", names_prefix="dem_",
                     values_to="dem") %>%
        mutate(year = 2000L + as.integer(year)) %>%
        group_by(sim, year, draw)

    calc_egap = function(s) {
        summarize(d_egap, shift = s,
                  egap = mean(if_else(dem + s > 0.5,
                                      1.5 - 2*(dem + s), 0.5 - 2*(dem + s))))
    }

    map_dfr(shifts, calc_egap)
}

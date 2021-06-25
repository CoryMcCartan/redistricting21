theme_r21 = theme_bw(base_size=10)

PAL_COAST = c("#7BAEA0", "#386276", "#3A4332", "#7A7D6F", "#D9B96E", "#BED4F0")
PAL_LARCH = c("#D2A554", "#626B5D", "#8C8F9E", "#858753", "#A4BADF", "#D3BEAF")
PAL = PAL_COAST[c(5, 1, 2, 4, 3, 6)]
GOP_DEM = c("#A0442C", "#B25D4C", "#C27568", "#D18E84", "#DFA8A0", "#EBC2BC",
            "#F6DCD9", "#F9F9F9", "#DAE2F4", "#BDCCEA", "#9FB6DE", "#82A0D2",
            "#638BC6", "#3D77BB", "#0063B1")


dem_distr_plot = function(pl, ...) {
    dem_cols = names(pl)
    dem_cols = dem_cols[str_starts(dem_cols, "dem_")]
    p = purrr::map(dem_cols, function(col) {
        redist.plot.distr_qtys(pl, !!rlang::sym(col), size=0.001, alpha=0.2) +
            scale_y_continuous("Democratic two-party share",
                               labels=scales::percent) +
            geom_hline(yintercept=0.5, alpha=0.25) +
            labs(title=str_c("20", str_sub(col, 5)),
                 x="Districts, ordered by Democratic share") +
            scale_color_manual(values=PAL[1]) +
            guides(color=F)
    })
    wrap_plots(p, nrow=2) + plot_layout(guides="collect")
}

plot_cds = function(map, county, abbr, city=TRUE) {
    plan = as.factor(redist:::color_graph(get_adj(map), as.integer(get_existing(map))))
    places = suppressMessages(tigris::places(abbr, cb=TRUE))
    if (city) {
        cities = arrange(places, desc(ALAND)) %>%
            filter(LSAD == "25") %>%
            head(4) %>%
            st_centroid() %>%
            suppressWarnings()
    }

    counties = map %>%
        as_tibble() %>%
        st_as_sf() %>%
        group_by({{ county }}) %>%
        summarize(is_coverage=TRUE)
    map %>%
        mutate(.plan = plan,
               .distr = get_existing(.)) %>%
        as_tibble() %>%
        st_as_sf() %>%
        group_by(.distr) %>%
        summarize(.plan = .plan[1], is_coverage=TRUE) %>%
    ggplot(aes(fill=.plan)) +
        geom_sf(size=0.0) +
        geom_sf(data=places, inherit.aes=FALSE, fill="#0000002A", color=NA) +
        geom_sf(data=counties, inherit.aes=FALSE, fill=NA, size=0.25, color="#ffffff1D") +
        geom_sf(fill=NA, size=0.4, color="black") +
        {if (city) geom_text_repel(aes(label=str_to_upper(NAME), geometry=geometry),
                        data=cities, color="#ffffff88", fontface="bold",
                        size=3.5, inherit.aes=FALSE, stat="sf_coordinates")} +
        scale_fill_manual(values=PAL) +
        theme_void() +
        guides(fill=F)
}

plot_partisan = function(map, dem, rep) {
    distrs = map %>%
        mutate(.distr = get_existing(.)) %>%
        as_tibble() %>%
        st_as_sf() %>%
        group_by(.distr) %>%
        summarize(is_coverage=TRUE)
    plot(co_map, {{dem}} / ({{dem}} + {{rep}})) +
        geom_sf(data=distrs, inherit.aes=FALSE, fill=NA, size=0.5, color="#00000044") +
        scale_fill_gradientn("Democratic share", colors=GOP_DEM, labels=scales::percent) +
        theme(legend.key.height=unit(0.4, "cm"),
              legend.key.width=unit(2, "cm"))
}

plot_minority = function(map, white) {
    distrs = map %>%
        mutate(.distr = get_existing(.)) %>%
        as_tibble() %>%
        st_as_sf() %>%
        group_by(.distr) %>%
        summarize(is_coverage=TRUE)
    plot(co_map, {{white}}) +
        geom_sf(data=distrs, inherit.aes=FALSE, fill=NA, size=0.5, color="#00000055") +
        scale_fill_wa_c("sound_sunset", name="Pct. white", labels=scales::percent) +
        theme(legend.key.height=unit(0.4, "cm"),
              legend.key.width=unit(2, "cm"))
}

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

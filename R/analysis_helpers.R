theme_r21 = function() theme_bw(base_size=10)

PAL_COAST = c("#7BAEA0", "#386276", "#3A4332", "#7A7D6F", "#D9B96E", "#BED4F0")
PAL_LARCH = c("#D2A554", "#626B5D", "#8C8F9E", "#858753", "#A4BADF", "#D3BEAF")
PAL = PAL_COAST[c(5, 1, 2, 4, 3, 6)]
GOP_DEM = c("#A0442C", "#B25D4C", "#C27568", "#D18E84", "#DFA8A0",
            "#EBC2BC",  "#F6DCD9", "#F9F9F9", "#DAE2F4", "#BDCCEA",
            "#9FB6DE", "#82A0D2", "#638BC6", "#3D77BB", "#0063B1")
scale_fill_party_c = function(name="Democratic share", midpoint=0.5, limits=0:1,
                              labels=scales::percent, oob=scales::squish, ...) {
    scale_fill_gradient2(name=name, ..., low = GOP_DEM[1], high = GOP_DEM[15],
                         midpoint=midpoint, limits=limits, labels=labels, oob=oob)
}
scale_color_party_d = function(...) {
    scale_color_manual(..., values=c(GOP_DEM[2], GOP_DEM[14]),
                       labels=c("Rep.", "Dem."))
}

grades = list(
    prop = wacolors::wa_pal("mountain", 5, type="continuous"),
    repr = wacolors::wa_pal("forest_fire", 5, type="continuous")
)
for (i in names(grades)) names(grades[[i]]) = c("F", "D", "C", "B", "A")
make_grade = function(pl) {
    refs = which(nchar(colnames(as.matrix(pl))) > 0)
    ref_names = unique(colnames(as.matrix(pl))[refs])
    ndists = max(pl$district)
    ref_seq = seq(by=ndists, length.out=length(ref_names))

    grade_qty = function(qty) {
        names(grades$prop)[ntile(qty, 5)[ref_seq]]
    }

    tibble(plan=ref_names,
           proportion=grade_qty(pl$proportion),
           prop_val=pl$proportion[ref_seq],
           represent=grade_qty(pl$represent),
           repr_val=pl$represent[ref_seq])
}
output_grades = function(gr) {
    textcol = c(`F`="white", D="white", C="black", B="black", A="black")
    fmter = function(x) stringr::str_replace(scales::number(x, 0.01), "-", "–")
    purrr::pmap_chr(gr, function(plan, proportion, prop_val, represent, repr_val) {
        str_glue('
<h4>{plan}</h4>
<div class="gradebox">
<div class="grade" style="background: {grades$prop[proportion]}; color: {textcol[proportion]}">
<div class="desc">Proportionality</div>
<div class="letter">{proportion}</div>
<div class="value">{fmter(prop_val)}</div>
</div>
<div class="grade" style="background: {grades$repr[represent]}; color: {textcol[represent]}">
<div class="desc">Representativeness</div>
<div class="letter">{represent}</div>
<div class="value">{fmter(repr_val)}</div>
</div>
</div>')
    }) %>%
        c('<a href="/redistricting/site/docs/methods.html#our-scoring-system">
          <h3 style="white-space: nowrap;">R.A. Plan Scores</h3></a>', .) %>%
        cat(sep="\n")
}


plot_dem_distr = function(pl, ...) {
    dem_cols = names(pl)
    dem_cols = dem_cols[str_starts(dem_cols, "dem_")]
    n_ref = sum(str_length(colnames(as.matrix(pl))) > 0)
    p = purrr::map(dem_cols, function(col) {
        redist.plot.distr_qtys(pl, !!rlang::sym(col), size=0.001, alpha=0.2, color_thresh=0.5) +
            scale_y_continuous("Democratic two-party share",
                               labels=scales::percent) +
            geom_hline(yintercept=0.5, alpha=0.25) +
            labs(title=str_c("20", str_sub(col, 5)),
                 x="Districts, ordered by Democratic share") +
            #scale_color_manual(values=PAL[1]) +
            scale_color_manual(values=GOP_DEM[c(1, 15)], guide="none") +
            theme_r21() +
            guides(lty=if (n_ref > 1) "legend" else "none")
    })
    wrap_plots(p, ...) + plot_layout(guides="collect")
}

plot_cds = function(map, pl, county, abbr, city=FALSE) {
    plan = as.factor(redist:::color_graph(get_adj(map), as.integer(pl)))
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
               .distr = pl) %>%
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
        scale_fill_manual(values=PAL, guide="none") +
        theme_void()
}

plot_partisan = function(map, dem, rep, plan=get_existing(.)) {
    distrs = map %>%
        mutate(.distr = plan) %>%
        as_tibble() %>%
        st_as_sf() %>%
        group_by(.distr) %>%
        summarize(is_coverage=TRUE)
    plot(map, {{dem}} / ({{dem}} + {{rep}})) +
        geom_sf(data=distrs, inherit.aes=FALSE, fill=NA, size=0.5, color="#00000055") +
        scale_fill_gradientn("Democratic share", colors=GOP_DEM, labels=scales::percent) +
        theme(legend.key.height=unit(0.4, "cm"),
              legend.key.width=unit(1.25, "cm"))
}

plot_minority = function(map, white) {
    distrs = map %>%
        mutate(.distr = get_existing(.)) %>%
        as_tibble() %>%
        st_as_sf() %>%
        group_by(.distr) %>%
        summarize(is_coverage=TRUE)
    plot(map, {{white}}) +
        geom_sf(data=distrs, inherit.aes=FALSE, fill=NA, size=0.5, color="#00000055") +
        scale_fill_wa_c("sound_sunset", name="Pct. white", labels=scales::percent) +
        theme(legend.key.height=unit(0.4, "cm"),
              legend.key.width=unit(1.25, "cm"))
}

#' Democratic share of district
#'
#' Returns a matrix of precincts by plans, where each entry is the Democratic
#' share in the district the precinct belongs to in that plan.
#'
#' @param plans a `redist_plans` object.
#' @param group column of `plans` containing the group share of each district.
#'
#' @returns a matrix
district_group = function(plans, group) {
    m = as.matrix(plans)
    m_grp = arrange(plans, as.integer(draw), district) %>%
        pull({{ group }}) %>%
        matrix(nrow=max(plans$district))
    m_prec = matrix(nrow=nrow(m), ncol=ncol(m))
    for (i in seq_len(ncol(m))) {
        m_prec[, i] = m_grp[, i][m[, i]]
    }
    m_prec
}

eff_gap_calc = function(pl, shifts=seq(-0.1, 0.1, by=0.01)) {
    if (!"sim" %in% names(pl)) pl$sim = NA_character_
    if (!"chain" %in% names(pl)) pl$chain = NA_integer_
    d_egap = pl %>%
        select(sim, chain, draw, district, starts_with("dem_")) %>%
        pivot_longer(starts_with("dem_"), names_to="year", names_prefix="dem_",
                     values_to="dem") %>%
        mutate(year = 2000L + as.integer(year)) %>%
        group_by(sim, chain, year, draw)

    calc_egap = function(s) {
        summarize(d_egap, shift = s,
                  egap = mean(if_else(dem + s > 0.5,
                                      1.5 - 2*(dem + s), 0.5 - 2*(dem + s))))
    }

    map_dfr(shifts, calc_egap)
}

plot_sv = function(map, pl) {
    refs = unique(subset_ref(pl)$draw)

    statewide = map %>%
        as_tibble() %>%
        summarize(across(starts_with("dem_"), sum),
                  across(starts_with("rep_"), sum)) %>%
        pivot_longer(c(starts_with("dem_"), starts_with("rep_")),
                       names_to=c("party", "year"), names_sep="_",
                     values_to="votes") %>%
        pivot_wider(names_from=party, values_from=votes) %>%
        mutate(year = 2000L + as.integer(year),
               statewide = dem/(dem+rep)) %>%
        select(-rep, -dem)

    if (!"sim" %in% names(pl)) pl$sim = NA_character_
    if (!"chain" %in% names(pl)) pl$chain = NA_integer_
    d_sv = pl %>%
        select(sim, chain, draw, district, starts_with("dem_")) %>%
        pivot_longer(starts_with("dem_"), names_to="year", names_prefix="dem_",
                     values_to="dem") %>%
        mutate(year = 2000L + as.integer(year)) %>%
        left_join(statewide, by="year") %>%
        group_by(sim, chain, year, draw) %>%
        arrange(desc(dem), .by_group=TRUE) %>%
        mutate(shift = 0.5 - dem,
               pct_seats = row_number()/n(),
               pct_votes = statewide + shift)

    d_sv %>%
        filter(!(draw %in% refs)) %>%
    ggplot(aes(pct_votes, pct_seats, group=draw)) +
        facet_wrap(~ year) +
        geom_line(alpha=0.05, size=0.3, color="#888888") +
        geom_hline(yintercept=0.5, lty="dashed") +
        geom_vline(xintercept=0.5, lty="dashed") +
        geom_line(data=filter(d_sv, draw %in% refs),
                  color="black", size=1.2, alpha=1) +
        geom_line(data=filter(d_sv, draw %in% refs),
                  color=PAL[3], size=0.8, alpha=1) +
        coord_equal(xlim=c(0.3, 0.7), ylim=c(0.3, 0.7)) +
        scale_x_continuous("Democratic share of votes", labels=scales::percent) +
        scale_y_continuous("Democratic share of seats", labels=scales::percent) +
        theme_r21()
}

plot_mm = function(pl) {
    refs = unique(subset_ref(pl)$draw)

    if (!"sim" %in% names(pl)) pl$sim = NA_character_
    if (!"chain" %in% names(pl)) pl$chain = NA_integer_
    d_mm = pl %>%
        select(sim, chain, draw, district, starts_with("dem_")) %>%
        pivot_longer(starts_with("dem_"), names_to="year", names_prefix="dem_",
                     values_to="dem") %>%
        mutate(year = 2000L + as.integer(year)) %>%
        group_by(sim, chain, year, draw) %>%
        summarize(meanmed = mean(dem) - median(dem))

    xmin = floor(min(d_mm$meanmed) * 400) / 400
    xmax = ceiling(max(d_mm$meanmed) * 400) / 400

    d_mm %>%
        filter(!(draw %in% refs)) %>%
    ggplot(aes(meanmed, fill=meanmed<0.0)) +
        facet_wrap(~ year) +
        geom_histogram(aes(y = after_stat(count / sum(count))),
                       breaks=seq(xmin, xmax, 0.0025)) +
        geom_vline(aes(xintercept=meanmed), data=filter(d_mm, draw %in% refs),
                   color="black", size=1.2) +
        scale_x_continuous("Mean-median difference", labels=scales::percent) +
        scale_y_continuous("Fraction of plans", labels=scales::percent,
                           expand=expansion(mult=c(0, 0.05))) +
        scale_fill_manual(values=c("TRUE"=GOP_DEM[14], "FALSE"=GOP_DEM[2])) +
        guides(fill=F) +
        theme_r21()
}

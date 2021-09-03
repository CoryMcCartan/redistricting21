# Simulate plans for `IA_cd_final`
# © August 2021

# Set up the redistricting problem, including filtering, cores, and population tolerance
make_map = function(shp_path) {
    ia_shp = read_rds(here(shp_path))
    ia_map = redist_map(ia_shp, pop_tol=0.0001,
                        ndists=4, adj=ia_shp$adj)

    ia_map
}

# Simulate redistricting plans
# Analyze and summarize simulated plans
# Returns a simulation-free summary frame with all the necessary data for visualization
simulate = function(map) {
    plans1 = redist_smc(map, nsims=10e3)

    plans = list(
        tol_0001 = plans1
    )

    plan_2010 = c(3L, 3L, 1L, 2L, 4L, 1L, 1L, 4L, 1L, 1L, 4L, 4L, 4L, 4L, 3L, 2L,
                  4L, 4L, 4L, 2L, 4L, 1L, 2L, 4L, 3L, 2L, 2L, 1L, 2L, 4L, 1L, 4L,
                  1L, 4L, 4L, 3L, 4L, 4L, 3L, 4L, 4L, 4L, 4L, 2L, 1L, 4L, 4L, 1L,
                  1L, 2L, 2L, 2L, 1L, 2L, 4L, 2L, 1L, 2L, 2L, 4L, 3L, 2L, 2L, 1L,
                  3L, 1L, 4L, 2L, 3L, 2L, 4L, 4L, 3L, 4L, 4L, 4L, 3L, 3L, 1L, 3L,
                  4L, 2L, 4L, 4L, 4L, 1L, 3L, 3L, 2L, 2L, 3L, 2L, 2L, 4L, 4L, 1L,
                  4L, 1L, 4L)

    ppp = redist.prep.polsbypopper(map)
    plans = purrr::map(plans, function(p) {
        p %>%
            add_reference(plan_2010, name="cd_2010") %>%
            mutate(dev =  plan_parity(map),
                   comp = distr_compactness(map),
                   lw = distr_compactness(map, "LengthWidth"),
                   polsby = distr_compactness(map, "PolsbyPopper", perim_df=ppp),
                   dem_16 = group_frac(map, adv_16, adv_16 + arv_16),
                   dem_18 = group_frac(map, adv_18, adv_18 + arv_18),
                   dem_20 = group_frac(map, adv_20, adv_20 + arv_20),
                   dem_avg = group_frac(map, ndv, ndv + nrv),
                   black = group_frac(map, pop_black),
                   hisp = group_frac(map, pop_hisp),
                   minority = group_frac(map, pop - pop_white))
    })

    pl = bind_rows(plans, .id="sim")
    path = "data/IA/IA_cd_final_results.rds"
    write_rds(pl, here(path), compress="xz")

    pl
}

if (F) {
analysis = run_analysis("IA", "cd")
map = analysis$map
pl = analysis$results

pl_sum = pl %>%
    group_by(draw) %>%
    summarize(dev=dev[1], comp=comp[1],
              minpol=min(polsby), avgpol=mean(polsby),
              totlw=sum(lw),
              across(starts_with("dem_"), ~ sum(. > 0.5)))

# REPRESENTATION
m_pl = as.matrix(pl)
m_dem = pl %>%
    arrange(as.integer(draw), district) %>%
    pull(dem_avg) %>%
    matrix(nrow=attr(map, "ndists"))

m_prec = matrix(nrow=nrow(m_pl), ncol=ncol(m_pl))
for (i in seq_len(ncol(m_pl))) {
    m_prec[, i] = m_dem[, i][m_pl[, i]]
}
diff = rowMeans(-m_prec[, -1] + m_prec[, 1])

diffs = matrix(nrow=nrow(m_pl), ncol=ncol(m_pl))
for (i in seq_len(ncol(m_pl))) {
    diffs[, i] = rowMeans(-m_prec[, -i] + m_prec[, i])
}
diffs_distr = matrix(nrow=max(m_pl[,1]), ncol=ncol(diffs))
for (i in seq_len(ncol(diffs))) {
    diffs_distr[, i] = tapply(abs(diffs[, i])*map$pop, m_pl[, i], sum)
}
diffs_distr = as.numeric(diffs_distr) / pl$total_pop
pl$diffs = diffs_distr

voters = sum(map$ndv + map$nrv)
kernel = function(x) pt((x - 0.5) / 0.035, df=22)
pl_sum$dem_repr = as.numeric(map$ndv %*% kernel(m_prec)) / sum(map$ndv)
pl_sum$rep_repr = as.numeric(map$nrv %*% (1 - kernel(m_prec))) / sum(map$nrv)
pl_sum$repr = with(map, (pl_sum$dem_repr*sum(ndv) + pl_sum$rep_repr*sum(nrv)) / voters)

# Chris measure
pl_sum$rep_harm = as.numeric((map$nrv * rowMeans(1 - kernel(m_prec))) %*% kernel(m_prec)) / sum(map$nrv)
pl_sum$dem_harm = as.numeric((map$ndv * rowMeans(kernel(m_prec))) %*% (1 - kernel(m_prec))) / sum(map$ndv)
pl_sum$harm = with(map, (pl_sum$dem_harm*sum(ndv) + pl_sum$rep_harm*sum(nrv)) / voters)


# DEM SEATS
hist(pl_sum, dem_16, breaks=seq(-0.25, 4.25, 0.5)) +
hist(pl_sum, dem_18, breaks=seq(-0.25, 4.25, 0.5)) +
hist(pl_sum, dem_20, breaks=seq(-0.25, 4.25, 0.5))  +
    plot_layout(guides="collect")
pl_sum %>%
    filter(draw != "cd_2010") %>%
    hist(dem_avg, breaks=seq(-0.25, 4.25, 0.5)) +
    labs(x="Estimated Democratic seats (average last 3 elections)") +
    theme_r21()


# PARETO FRONTIER
pl_sum %>%
    clean_plans() %>%
    filter(draw != "cd_2010") %>%
    distinct(dev, minpol, .keep_all=TRUE) %>%
ggplot(aes(dev, minpol, label=draw, color=dem_avg)) +
    geom_text(size=3)

#pareto = c(7175, 452, 8900, 1057, 19, 25) # min polsby
pareto = c(7175, 7282, 9741, 9884) # total L/W
pareto = c(558, 5608, 102, 2138) # Rep gerry but good
plot_pareto = function(idx) {
    dr = filter(pl_sum, draw == as.character(idx))
    dem = filter(pl, draw == as.character(idx))$dem_avg %>%
        sort() %>%
        scales::percent(accuracy=1) %>%
        paste0(collapse=", ")
    plot_cds(map, as.matrix(pl)[, idx + 1], county, "IA") +
        labs(title=str_glue("Pop. deviation: {scales::percent(dr$dev, accuracy=0.001)}\n",
                            "Compactness (P-P): {round(dr$minpol, 2)}\n",
                            "Democratic shares: {dem}"))
}
purrr::map(pareto, plot_pareto) %>%
    patchwork::wrap_plots()


# SIGNATURE PLOT
plot(pl, dem_20, size=0.1, color_thresh=0.5) +
    scale_color_manual(values=c(.GOP, .DEM)) +
    geom_hline(yintercept=0.5, lty="dashed", color="#444444") +
    labs(y="Democratic vote share, 2020 statewide elections") +
    theme_r21()

# COMPACTNESS TRADEOFFS
pl_sum %>%
    filter(draw != "cd_2010") %>%
    clean_plans() %>%
ggplot(aes(as.factor(dem_avg), comp)) +
    geom_boxplot() +
    labs(x="Estimated Democratic seats (average last 3 elections)",
         y="Compactness (higher is more compact)",
         title="Iowa: Partisan Implications of Compactness ") +
    theme_r21()

plot(map, adv_20/(adv_20+arv_20)) + scale_fill_gradient2(low=.GOP, high=.DEM, midpoint=0.5)
plot(map, adv_18/(adv_18+arv_18)) + scale_fill_gradient2(low=.GOP, high=.DEM, midpoint=0.5)
plot(map, sqrt(pop))
compare_plans(pl_sum, dem_20 == 2, dem_20 == 1, shp=map)

exs = list()
for (i in 1:12) {
     exs[[i]] = plot_cds(map, as.matrix(pl)[,i+1], county, "IA")
}
patchwork::wrap_plots(exs)


# REDIST CHALLENGE
scorer_pop_diff = function(map) {
    ndists <- attr(map, "ndists")
    total_pop = map[[attr(map, "pop_col")]]
    stopifnot(!is.null(total_pop))
    fn = function(plans) {
        pops = redist:::pop_tally(plans, total_pop, ndists)
        apply(pops, 2, function(x) max(x) - min(x))
    }
    class(fn) <- c("redist_scorer", "function")
    fn
}
scorer = scorer_pop_diff(map)

opt = redist_smc(set_pop_tol(map, 0.000005), 20, seq_alpha=1e-9, resample=F)
devs = scorer(as.matrix(opt))
best = as.matrix(opt)[, which.min(devs)]
plot_cds(map, best, county, "IA")

opt %>%
    filter(draw == 1) %>%
    mutate(dev =  plan_parity(map),
           comp = distr_compactness(map),
           lw = distr_compactness(map, "LengthWidth"),
           dem_16 = group_frac(map, adv_16, adv_16 + arv_16),
           dem_18 = group_frac(map, adv_18, adv_18 + arv_18),
           dem_20 = group_frac(map, adv_20, adv_20 + arv_20),
           dem_avg = group_frac(map, ndv, ndv + nrv),
           black = group_frac(map, pop_black),
           hisp = group_frac(map, pop_hisp),
           minority = group_frac(map, pop - pop_white))

scorer_perim = function(map, perim_df=NULL) {
    ndists <- attr(map, "ndists")
    areas = rep(1, nrow(map))
    if (is.null(perim_df))
        perim_df = redist.prep.polsbypopper(map)
    fn = function(plans) {
        pp <- redist:::polsbypopper(from = perim_df$origin, to = perim_df$touching,
                                    area = areas, perimeter = perim_df$edge, dm = plans,
                                    nd = ndists)
        colSums(sqrt(4 * pi / pp)) / 5280
    }
    class(fn) <- c("redist_scorer", "function")
    fn
}
sc_comp = scorer_perim(map, perim_df)

opt_comp = redist_shortburst(set_pop_tol(map, 0.0005), sc_comp, maximize=F,
                             max_bursts=100, return_all=F)
opt_comp2 = redist_shortburst(set_pop_tol(map, 0.0005), sc_comp, maximize=F,
                              max_bursts=100, return_all=F)
opt_comp %>%
    mutate(dev =  plan_parity(map),
           comp = distr_compactness(map),
           polsby = distr_compactness(map, "PolsbyPopper", perim_df=perim_df),
           lw = distr_compactness(map, "LengthWidth"),
           dem_16 = group_frac(map, adv_16, adv_16 + arv_16),
           dem_18 = group_frac(map, adv_18, adv_18 + arv_18),
           dem_20 = group_frac(map, adv_20, adv_20 + arv_20),
           dem_avg = group_frac(map, ndv, ndv + nrv),
           black = group_frac(map, pop_black),
           hisp = group_frac(map, pop_hisp),
           minority = group_frac(map, pop - pop_white))

opt_comp2 = redist_shortburst(set_pop_tol(map, 0.01), scorer, maximize=F,
                              max_bursts=100, return_all=F, backend="flip",
                              flip_eprob=0.1)

}

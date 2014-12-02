rm(list = ls())

library('ggplot2')
library('reshape2')
library('plyr')
library('extrafont')

library('locfit')

set.seed(134678684)

setwd("M:/Google Drive/Classes/Stat 37400/project/")


ggplot.format = function(gplot, legend_loc=c(1, 0)) {
    gplot = gplot +
        xlab('Maturity') + ylab('Yield') +
        theme_bw() +
        theme(legend.position=legend_loc, legend.justification=legend_loc,
              legend.title=element_blank(),
              legend.background=element_rect(color="lightgrey"))
    return(gplot)
}

# Nelson Siegel Estimation
ns.pred = function(pars, mats) {
    res = pars[1]
    res = res + pars[2]*(1-exp(-mats/pars[5]))/(mats/pars[5])
    res = res + pars[3]*((1-exp(-mats/pars[5]))/(mats/pars[5]) -
                         exp(-mats/pars[5]))
    res = res + pars[4]*((1-exp(-mats/pars[6]))/(mats/pars[6]) -
                         exp(-mats/pars[6]))
    return(res)
}
ns.mse = function(pars, yields, mats) {
    return(mean((yields - ns.pred(pars, mats))^2))
}
ns.est = function(yields, mats) {
    init = c(3, -.1, 1, 2, .5, 15)
    res = optim(init, function (pars) { ns.mse(pars, yields, mats) },
                method='BFGS')
    return(res$par)
}
ns.bootstrap = function(yields, mats, num_bootstrap=200,
                        mat_grid=seq(.08, 30, length=100)) {
    n = length(yields)
    bs_results = matrix(nrow=num_bootstrap, ncol=length(mat_grid))
    for (b in 1:num_bootstrap) {
        samp_ind = sample(n, n, replace=TRUE)
        yields_bs = yields[samp_ind]
        mats_bs = mats[samp_ind]
        bs_results[b,] = ns.pred(ns.est(yields_bs, mats_bs), mat_grid)
    }
    quantiles = apply(bs_results, 2, quantile, probs=c(0.025, 0.975))

    res = data.frame(lower=quantiles[1,], upper=quantiles[2,])
    return(res)
}

# Estimate all regressions
est_for_date = function(date, data) {
    maturity_grid = seq(.08, 30, length=100)

    results = list()

    datum = data[data$date == as.Date(date),]
    datum$variable = 'Raw Yields'
    results[[1]] = datum

    alpha = seq(.05, .5, length=25)
    gcv_res = gcvplot(yield ~ maturity, data=datum, alpha=alpha)
    loclin_mod = locfit(yield ~ maturity, data=datum,
                        alpha=alpha[which.min(gcv_res$values)])
    loclin_results = predict(loclin_mod, maturity_grid, se.fit=TRUE)

    df_loclin = data.frame(maturity=maturity_grid, yield=loclin_results$fit,
                           variable='Local Linear')
    df_loclin$lower = loclin_results$fit - 1.96*loclin_results$se.fit
    df_loclin$upper = loclin_results$fit + 1.96*loclin_results$se.fit
    results[[2]] = df_loclin

    ns = ns.est(datum$yield, datum$mat)
    df_ns = data.frame(maturity=maturity_grid, yield=ns.pred(ns, maturity_grid),
                       variable='Svensson')
    results[[3]] = df_ns
    names(results) = c('raw', 'loclin', 'ns')

    return(results)
}


# Load data
data = read.csv("./data/treasury_data.csv")
data$date = as.Date(data$date)


ex_dates = c('2006-06-14', '2012-01-12')

# Example data
df = data.frame()
for (ex_date in ex_dates) {
    datum = data[data$date == as.Date(date),]
    datum$variable = 'Raw Yields'
    results[[1]] = datum
}


# Example curve fits
ex_dates = c('2006-06-14', '2012-01-12')

for (ex_date in ex_dates) {
    res = est_for_date(ex_date, data)
    res$ns = cbind(res$ns,
                   ns.bootstrap(res$raw$yield, res$raw$maturity,
                                mat_grid=res$ns$maturity, num_bootstrap=100))
    res$ns$variable = 'Fitted Values'
    res$loclin$variable = 'Fitted Values'

    gplot = ggplot(res$raw, aes(x=maturity, y=yield, color=variable)) +
        geom_ribbon(data=res$ns, aes(x=maturity, ymin=lower, ymax=upper),
                    fill='lightgrey', alpha=.7, size=0, color='white') +
        geom_point(alpha=.45) +
        geom_line(data=res$ns)

    pdf(paste0("./include/ex_ns_", ex_date, ".pdf"), height=4.5, width=6,
        family='CM Roman')
    print(ggplot.format(gplot))
    dev.off()

    gplot = ggplot(res$raw, aes(x=maturity, y=yield, color=variable)) +
        geom_ribbon(data=res$loclin, aes(x=maturity, ymin=lower, ymax=upper),
                    fill='lightgrey', alpha=.7, size=0, color='white') +
        geom_point(alpha=.45) +
        geom_line(data=res$loclin)

    pdf(paste0("./include/ex_loclin_", ex_date, ".pdf"), height=4.5, width=6,
        family='CM Roman')
    print(ggplot.format(gplot))
    dev.off()
}


# Comparisons
ex_dates = c('2006-06-14')

for (ex_date in ex_dates) {
    res = est_for_date(ex_date, data)

    gplot = ggplot(res$raw, aes(x=maturity, y=yield, color=variable)) +
        geom_point(alpha=.45) +
        geom_line(data=res$ns) +
        geom_line(data=res$loclin)


    pdf(paste0("./include/comparison_", ex_date, ".pdf"), height=4.5, width=6,
        family='CM Roman')
    print(ggplot.format(gplot))
    dev.off()
}


# Daily Changes
date_pairs = list(
    c('2008-09-11', '2008-09-12', '2008-09-15'),
    c('2009-03-16', '2009-03-17', '2009-03-18')
)

for (date_pair in date_pairs) {
    res1 = est_for_date(date_pair[1], data)
    res1$loclin$date = format(as.Date(date_pair[1]), format="%B %d, %Y")
    res2 = est_for_date(date_pair[2], data)
    res2$loclin$date = format(as.Date(date_pair[2]), format="%B %d, %Y")
    res3 = est_for_date(date_pair[3], data)
    res3$loclin$date = format(as.Date(date_pair[3]), format="%B %d, %Y")

    gplot = ggplot(data=res1$raw, aes(x=maturity, y=yield, color=date, fill=date)) +
        geom_ribbon(data=res1$loclin, aes(x=maturity, ymin=lower, ymax=upper),
                    alpha=.25, size=0, color='white') +
        geom_ribbon(data=res2$loclin, aes(x=maturity, ymin=lower, ymax=upper),
                    alpha=.25, size=0, color='white') +
        geom_ribbon(data=res3$loclin, aes(x=maturity, ymin=lower, ymax=upper),
                    alpha=.25, size=0, color='white') +
        geom_line(data=res1$loclin) +
        geom_line(data=res2$loclin) +
        geom_line(data=res3$loclin)

    pdf(paste0("./include/change_", date_pair[1], ".pdf"), height=4.5, width=6,
        family='CM Roman')
    print(ggplot.format(gplot))
    dev.off()

}

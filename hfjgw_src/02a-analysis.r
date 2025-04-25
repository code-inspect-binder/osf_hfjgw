
# working directory
PATH <- "/n/legewie_lab/Lab/neighborhood-boundaries/replication-material"
## load libraries
library("MASS")
library("tidyverse")
library("stargazer")
library("glue")
library("spdplyr")
library("multiwayvcov")
library("lmtest")
library("car", pos = length(search()))
library("scales")
library("Hmisc")
library("rgeos")
library("maptools")
# utility functions
source(file.path(PATH, "00-utils.r"))
source(file.path(PATH, "areal_wombling.r"))

scale_color_cts <- function(domain, x = NULL, colors = c("#F1EEF6", "#034E7B"), n = 9) {
    if(is.null(x) & is.null(domain)) stop("Set either 'x' or 'domain'")
    palette   <- colorRampPalette(colors)(n)
    values <- seq(domain[1], domain[2], length.out = n)
    scale  <- nearestScheme(data.frame(col = palette, values = values))
    # scale  <- colourschemes::nearestScheme(data.frame(col = palette, values = values))
    f      <- function(x, mis = "#f6f6f6") {
        col <- rep(mis, length(x))
        col[!is.na(x)] <- scale(x[!is.na(x)])
        return(col)
    }
    return(f)
}
scale_color <- scale_color_cts(domain = c(0,1), n = 9)
fit_glmnb <- function(d, formula, cluster) {
    m <- glm.nb(formula, data = d[sel_chi,])
    vcov_cluster <- cluster.vcov(m, cluster = cluster)
    coeftest(m, vcov_cluster)
}
mi_combine <- function(m, df) {
    k       <- length(m)
    out     <- m[[1]]
    out[,1] <- m %>% map(~.x[,1]) %>% {do.call(cbind, .)} %>% rowMeans()
    wvar    <- m %>% map(~.x[,2]) %>% {do.call(cbind, .)} %>% apply(1, function(x) mean(x^2))
    bvar    <- m %>% map(~.x[,1]) %>% {do.call(cbind, .)} %>% apply(1, function(x) var(x))
    out[,2] <- sqrt(wvar + (1 + 1/k) * bvar)
    out[,3] <-  out[,1] / out[,2]
    out[,4] <-  2 * pt(-abs(out[,3]), df = df)
    out
}

# read data
ccahs  <- read_rds(file.path(PATH, "data", "ccahs-2001-2003-recoded.rds"))
ccahs  <- ccahs %>% mutate(cb_code = str_c(state, county, tract, block), bg_code = str_sub(cb_code, 1, 12))
chi_cb <- read_rds(file.path(PATH, "data", "census-cook-cb-2010.rds"))
chi_cb <- chi_cb@data %>%
    filter(chicago) %>%
    mutate(
        cb_code                  = paste0(state, county, tract, block),
        bg_code                  = paste0(state, county, tract, block_group),
        ct_code                  = paste0(state, county, tract),
        crime_violent_2001_rate  = crime_violent_2001/pop,
        crime_property_2001_rate = crime_property_2001/pop,
        crime_violent_2006_rate  = crime_violent_2006/pop,
        crime_property_2006_rate = crime_property_2006/pop,
        crime_violent_2010_rate  = crime_violent_2010/pop,
        crime_property_2010_rate = crime_property_2010/pop,
        major_road               = roads_primary | roads_secondary
    )
chi_cb_pv <- read_rds(file.path(PATH, "data", "census-cook-cb-plausible-values.rds"))
# chi_cb_pv <- chi_cb_pv %>% map(~ .x %>% mutate(major_road = roads_primary | roads_secondary))

## Tab 1: Correlation matrix
corstarsl <- function(x) {
    require(Hmisc)
    x <- as.matrix(x)
    R <- Hmisc::rcorr(x)$r
    p <- Hmisc::rcorr(x)$P
    # define notions for significance levels; spacing is important.
    mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
    # trunctuate the matrix that holds the correlations to two decimal
    R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
    # build a new matrix that includes the correlations with their apropriate stars
    Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
    diag(Rnew) <- paste(diag(R), " ", sep="")
    rownames(Rnew) <- colnames(x)
    colnames(Rnew) <- paste(colnames(x), "", sep="")
    # remove upper triangle
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
    # remove last column and return the matrix (which is now a data frame)
    Rnew <- cbind(Rnew[1:length(Rnew)-1])
    return(Rnew)
}

sel_chi  <- with(chi_cb, pop !=0 & hh != 0 & !park_area & !is.infinite(edge_race_areal) & !is.infinite(p_race_white_blv) & !is.infinite(p_race_black_blv) & !is.infinite(p_race_hisp_blv))
COR      <- corstarsl(with(chi_cb[sel_chi,], cbind(p_race_white, p_race_black, p_race_hisp, p_white_splag, p_black_splag, p_hisp_splag, p_race_white_blv, p_race_black_blv, p_race_hisp_blv, edge_race_areal)))
COR$Mean <- round(apply(chi_cb[sel_chi,rownames(COR)], 2, mean, na.rm = TRUE), 2)
COR$SD   <- round(apply(chi_cb[sel_chi,rownames(COR)], 2, sd, na.rm = TRUE), 2)
COR      <- select(COR, Mean, SD, everything())
rownames(COR) <- c("1. Prop. White", "2. Prop. Black", "3. Prop. Hispanic", "4. Prop. White (spatial lag)", "5. Prop. Black (spatial lag)", "6. Prop. Hispanic (spatial lag)", "7. NBHD Boundary (White)", "8. NBHD Boundary (Black)", "9. NBHD Boundary (Hispanic)", "10. NBHD Boundary (combined)")
names(COR)[-(1:2)] <- 1:(ncol(COR) - 2)
# correlation table
stargazer(COR, digits = 2, initial.zero = TRUE, summary = FALSE, type = "text")

## Tab A2: Correlation matrix
COR      <- corstarsl(with(chi_cb[sel_chi,], cbind(p_race_white_blv, p_race_black_blv, p_race_hisp_blv, edge_race_areal, major_road, touches_park, river, elementary_school_district)))
COR$Mean <- round(apply(chi_cb[sel_chi,rownames(COR)], 2, mean, na.rm = TRUE), 2)
COR$SD   <- round(apply(chi_cb[sel_chi,rownames(COR)], 2, sd, na.rm = TRUE), 2)
COR      <- select(COR, Mean, SD, everything())
rownames(COR) <- c("1. NBHD Boundary (White)", "2. NBHD Boundary (Black)", "3. NBHD Boundary (Hispanic)", "4. NBHD Boundary (combined)", "5. Adjacent to Major Road", "6. Adjacent to Park", "7. Adjacent to River", "8. Elem. School District Border")
names(COR)[-(1:2)] <- 1:(ncol(COR) - 2)
# correlation table
stargazer(COR, digits = 2, initial.zero = TRUE, summary = FALSE, type = "text")

## Tab 2: Neighborhood boundaries and violent crime
sel_chi <- with(chi_cb, pop !=0 & hh != 0 & !park_area & !is.infinite(edge_race_areal))
U     <- "log(pop) + crime_violent_2001_rate + crime_property_2001_rate + std(p_race_black) + std(p_race_hisp) + std(p_race_asian) + std(con_disadv_bg) + std(res_instab_bg) + std(immi_con_bg) + std(hhi) + std(age_15_35_male)"
U     <- "I(pop/100) + crime_violent_2001 + crime_property_2001 + std(p_race_black) + std(p_race_hisp) + std(p_race_asian) + std(con_disadv_bg) + std(res_instab_bg) + std(immi_con_bg) + std(hhi) + std(age_15_35_male)"
SPLAG <- "std(p_black_splag) + std(p_hisp_splag) + std(p_asian_splag) + std(hhi_splag)"
f <- list(
        "crime_violent_2011 ~ {U}",
        "crime_violent_2011 ~ {U} + std(edge_race_areal)",
        "crime_violent_2011 ~ {U} + std(edge_race_areal) + major_road + touches_park + river + elementary_school_district",
        "crime_violent_2011 ~ {U} + std(edge_race_areal) + major_road + touches_park + river + elementary_school_district + {SPLAG}"
    ) %>%
    map(glue) %>%
    map(as.formula)
# fit negative bin models
m <- map(f, ~glm.nb(.x, data = chi_cb[sel_chi,]))
# clustered standard errors
cluster      <- chi_cb$bg_code[sel_chi]
vcov_cluster <- map(m, cluster.vcov, cluster = cluster)
m_cluster    <- map2(m, vcov_cluster, coeftest)
# regression table
stargazer(m, type = "text")
stargazer(m_cluster, type = "text")
# Multi-collinearity
fit <- lm(f[[3]], data = chi_cb[sel_chi,])
vif(fit)
fit <- lm(f[[4]], data = chi_cb[sel_chi,])
vif(fit)
# plausible values
m1 <- chi_cb_pv %>%
    map(fit_glmnb, formula = f[[1]], cluster = cluster) %>%
    mi_combine(df = m[[1]]$df.residual)
m2 <- chi_cb_pv %>%
    map(fit_glmnb, formula = f[[2]], cluster = cluster) %>%
    mi_combine(df = m[[1]]$df.residual)
m3 <- chi_cb_pv %>%
    map(fit_glmnb, formula = f[[3]], cluster = cluster) %>%
    mi_combine(df = m[[1]]$df.residual)
m4 <- chi_cb_pv %>%
    map(fit_glmnb, formula = f[[4]], cluster = cluster) %>%
    mi_combine(df = m[[1]]$df.residual)
stargazer(m1, m2, m3, m4, type = "text")

## Tab 3: Supplementary analysis: Neighborhood boundaries and violent crime
f1 <- crime_violent_2011 ~ I(pop/100) + crime_violent_2001 + crime_property_2001 + 
    std(p_race_black) + std(p_race_hisp) + std(p_race_asian) + 
    std(con_disadv_bg) + std(res_instab_bg) + std(immi_con_bg) + 
    std(hhi) + std(age_15_35_male) + std(edge_race_wb) + std(edge_race_wh) + std(edge_race_bh) + major_road + 
    touches_park + river + elementary_school_district + std(p_black_splag) + 
    std(p_hisp_splag) + std(p_asian_splag) + std(hhi_splag)
f2 <- crime_violent_2011 ~ offset(log(pop/100)) + crime_violent_2001_rate + crime_property_2001_rate + 
    std(p_race_black) + std(p_race_hisp) + std(p_race_asian) + 
    std(con_disadv_bg) + std(res_instab_bg) + std(immi_con_bg) + 
    std(hhi) + std(age_15_35_male) + std(edge_race_areal) + major_road + 
    touches_park + river + elementary_school_district + std(p_black_splag) + 
    std(p_hisp_splag) + std(p_asian_splag) + std(hhi_splag)
f3 <- crime_violent_day_2011 ~ I(pop/100) + crime_violent_2001 + crime_property_2001 + 
    std(p_race_black) + std(p_race_hisp) + std(p_race_asian) + 
    std(con_disadv_bg) + std(res_instab_bg) + std(immi_con_bg) + 
    std(hhi) + std(age_15_35_male) + std(edge_race_areal) + major_road + 
    touches_park + river + elementary_school_district + std(p_black_splag) + 
    std(p_hisp_splag) + std(p_asian_splag) + std(hhi_splag)
f4 <- crime_violent_night_2011 ~ I(pop/100) + crime_violent_2001 + crime_property_2001 + 
    std(p_race_black) + std(p_race_hisp) + std(p_race_asian) + 
    std(con_disadv_bg) + std(res_instab_bg) + std(immi_con_bg) + 
    std(hhi) + std(age_15_35_male) + std(edge_race_areal) + major_road + 
    touches_park + river + elementary_school_district + std(p_black_splag) + 
    std(p_hisp_splag) + std(p_asian_splag) + std(hhi_splag)
f5 <- crime_violent_2011_2016 ~ I(pop/100) + crime_violent_2001 + crime_property_2001 + 
    std(p_race_black) + std(p_race_hisp) + std(p_race_asian) + 
    std(con_disadv_bg) + std(res_instab_bg) + std(immi_con_bg) + 
    std(hhi) + std(age_15_35_male) + std(edge_race_areal) + major_road + 
    touches_park + river + elementary_school_district + std(p_black_splag) + 
    std(p_hisp_splag) + std(p_asian_splag) + std(hhi_splag)
m1 <- chi_cb_pv %>%
    map(fit_glmnb, formula = f1, cluster = cluster) %>%
    mi_combine(df = m[[1]]$df.residual)
m2 <- chi_cb_pv %>%
    map(fit_glmnb, formula = f2, cluster = cluster) %>%
    mi_combine(df = m[[1]]$df.residual)
m3 <- chi_cb_pv %>%
    map(fit_glmnb, formula = f3, cluster = cluster) %>%
    mi_combine(df = m[[1]]$df.residual)
m4 <- chi_cb_pv %>%
    map(fit_glmnb, formula = f4, cluster = cluster) %>%
    mi_combine(df = m[[1]]$df.residual)
m5 <- chi_cb_pv %>%
    map(fit_glmnb, formula = f5, cluster = cluster) %>%
    mi_combine(df = m[[1]]$df.residual)
# regression table
stargazer(m2, m3, m4, m5, m1, type = "text")


## Tab 4: Neighborhood boundaries, violent crimes, perceived violence and homicides

# model 1: violent crime (model 3 from tab 1)
f <- crime_violent_2011 ~ I(pop/100) + crime_violent_2001 + crime_property_2001 + 
    std(p_race_black) + std(p_race_hisp) + std(p_race_asian) + 
    std(con_disadv_bg) + std(res_instab_bg) + std(immi_con_bg) + 
    std(hhi) + std(age_15_35_male) + std(edge_race_areal) + major_road + 
    touches_park + river + elementary_school_district + std(p_black_splag) + 
    std(p_hisp_splag) + std(p_asian_splag) + std(hhi_splag)
m4 <- chi_cb_pv %>%
    map(fit_glmnb, formula = f, cluster = cluster) %>%
    mi_combine(df = m[[1]]$df.residual)
# model 2: Homicides
f <- crime_homicide_night_2011 ~ I(pop/100) + crime_violent_2001 + crime_property_2001 + 
    std(p_race_black) + std(p_race_hisp) + std(p_race_asian) + 
    std(con_disadv_bg) + std(res_instab_bg) + std(immi_con_bg) + 
    std(hhi) + std(age_15_35_male) + std(edge_race_areal) + major_road + 
    touches_park + river + elementary_school_district + std(p_black_splag) + 
    std(p_hisp_splag) + std(p_asian_splag) + std(hhi_splag)
m2 <- chi_cb_pv %>%
    map(fit_glmnb, formula = f, cluster = cluster) %>%
    mi_combine(df = m[[1]]$df.residual)
# model 3: perceived violence
sel_ccahs <- with(ccahs, pop !=0 & hh != 0 & !is.infinite(edge_race_areal) & race != 5 &
    complete.cases(violence_perceived_scale, female, edu_years, emp_status, race, age, home_ownership, foreign_born, edge_race_areal))
f <- std(violence_perceived_scale) ~ female + std(edu_years) + factor(emp_status) + factor(race) +
    std(age) + home_ownership + foreign_born +
    log(pop) + std(p_race_black) + std(p_race_hisp) + std(p_race_asian) +
    std(con_disadv_ct) + std(res_instab_ct) + std(immi_con_ct) + 
    std(hhi) + std(age_15_35_male) + std(edge_race_areal) + std(p_black_splag) + 
    std(p_hisp_splag) + std(p_asian_splag) + std(hhi_splag)
f <- std(violence_perceived_scale) ~ female + std(edu_years) + factor(emp_status) + factor(race) +
    std(age) + home_ownership + foreign_born +
    log(pop) + std(p_race_black) + std(p_race_hisp) + std(p_race_asian) +
    std(con_disadv_bg) + std(res_instab_bg) + std(immi_con_bg) + 
    std(hhi) + std(age_15_35_male) + std(edge_race_areal) + std(p_black_splag) + 
    std(p_hisp_splag) + std(p_asian_splag) + std(hhi_splag)
m3         <- lm(f, data = ccahs[sel_ccahs,])
vcov_m3    <- cluster.vcov(m3, cluster = ccahs$bg_code[sel_ccahs])
m3_cluster <- coeftest(m3, vcov_m3)

# regression table
stargazer(m4, m2, m3, type = "text", keep = c("edge_.*"), keep.stat = "n", dep.var.labels.include = FALSE)


## Figure 1
chi_bg <- read_rds(file.path(PATH, "data", "census-cook-bg-2010.rds")) %>% filter(chicago)
# exclude O'Hare International Airport (tract `17031980000` and `17031770602`)
sel_airport <- chi_bg$tract %in% c("980000", "770602")
sel_airport <- sel_airport | rgeos::gTouches(rgeos::gUnaryUnion(chi_bg[sel_airport,]), chi_bg, byid = TRUE)[,1]
chi_bg      <- chi_bg[!sel_airport,]
# chicago line file
sl_bg  <- areal_wombling(chi_bg, c("p_race_white", "p_race_black", "p_race_hisp", "p_race_asian", "hhinc_average"))
chi    <- unionSpatialPolygons(chi_bg, IDs = rep(1, length(chi_bg)))

png(file.path(PATH, "chi-bg-01-choropleth.png"), width=3.05, height=4.5, units="in", res=600)
par(mfrow=c(1,1), mar=c(0,0,0,0), cex=0.8, cex.lab=0.8, cex.main=0.8, mgp=c(1.2,0.15,0), cex.axis=0.7, tck=-0.01)
plot(chi_bg, lwd = 0.1, col = scale_color(chi_bg$p_race_black), bor = "white")
# plot(chi, lwd = 0.5)
vals <- quantile(chi_bg$p_race_black, probs = c(0, 0.25, .5, 0.75, 1), na.rm = TRUE) 
legend(-87.87, 41.73, legend = c("0%", "25%","50%", "75%", "100%"), fill = scale_color(vals), cex = 0.8,
    box.lty = 0, border = "#00000000", title = "Percent Black", title.adj = 3.5)
dev.off()
png(file.path(PATH, "chi-bg-02-borders.png"), width=3.05, height=4.5, units="in", res=600)
par(mfrow=c(1,1), mar=c(0,0,0,0), cex=0.8, cex.lab=0.8, cex.main=0.8, mgp=c(1.2,0.15,0), cex.axis=0.7, tck=-0.01)
plot(sl_bg, lwd = rescale(sl_bg$p_race_black_blv, to = c(0.1, 1.25)))
plot(chi, lwd = 0.5, add = TRUE)
legend(-87.87, 41.73, legend = c("0.0", "0.25", "0.5", "0.75", "1.0"), lwd = rescale(c(0, 0.25, 0.5, 0.75, 1), to = c(0.1, 1.25)),
    cex = 0.8, box.lty = 0, border = "#00000000", title = "Boundary Value", title.adj = 3.5)
dev.off()
png(file.path(PATH, "chi-bg-03-border-areas.png"), width=3.05, height=4.5, units="in", res=600)
par(mfrow=c(1,1), mar=c(0,0,0,0), cex=0.8, cex.lab=0.8, cex.main=0.8, mgp=c(1.2,0.15,0), cex.axis=0.7, tck=-0.01)
plot(chi_bg, lwd = 0.1, col = scale_color(chi_bg$p_race_black_blv), bor = "white")
legend(-87.87, 41.73, legend = c("0.0", "0.25", "0.5", "0.75", "1.0"), fill = scale_color(c(0, 0.25, 0.5, 0.75, 1)),
    cex = 0.8, box.lty = 0, border = "#00000000", title = "Boundary Value", title.adj = 3.5)
dev.off()

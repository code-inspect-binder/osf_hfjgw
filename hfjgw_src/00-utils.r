
open_shapefile <- function(path, proj = sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")) {
    tmp <- tempdir()
    # unzip
    utils::unzip(path, exdir = tmp)
    # get filebase of shapefile
    files <- list.files(tmp)
    fn    <- strsplit(files[stringr::str_detect(files, "\\.shp")], "\\.")[[1]][1]
    # open shapefile
    spdf <- maptools::readShapeSpatial(file.path(tmp, fn), proj4string = proj)
    # remove unzipped files
    list.files(tmp) %>% keep(str_detect, pattern = fn) %>% map_chr(~ file.path(tmp, .)) %>% file.remove()
    # return
    return(spdf)
}

# From colourschemes package
nearestScheme <- function (valueColours) {
    force(valueColours)
    f = function(data) {
        colours = rep(NA, length(data))
        for (i in 1:length(data)) {
            d = data[i]
            diffs = abs(valueColours$value - d)
            colours[i] = as.character(valueColours[which.min(diffs),
                "col"])
        }
        return(colours)
    }
    class(f) <- c("nearestScheme", "colourScheme", "function")
    attr(f, "type") <- "nearest colour scheme"
    return(f)
}


na_codes <- function(x, ...) ifelse(x %in% c(...), NA, x)

# spatial lag
spatial_lag <- function(x, W, fun = funs(sum(., na.rm = TRUE))) {
    out <- purrr::map2_dbl(W$neighbours, W$weights, ~ lazyeval::lazy_eval(fun[[1]], list(. = x[.x] * .y)))
    out <- ifelse(is.na(x), NA, out)
    return(out)
}


# spatial weight
spatial_weight <- function(sp, km = 2, proj4 = CRS("+proj=longlat +ellps=GRS80"), zero.policy = TRUE, sparse_matrix = TRUE) {
    # project to  longitude-latitude decimal degrees
    sp <- sp::spTransform(sp, proj4)
    # Bivand et al (2008: 251f)
    coords <- sp::coordinates(sp)
    # Identify all neighbours within 2km
    sp_nb <- spdep::dnearneigh(coords, d1 = 0, d2 = km, row.names = row.names(sp), longlat = TRUE)
    # Estimate the distances between the neighbours
    dsts <- spdep::nbdists(sp_nb, coords, longlat = TRUE)
    if(all(purrr::map_lgl(dsts, is.null))) return(matrix(0, nrow = nrow(sp), ncol = ncol(sp)))
    # take the inverse of the distances
    W <- lapply(dsts, function(x) 1 / x)
    # create a weight, by rescaling the inverse distance so they sum up to one -> a mean
    W <- spdep::nb2listw(sp_nb, glist = W, style = "W", zero.policy = zero.policy)
    # change the weights object to a sparse Matrix
    if (sparse_matrix)
        W <- spdep::listw2mat(W)
    # return spatial weight matrix
    return(W)
}


spatial_weight_neighbors <- function(sp, proj4 = CRS("+proj=longlat +ellps=GRS80"), zero.policy = TRUE, sparse_matrix = TRUE) {
    # project to  longitude-latitude decimal degrees
    sp <- sp::spTransform(sp, proj4)
    coords <- sp::coordinates(sp)
    # neighboring regions
    sp_nb <- spdep::poly2nb(sp)
    # Estimater the distances between the neighbours
    dsts <- spdep::nbdists(sp_nb, coords, longlat = TRUE)
    # take the inverse of the distances
    W <- lapply(dsts, function(x) 1 / x)
    # create a weight, by rescaling the inverse distance so they sum up to one -> a mean
    W <- spdep::nb2listw(sp_nb, glist = W, style = "W", zero.policy = zero.policy)
    # change the weights object to a sparse Matrix
    if (sparse_matrix)
        W <- spdep::listw2mat(W)
    # return spatial weight matrix
    return(W)
}

str_digits <- function(x, digits = 3) {
    sel <- !is.na(x)
    len <- stringr::str_length(x[sel])
    chars <- unique(len[len < digits])
    for (l in chars)
        x[sel][len == l] <- paste0(paste(rep("0", digits - l), collapse = ""),x[sel][len == l])
    return(x)
}

std <- function (x,select=TRUE) (x-mean(x[select],na.rm=TRUE))/sd(x[select],na.rm=TRUE)

fac <- function(x, factors, rotation = "varimax", scores = "regression") {
    ok <- complete.cases(x)
    fa <- factanal(x[ok,], factors, rotation=rotation, scores=scores)
    score <- rep(NA, fa$n.obs + sum(!ok))
    score[ok] <- fa$scores
    return(score)
}

# herfindahl or fractionalization index
f_index <- function(x) 1 - rowSums(x^2)
# ethnic polarization
p_index <- function(x) 4 * (rowSums((x^2 * (1 - x))))
# Theil index (Reardon and Firebaugh 2002, 37)
th_index <- function(x) rowSums(x * ifelse(x != 0, log(1 / x), 0))
# index of dissimilarity(ID) for census tracts
seg_index <- function(x1, x2) 0.5 * sum(abs(x1/sum(x1) - x2/sum(x2)))
# Theil multigroup segregation
theilseg_index <- function(x, pop) {
    Logger <- log(1 / colMeans(x))
    Logger[is.infinite(Logger)] <- 0

    Loggr <- log(t(t(x) / colMeans(x)))
    Loggr[is.infinite(Loggr)] <- 0

    # exclude columns with na (when a group is zero in all blocks)
    sel <- !plyr::aaply(Loggr,2,function(k) all(is.na(k)))
    x <- x[,sel,drop=FALSE]
    Logger <- Logger[sel]
    Loggr <- Loggr[,sel,drop=FALSE]

    # the index
    idx <- (1 / (sum(colMeans(x) * Logger))) * sum( colMeans(x)
        * colSums((pop/sum(pop)) * ( t(t(x) / colMeans(x)) * Loggr )) )
    # return 0 if there is only one group
    return(ifelse(sum(sel) > 1, idx, 0))
}
# segregation indicies on census tract level
seg_indices <- function(df) {
    # Multigroup: HHI
    # ID.hhi = with(df, hhiseg_index(x = cbind(p_race_white, p_race_black, p_race_asian, p_race_hisp, p.other.cb), pop = pop))
    dplyr::summarise(df,
        # ID: White/Black
        seg_wb = seg_index(p_race_white * pop, p_race_black * pop),
        # White/Asian
        seg_wa = seg_index(p_race_white * pop, p_race_asian * pop),
        # ID: White/Hispanic
        seg_wh = seg_index(p_race_white * pop, p_race_hisp * pop),
        # ID: Black/Hisp
        seg_bh = seg_index(p_race_black * pop, p_race_hisp * pop),
        # ID: Black/Asian
        seg_ba = seg_index(p_race_black * pop, p_race_asian * pop),
        # ID: Hispanis/Asian
        seg_ha = seg_index(p_race_hisp * pop, p_race_asian * pop),
        # Multigroup: Theil
        seg_theil = theilseg_index(x = cbind(p_race_white, p_race_black, p_race_asian, p_race_hisp, p_race_other), pop = pop)
    )
}

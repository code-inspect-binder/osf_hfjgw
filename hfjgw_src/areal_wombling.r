areal_wombling <- function(sp, x, threshold = NA, dist = function(x) abs(x[1] - x[2])) {
    # check `reshape2:::parse_formula` for p.white.cb + p.black.cb + p.hisp.cb + p.asian.cb ~ 1
    # Coerce simple feature geometries to corresponding Spatial* objects
    if (is(sp, "sf")) sp <- as(sp, "Spatial")
    # get borders as line segments in SpatialLinesDataFrame
    sl <- border_lines(sp)
    # boundary likelihood value (BLV) and boundary membership value (BMV) to data.frame
    dots_format <- function(s, suffix)
        s %>% setNames(paste0(x, suffix)) %>%
            as.list() %>% lapply(FUN = as.formula, env = environment())
    dots_blv <- dots_format(sprintf("~ dist(sp@data[['%s']][c(i, j)])[1]", x), suffix = "_blv")
    dots_bmv <- dots_format(sprintf("~ %s_blv/max(%s_blv, na.rm = TRUE)", x, x), suffix = "_bmv")
    if(!is.na(threshold))
        dots_bmv <- dots_format(sprintf("~ %s_blv > %s", x, threshold), suffix = "_bmv")
    sl@data <- sl@data %>%
        dplyr::group_by(i, j) %>%
        dplyr::mutate_(.dots = dots_blv) %>%
        dplyr::ungroup() %>%
        dplyr::mutate_(.dots = dots_bmv) %>%
        as.data.frame()
    # return
    return(sl)
}


border_lines <- function(sp, longlat = TRUE) {
    # Coerce simple feature geometries to corresponding Spatial* objects
    if (is(sp, "sf")) sp <- as(sp, "Spatial")
    P  <- sp::polygons(sp)
    # get adjacency matrix A
    # nb <- spdep::poly2nb(sp, row.names = rownames(sp), queen = FALSE)
    # A  <- nb2mat::nb2mat(nb, style = "B", zero.policy = TRUE)
    nb <- spdep::poly2nb(sp, queen = FALSE)
    # create data.frame with adjacent areas
    greater_than <- function(a, b) a[a > b]
    data <- data.frame(i = 1:length(nb), j = NA) %>%
        group_by(i, j) %>%
        do(expand.grid(i = .$i, j = greater_than(nb[[.$i]], .$i))) %>%
        as.data.frame()
    # area borders as SpatialLines
    lines <- apply(data, 1, function(d) {
        i <- as.numeric(d["i"])
        j <- as.numeric(d["j"])
        # get list of coordinates for polygons
        c1 <- plyr::llply(P@polygons[[i]]@Polygons, sp::coordinates)
        c2 <- plyr::llply(P@polygons[[j]]@Polygons, sp::coordinates)
        # get borders for each combination of polygons
        grid <- expand.grid(s1 = 1:length(c1), s2 = 1:length(c2))
        line <- apply(grid, 1, function(obj) {
            a   <- c1[[obj["s1"]]]
            b   <- c2[[obj["s2"]]]
            # select intersecting rows
            sel <- a[, 1] %in% b[, 1] & a[, 2] %in% b[, 2]
            if(sum(sel) == 0) return(NULL)
            # create Line object for each sequence of matching coordinates
            runs <- rle(sel)
            runs <- data.frame(
                    val = runs$values,
                    i   = c(1, cumsum(runs$length) + 1)[-(length(runs$length) + 1)],
                    len = runs$length) %>%
                dplyr::filter(val)
            # coordinates for each sequence
            pos    <- plyr::alply(as.matrix(runs), 1, . %>% {.[["i"]] : (.[["i"]] + .[["len"]] - 1)})
            coords <- plyr::llply(pos, . %>% a[., , drop = FALSE])
            # remove duplicate line elements
            len_one <- plyr::laply(coords, nrow) == 1
            if (!all(len_one) & any(len_one)) {
                B <- do.call(rbind, coords)
                coords <- plyr::llply(coords, function(A) {
                    if(nrow(A) > 1) return(A)
                    cond <- sum(A[, 1] == B[, 1] & A[, 2] == B[, 2]) > 1
                    if(cond) return(NULL)
                    return(A)
                })
                coords <- coords[!sapply(coords, is.null)]
            }
            # return list of Line objects (one element for each sequence of coordinates)
            plyr::llply(coords, sp::Line)
        })
        segments <- unlist(line[!sapply(line, is.null)], recursive=FALSE)
        sp::Lines(segments, ID = sprintf("i%s_j%s", i, j))
    })
    sl <- sp::SpatialLines(lines[!sapply(lines, is.null)], proj4string = sp::CRS(sp::proj4string(sp)))
    # SpatialLinesDataFrame from SpatialLines and data
    sldf <- sp::SpatialLinesDataFrame(sl, data, match.ID = FALSE)
    # sldf$length <- SpatialLinesLengths(sldf, longlat = longlat)
    return(sldf)
}

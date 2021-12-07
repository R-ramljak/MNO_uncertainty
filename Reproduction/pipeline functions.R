# pop_gen_param <- function (tile.num = 10000, # number of tiles
#           base.tile.size = 100, # size of a single tile
#           city.num = 4, # number of single cities
#           city.size = 2000, # size of a single city polygon
#           hole.num = 20, # number of single holes
#           hole.size = 400) 
# {
#   nms <- names(formals(pop_gen_param))
#   lst <- sapply(nms, get, envir = environment(), simplify = FALSE)
#   class(lst) <- "pop_gen_param"
#   lst
# }


# Population generation function
pop_gen <- function(tile.num, base.tile.size, 
                    city.num, city.size, city.shape = "SQUARE", 
                    hole.num, hole.size, hole.shape = "SQUARE",
                    pop.dist.ls) {
  
  
  # save area parameters
  area.params <- list(tile.num = tile.num, 
                      base.tile.size = base.tile.size, 
                      city.num = city.num, 
                      city.size = city.size, 
                      city.shape = city.shape, 
                      hole.num = hole.num, 
                      hole.size = hole.size, 
                      hole.shape = hole.shape,
                      pop.dist.df = pop.dist.df)
  
  # calculate complete area size
  poly.size <- sqrt(tile.num) * base.tile.size
  
  # build basis polygon
  area.polygon <- st_polygon(list(rbind(c(0, 0), c(poly.size, 0), c(poly.size, poly.size), c(0, poly.size), c(0, 0)))) %>%
    st_sfc() %>%
    st_sf()
  
  # create bounding box area and corresponding raster object
  area.bbox <- st_bbox(area.polygon)
  area.raster <- create_raster(area.bbox, tile.size = base.tile.size)
  
  # specify elevation raster --> default at 0 currently
  area.elevation <- area.raster
  values(area.elevation) <- 0
  
  # retransform raster to sf for cell creation
  base.tiles <- st_as_sf(st_as_stars(area.raster))
  
  # build city geometry (1 obs.)
  cities <- st_sample(area.polygon, city.num) %>%
    st_buffer(dist = city.size, endCapStyle = city.shape) %>% # parameter if square or circle
    st_geometry() %>%
    st_union() %>%
    st_sf() %>%
    mutate(city = 1)
  
  # build hole geometry (1 obs.)
  holes <- st_sample(cities, hole.num) %>%
    st_buffer(dist = hole.size, endCapStyle = as.character(hole.shape)) %>% # parameter if square or circle
    st_geometry() %>%
    st_union() %>%
    st_sf() %>%
    mutate(hole = 1)
  
  # join tiles with cities and holes
  area.sf.helper <- base.tiles %>%
    st_join(cities) %>%
    st_join(holes) %>%
    mutate(type = case_when(city == 1 & is.na(hole) ~ "Urban",
                            city == 1 & hole == 1 ~ "Hole",
                            TRUE ~ "Rural")) %>%
    mutate(category = case_when(type %in% c("Urban", "Hole") ~ "Urban",
                                type == "Rural" ~ "Rural")) %>% 
    rownames_to_column("tile.id") %>%
    mutate(tile.id = as.integer(tile.id)) %>% 
    arrange(type)
  
  # find out how many tiles are of a certain type to match with respective pop-distribution function
  summary.area <- area.sf.helper %>% 
    st_drop_geometry() %>% 
    dplyr::select(tile.id, type) %>% 
    group_by(type) %>% 
    summarise(n = n()) %>% 
    left_join(pop.dist.df, by = "type") %>% 
    mutate(final = paste0(exprssion, ", n = ", n, ")")) %>% 
    arrange(type)
  
  # sample pop vector from input distribution function
  pop.helper <- unlist(map(summary.area$final, ~eval(parse(text = .x))))
  
  # append pop to the sf data frame, apply necessary rounding and create final sf dataframe
  area.sf <- area.sf.helper %>% 
    mutate(pop = pop.helper) %>% 
    mutate(pop = round(pop, 0)) %>%
    mutate(pop = if_else(pop < 0, 0, pop)) %>%
    mutate(centroid.geometry = st_centroid(.$geometry)) %>% 
    mutate(X.centroid = unlist(map(.$centroid.geometry, 1)),
           Y.centroid = unlist(map(.$centroid.geometry, 2))) %>% 
    dplyr::select(tile.id, type, category, pop, X.centroid, Y.centroid) %>%
    arrange(tile.id)
  
  # create non-sf data frame version
  area.df <- area.sf %>% 
    st_drop_geometry() %>% 
    arrange(tile.id) 
  
  # put everything into a output list
  final <- list(area.params = area.params,
                area.sf = area.sf,
                area.df = area.df,
                area.union = area.polygon,
                area.bbox = area.bbox,
                area.raster = area.raster,
                area.elevation = area.elevation)
  
  return(final)
}



# create tower positions with attached cells
create_cells <- function(area.sf, tower.dist, rotation.deg, jitter, small = FALSE, subscript, seed) {
  
  
  set.seed = seed   
  
  rotation = function(a){
    r = a * pi / 180 #degrees to radians
    matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
  } 
  
  layer_network_generate = function(x, tower.dist, rotation.deg){
    layer.geo <- x %>% 
      st_make_grid(cellsize = tower.dist, 
                   square = F, # hexagon 
                   flat_topped = T) %>%  # different cell size (qm)
      st_geometry()
    
    layer.centroid <- st_centroid(layer.geo)
    layer <- (layer.geo - layer.centroid) * rotation(rotation.deg) + layer.centroid # rotate by 35 degrees
    return(layer)
    
  }
  
  # create layer object, placing towers
  layer <- layer_network_generate(x = area.sf, tower.dist = tower.dist, rotation.deg = rotation.deg)
  
  # specify exact location of towers and labelling
  towers <- layer %>%
    st_centroid() %>% 
    st_jitter(jitter) %>%
    st_coordinates() %>%
    as_tibble() %>%
    dplyr::select(X.tow = X, Y.tow = Y) %>% 
    mutate(tower.id = paste0(subscript, ".", 1:n()))
  
  # create 3 cells per tower and labelling
  cells.unparam <- towers %>% 
    slice(rep(1:n(), each = 3)) %>%
    group_by(tower.id) %>%
    mutate(cell = paste(tower.id, "C", 1:3, sep = ".")) %>%
    ungroup() %>%
    mutate(cell.kind = subscript) %>% 
    mutate(intra.cell.number = str_sub(cell, -1)) %>% 
    mutate(small = small) %>% 
    mutate(rotation.deg = rotation.deg)
  
  return(cells.unparam)
  
}


# area.elevation <- sim.area$area.elevation
# area.sf <- sim.area$area.sf
# area.bbox <- sim.area$area.bbox
# cells.unparam <- MA.cells.unparam

# specify parameters of each cell
create_cellplan <- function(area.sf, area.bbox, area.elevation, cells.unparam, cell.param.mobloc) {
  
  crs.set <- st_crs(area.sf)
  
  # specify parameters if missing (NAs will automatically be replaced with mobloc default value)
  if (missing(cell.param.mobloc)) {
    specified.param <- mobloc_param()
  } else {
    specified.param <- cell.param.mobloc
  }
  
  # prepare cells for validation
  cellplan.unval <- cells.unparam %>% 
    mutate(direction.int = case_when(small == FALSE & intra.cell.number == "1" ~ rotation.deg + 0,
                                     small == FALSE & intra.cell.number == "2" ~ rotation.deg + 120,
                                     small == FALSE & intra.cell.number == "3" ~ rotation.deg + 240,
                                     TRUE ~ NA_real_)) %>% 
    mutate(direction = case_when(is.na(direction.int) ~ NA_real_,
                                 direction.int > 360 ~ 360 - direction.int, # direction needs to be [0; 360]
                                 direction.int <= 360 ~ direction.int),
           # tilt, etc will be changed if parameters are set in cell.param
           tilt = NA,
           beam_h = NA,
           beam_v = NA) %>%
    st_as_sf(coords = c("X.tow", "Y.tow"), crs = crs.set) %>%
    st_crop(area.bbox) %>%
    st_set_agr("aggregate") %>% 
    st_intersection(area.sf) %>% 
    dplyr::select(cell, small, direction, tilt, beam_h, beam_v)
  
  
  # validate cellplan and add antenna position indication through offset
  cellplan.val <- validate_cellplan(cp = cellplan.unval, 
                                    param = specified.param,
                                    elevation = area.elevation
                                    # region = region,
                                    # envir = envir
  ) 
  # mutate(move_cells_into_prop_direction(., offset = 100)) %>% 
  # mutate(x.offset = st_coordinates(.)[, 1],
  #        y.offset = st_coordinates(.)[, 2]) %>% 
  # st_drop_geometry()
  
  # put everything into a list
  final <- list(cellplan.val = cellplan.val,
                cell.param.mobloc = specified.param)
  
  return(final)
  
}


## combine all cell creation functions

complete_cellplan_gen <- function(area, layer.params.ext, param.df) {
  
  param.df.reduced <- param.df %>% 
    dplyr::select(cell.kind, dominance.th)
  
  cells.unparam.list <- map(layer.params.ext, ~create_cells(area.sf = area$area.sf,
                                                            tower.dist = .x$tower.dist,
                                                            rotation.deg = .x$rotation.deg,
                                                            jitter = .x$jitter,
                                                            small = FALSE,
                                                            subscript = .x$subscript,
                                                            seed = .x$seed))
  
  cellplan.val.list <- map2(cells.unparam.list, layer.params.ext,
                            ~create_cellplan(area.sf = area$area.sf,
                                             area.bbox = area$area.bbox, 
                                             area.elevation = area$area.elevation,
                                             cells.unparam = .x,
                                             cell.param.mobloc = .y$mobloc.params))
  
  cellplan.combined.df <- map_dfr(cellplan.val.list, 
                                  ~bind_rows(as_tibble(.x$cellplan.val)),
                                  .id = "cell.kind") %>% 
    mutate(cell.chr = as.character(cell)) %>% 
    mutate(cell.fac = factor(cell.chr)) %>% 
    mutate(cell.num = as.numeric(cell.fac)) %>% 
    left_join(param.df.reduced, by = "cell.kind")
  
  cellplan.combined.reduced.df <- cellplan.combined.df %>% 
    dplyr::select(cell, dominance.th)
  
  # compute signal strength and device to cell association
  signal.strength.list <- map(cellplan.val.list, 
                              ~compute_sig_strength(cp = .x$cellplan.val,
                                                    raster = area$area.raster,
                                                    param = .x$cell.param.mobloc,
                                                    elevation = area$area.elevation))
  
  # create signal strength object of all cells
  signal.strength.comb.dt <- rbindlist(signal.strength.list) %>% 
    mutate(tile.id.chr = as.character(rid)) %>% 
    mutate(tile.id.fac = factor(tile.id.chr, levels = fct_unique(area$area.sf$tile.id.fac))) %>% 
    mutate(tile.id.num = as.numeric(tile.id.fac)) %>% 
    mutate(cell.chr = as.character(cell)) %>% 
    mutate(cell.fac = factor(cell.chr, levels = fct_unique(cellplan.combined.df$cell.fac))) %>% 
    mutate(cell.num = as.numeric(cell.fac)) %>% 
    left_join(area$area.reduced.df, by = "tile.id.chr") %>% 
    as.data.table(.)
  
  signal.strength.summary.helper <- signal.strength.comb.dt %>%
    as_tibble() %>%
    mutate(cell.kind = substr(cell, 1, 2)) %>%
    left_join(cellplan.combined.reduced.df, by = c("cell.chr" = "cell")) %>% 
    filter(!s < dominance.th) # filter rows out that are below the set dominance threshold
  
  signal.strength.summary <- signal.strength.summary.helper %>% 
    group_by(tile.id.chr) %>%
    mutate(max.dBm = max(dBm),
           max.s = max(s),
           min.dist = min(dist),
           count = n()) %>%
    ungroup()
  
  # identify the cell-tile relations with maximum signal dominance and identify tiles that are not covered sufficiently
  signal.dom <- signal.strength.summary %>% 
    distinct(tile.id.chr, max.s) %>%
    left_join(signal.strength.summary, by = c("tile.id.chr", "max.s" = "s")) %>% 
    dplyr::select(tile.id.chr, max.s, cell, cell.kind) %>% 
    full_join(area$area.sf, by = "tile.id.chr") %>% 
    mutate(missing = case_when(is.na(max.s) ~ 1,
                               TRUE ~ 0))
  
  
  
  return(list(param.df = param.df,
              cellplan.combined.df = cellplan.combined.df,
              cellplan.combined.reduced.df = cellplan.combined.reduced.df,
              signal.strength.comb.dt = signal.strength.comb.dt,
              signal.strength.summary.helper = signal.strength.summary.helper,
              signal.strength.summary = signal.strength.summary,
              signal.dom = signal.dom))
  
}

cell.spat.diag <- function(area, cellplan, label.name) {
  
  signal.strength.summary.ck <- cellplan$signal.strength.summary.helper %>% 
    group_by(tile.id.chr, cell.kind) %>%
    summarise(cell.count = n(),
              max.dBm = max(dBm),
              max.s = max(s),
              min.dist = min(dist)) %>% 
    mutate(cell.count.complete = sum(cell.count),
           max.dBm.complete = max(max.dBm),
           max.s.complete = max(max.s), 
           min.dist.complete = max(min.dist)) %>% 
    ungroup() %>% 
    pivot_longer(cols = -c(tile.id.chr, cell.kind),
                 names_to = "kind", 
                 values_to = "values") 
  
  # histogram cells
  cat.labs <- c("Complete", "Macro", "Meso", "Micro")
  names(cat.labs) <- c("complete", "MA", "ME", "MI")
  
  tile.coverage.hist <- signal.strength.summary.ck %>%
    filter(str_detect(kind, pattern = "count")) %>% 
    mutate(cat = case_when(str_detect(kind, pattern = "complete") ~ "complete",
                           TRUE ~ paste0(cell.kind))) %>% 
    distinct(tile.id.chr, cat, .keep_all = T) %>% 
    dplyr::select(-cell.kind) %>% 
    ggplot() +
    geom_histogram(aes(values), binwidth = 1) +
    theme(text = element_text(size = 13)) +
    # scale_x_continuous(breaks = seq(0, 12, 1)) + # respecify to be dynamic
    facet_grid(~cat, labeller = labeller(cat = cat.labs)) +
    labs(title = paste0(label.name, ": Overlap histogram"),
         y = "Tile count",
         x = "Number of cells")
  
  
  coverage.map.dom.df.complete <- signal.strength.summary.ck %>%
    filter(str_detect(kind, pattern = "max.s")) %>% 
    mutate(cat = case_when(str_detect(kind, pattern = "complete") ~ "complete",
                           TRUE ~ paste0(cell.kind))) %>% 
    filter(cat == "complete") %>% 
    distinct(tile.id.chr, cat, .keep_all = T) %>% 
    dplyr::select(-cell.kind) %>% 
    full_join(area$area.sf, by = c("tile.id.chr")) %>%  
    mutate(missing = case_when(is.na(values) ~ 1,
                               TRUE ~ 0)) %>% 
    st_as_sf()
  
  
  coverage.map.complete <- coverage.map.dom.df.complete %>% 
    ggplot() +
    geom_sf(aes(fill = values), color = "transparent") +
    geom_point(data = cellplan$cellplan.combined.df, aes(x, y, color = cell.kind, shape = cell.kind), stroke = 1.2) +
    scale_color_manual(values = c("MA" = "red", "ME" = "#FF00FF", "MI" = "blue")) +
    scale_shape_manual(values = c(0, 2, 1), guide = NULL) +
    scale_fill_gradient(low = "white", high = "black", na.value = "red", 
                        limits = c(0, 1)) +
    labs(title = paste0(label.name, ": Coverage density"),
         color = "Cell Layer",
         fill = "Signal Dominance",
         x = "",
         y = "") +
    theme_minimal() +
    theme(text = element_text(size = 13))
  
  
  return(list(tile.coverage.hist = tile.coverage.hist,
              coverage.map.complete = coverage.map.complete))
  
}


smart_round <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x - y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}


# implement selected iterations option with input vector

# New MLE iteration function by Matyas
EM_est <- function(c.vec.dt, P.dt, a.vec.dt, n.iter, selected.range, ldt.dt, message = T) {
  
  cdt <- c.vec.dt
  pij <- cdt[P.dt, on = "i"]
  pij <- pij[c > 0] # remove those lines where c==0 because it will create 0 division
  # adt <- data.table(a = a.vec)
  # tiles <- adt[, .(j = 1:.N, u = a)]
  tiles <- a.vec.dt
  keep <- a.vec.dt # base dataframe for the selected iterations
  
  for(m in 1:(n.iter)){
    
    if(message == T) {
      cat(format(Sys.time()), paste0("---- calculating u", m), "----\n")
    }
    
    cols <- c("j", paste0("u"))
    ju <- tiles[, cols, with = F]
    setnames(ju, c("j", "u"))
    pij <- ju[pij, on = "j"]
    denom <- pij[, .(sum_pik_uk = sum(u * pij)), by = i]
    pij <- denom[pij, on = "i"]
    faktor <- pij[, .(f = sum(c * pij / sum_pik_uk)), by = j]
    faktor.adj <- faktor[, f := fifelse(test = {is.na(f) | is.nan(f) | is.infinite(f)}, 1, f)] # if else to assure that the posterior is 1 to secure the same estimand value after ldt
    pij[, c("u", "sum_pik_uk") := NULL]
    tiles <- faktor.adj[tiles, on = "j"]
    # tiles <- eval(parse(text = paste0("tiles[, u := u * f]")))
    tiles <- eval(parse(text = paste0("tiles[,  u := fifelse(u * f < ldt.dt, 0, u * f)]")))
    tiles[, "f" := NULL]
    
    if(m %in% selected.range) {
      keep <- tiles[keep, on = "j"]
      keep <- eval(parse(text = paste0("keep[, u_", m, ":= u]")))
      keep[, "u" := NULL]
    }
    
  }
  
  # final <- list(tiles = tiles,
  #               keep = keep)
  
  return(keep)
  
}



# # New MLE iteration function by Matyas
# EM_est <- function(c.vec, P.dt, a.vec, n.iter, ldt = 10^-04) {
# 
# 
#   cdt <- data.table(c = c.vec)
#   cdt <- cdt[, .(i = 1:.N, c = c)]
#   pag <- cdt[P.dt, on = "i"]
#   pag <- pag[c > 0] # remove those lines where c==0 because it will create 0 division
#   adt <- data.table(a = a.vec)
#   tiles <- adt[, .(j = 1:.N, u0 = a)]
# 
#   for(m in 0:(n.iter - 1)){
#     cat(format(Sys.time()), paste0("---- calculating u", m + 1), "----\n")
#     cols <- c("j", paste0("u", m))
#     ju <- tiles[, cols, with = F]
#     setnames(ju, c("j", "u"))
#     pag <- ju[pag, on = "j"]
#     denom <- pag[, .(sum_pik_uk = sum(u * pag)), by = i]
#     pag <- denom[pag, on = "i"]
#     faktor <- pag[, .(f = sum(c * pag / sum_pik_uk)), by = j]
#     faktor.adj <- faktor[, f := fifelse(test = {is.na(f) | is.nan(f) | is.infinite(f)}, 1, f)] # if else to assure that the posterior is 1 to secure the same estimand value after ldt
#     pag[, c("u", "sum_pik_uk") := NULL]
#     tiles <- faktor.adj[tiles, on = "j"]
#     tiles <- eval(parse(text = paste0("tiles[, u", m + 1, " := u", m, "* f]")))
#     tiles <- eval(parse(text = paste0("tiles[,  u", m + 1, " := fifelse(u", m + 1, " < ldt, 0, u", m + 1, ")]")))
#     tiles[, "f" := NULL]
#   }
# 
#   return(tiles)
# 
# }



DF_est_relaxed <- function(c.vec.dt, P.star.spm, a.supertile.vec){
  
  c.vec <- c(c.vec.dt)$c
  A.spm <- .sparseDiagonal(n = length(a.supertile.vec), x = a.supertile.vec)
  
  Y <- P.star.spm %*% A.spm %*% t(P.star.spm) 
  Y1 <- VCA::MPinv(Y) %*% (c.vec - P.star.spm %*% a.supertile.vec)
  u <- as.vector(A.spm %*% t(P.star.spm) %*% Y1 + a.supertile.vec)
  
  return(u)
}


DF_est_num <- function(c.vec.dt, P.star.spm, a.supertile.vec){
  
  settings <- osqpSettings(alpha = 1.0, eps_abs = 1e-12, eps_rel = 1e-12, 
                           max_iter = 100000, linsys_solver = 0, warm_start = 1)
  
  model <- osqp(P = Q, q = a, A = P.mat.1, l = l, u = u, settings)
  res <- model$Solve()
  
  return(u)
}


DF_est_iterated <- function(c.vec.dt, P.star.spm, a.supertile.vec, P.dt, DF.threshold = 1, selected.range.DF, n.iter.DF){
  
  c.vec <- c(c.vec.dt)$c
  
  DF.tiles <- data.table(j = as.numeric(names(a.supertile.vec)),
                         u = a.supertile.vec)
  keep <- data.table(j = as.numeric(names(a.supertile.vec)),
                     prior = a.supertile.vec)
  
  for(m.DF in 1:(n.iter.DF)){
    
    m.DF = 1
    
    
    # time/iteration indication
    cat(format(Sys.time()), paste0("---- calculating u", m.DF), "----\n")
    
    # specify prior (A)
    U.spm <- .sparseDiagonal(n = length(a.supertile.vec), x = DF.tiles$u)
    
    # calculate DF
    Y <- P.star.spm %*% U.spm %*% t(P.star.spm)
    Y1 <- VCA::MPinv(Y) %*% (c.vec - P.star.spm %*% a.supertile.vec)
    u.unadj <- as.vector(U.spm %*% t(P.star.spm) %*% Y1 + a.supertile.vec)
    
    # save as datatable
    DF.tiles <- data.table(j = as.numeric(names(a.supertile.vec)),
                           u = u.unadj)
    
    # Transforming based on dynamic DF.threshold
    # DF.tiles <- eval(parse(text = paste0("DF.tiles[, u := fifelse(u < (DF.threshold / m.DF), DF.threshold / m.DF, u)]")))
    DF.tiles[, u := fifelse(u < (DF.threshold / m.DF), DF.threshold / m.DF, u)]
    
    # Renormalizing with 1 EM iteration
    u.dt <- EM_est(c.vec.dt = c.vec.dt,
                   P.dt = P.star.oracle.supertile.dt,
                   a.vec.dt = DF.tiles,
                   selected.range = c(1,2, 3),
                   n.iter = 3)
    setnames(u.dt, c("j", "u.prior", "u"))
    u.dt.final <- u.dt[, c("j", "u")]
    DF.tiles <- DF.tiles[, "j"]
    DF.tiles <- u.dt.final[DF.tiles, on = "j"]
    
    # define datatable object "keep" which contains only the iterations that were indicated in DF.selected.range
    if(m.DF %in% selected.range.DF) {
      keep <- DF.tiles[keep, on = "j"]
      keep <- eval(parse(text = paste0("keep[, u", m.DF, ":= u]")))
      keep[, "u" := NULL]
    }
    
  }
  
  return(keep)
}



# paramter: sim.area, cellplan.combined, signal.strength.comb.dt, c.vec.df, method, offset


# # Voronoi
# # aggregating the antennas to towers (and corresponding values) and identifying problematic tower locations (outside of focus area)
# 
# ### 1 ####
# 
# 
# VOR_est <- function(area, cellplan.combined, signal.strength.comb.dt, C.vec.df, seed = c("tower", "cell.offset", "cell.hotpoint"), offset = 20) {
#   
#   ### helper 
#   
#   base.tile.size <- area$area.params$base.tile.size
#   crs.set <- st_crs(area$area.sf)
#   cellplan.reduced <- cellplan.combined %>% 
#     dplyr::select(cell, direction)
#   
#   if (seed == "tower") {
#     
#     ### tower ###
#     seed.object.unadj <- C.vec.df %>% 
#       left_join(cellplan.combined, by = "cell") %>% 
#       mutate(tower = str_extract(cell, "[A-Z]+.[:digit:]+")) %>% 
#       group_by(tower) %>% 
#       mutate(phones.sum = sum(phones.sum)) %>% 
#       distinct(tower, .keep_all = T) %>% 
#       ungroup() %>% 
#       dplyr::select(seed = tower, X.tow = x, Y.tow = y, phones.sum) %>%
#       st_as_sf(coords = c("X.tow", "Y.tow")) %>% 
#       st_sf(crs = crs.set) %>%  # optional
#       mutate(within.fa = lengths(st_within(., area$area.union))) # find seeds outside the focus area
#     
#   } else if (seed == "cell.offset") {
#     
#     #### offset ####
#     cellplan.offset <- move_cells_into_prop_direction(st_as_sf(cellplan.combined), offset = offset)
#     
#     seed.object.unadj <- C.vec.df %>% 
#       left_join(cellplan.offset, by = "cell") %>% 
#       dplyr::select(seed = cell, phones.sum, geometry) %>%
#       st_sf() %>% 
#       st_sf(crs = crs.set) %>%
#       mutate(within.fa = lengths(st_within(., area$area.union))) # find seeds outside the focus area
#     
#   } else if (seed == "cell.hotpoint") {
#     
#     ### Hotpoint ###
#     cell.max.location <- signal.strength.comb.dt %>% 
#       as_tibble() %>% 
#       rename(tile.id = rid) %>% 
#       group_by(cell) %>% 
#       mutate(max.cell.s = max(dBm)) %>% # find the max SIGNAL STRENGTH per cell
#       filter(dBm == max.cell.s) %>%  # filter the rows where corresponds to the cell specific max.s to retain the tile.id
#       ungroup() %>% 
#       group_by(tile.id) %>% 
#       mutate(count.same.centroids = n()) %>% # tiles that act as hotpoint for multiple cells
#       ungroup() %>% 
#       mutate(same.centroids = case_when(count.same.centroids > 1 ~ 1, # mark identical hotpoints for further adjustment into respective direction
#                                         TRUE ~ 0)) %>% 
#       left_join(area$area.sf, by = "tile.id") %>% 
#       left_join(cellplan.reduced, by = "cell") %>% # join to receive direction info
#       dplyr::select(cell, same.centroids, direction, geometry) %>% 
#       st_sf(crs = crs.set) %>% 
#       st_centroid() %>% 
#       mutate(X = unlist(map(.$geometry, 1)),
#              Y = unlist(map(.$geometry, 2))) %>% 
#       mutate(X.adj = case_when(same.centroids == "1" ~ X + SIN(direction) * 10, # move point in cell defined direction with offset of 10m if identical hotpoints
#                                TRUE ~ X),
#              Y.adj = case_when(same.centroids == "1" ~ Y + COS(direction) * 10,
#                                TRUE ~ Y)) %>% 
#       st_drop_geometry() %>% 
#       st_as_sf(coords = c("X.adj", "Y.adj"), crs = crs.set) %>% 
#       dplyr::select(-X, -Y) # add again for consistency check plot if the adjutsment was right
#     
#     # cell.max.location %>% 
#     #   ggplot() +
#     #   geom_sf() +
#     #   geom_point(aes(x = X, y = Y, color = factor(same.centroids)))
#     
#     
#     seed.object.unadj <- C.vec.df %>% 
#       left_join(cell.max.location, by = "cell") %>% 
#       dplyr::select(seed = cell, phones.sum, geometry) %>%
#       mutate(seed = as.character(seed)) %>% 
#       st_sf(crs = crs.set) %>% 
#       mutate(within.fa = lengths(st_within(., area$area.union))) # find seeds outside the focus area
#     
#   } else if (seed == "cell.barycenter") {
#     
#     ### barycenter ###
#     
#     # calculate the centroid of all tiles and save as point coordinates
#     tile.centroid <- area$area.sf %>% 
#       st_centroid() %>% 
#       mutate(centroid.X = unlist(map(.$geometry, 1)),
#              centroid.Y = unlist(map(.$geometry, 2))) %>% 
#       dplyr::select(tile.id, centroid.X, centroid.Y) %>% 
#       st_drop_geometry()
#     
#     # join signal strength with tile centroid. calculate weighted 2d mean of x and y coordinate with mean(sig.dom.) as weights
#     cell.max.location <- signal.strength.comb.dt %>% 
#       as_tibble() %>% 
#       rename(tile.id = rid) %>% 
#       left_join(tile.centroid, by = "tile.id") %>% 
#       group_by(cell) %>% 
#       summarise(X.barycenter = weighted.mean(centroid.X, s),
#                 Y.barycenter = weighted.mean(centroid.Y, s)) %>% # calculate for each cell the barycenter location
#       ungroup() %>% 
#       st_as_sf(coords = c("X.barycenter", "Y.barycenter"), crs = crs.set)
#     
#     
#     seed.object.unadj <- C.vec.df %>% 
#       left_join(cell.max.location, by = "cell") %>% 
#       dplyr::select(seed = cell, phones.sum, geometry) %>%
#       mutate(seed = as.character(seed)) %>% 
#       st_sf(crs = crs.set) %>% 
#       mutate(within.fa = lengths(st_within(., area$area.union))) # find seeds outside the focus area
#     
#   }
#   
#   
#   # saving seed IDs of problematic seeds (2)
#   
#   ### 2 ####
#   
#   seed.outside <- seed.object.unadj %>% 
#     filter(within.fa == 0) %>%
#     st_drop_geometry() %>% 
#     dplyr::select(seed) %>% 
#     deframe()
#   
#   # filtering unproblematic seeds
#   seed.inside <- seed.object.unadj %>% 
#     filter(within.fa == 1)
#   
#   if (length(seed.outside) >= 1) {
#     
#     seed.object.adj <- seed.object.unadj %>% 
#       filter(within.fa == 0) %>%
#       st_nearest_points(., area$area.union) %>%
#       st_cast("POINT") %>%
#       .[seq(2, length(.), 2)] %>% #
#       st_as_sf() %>%
#       mutate(seed = names(seed.outside)) %>%
#       mutate(geometry = x) %>%
#       st_sf(sf_column_name = "geometry") %>%
#       dplyr::select(-x) %>%
#       bind_rows(seed.inside)
#     
#   } else {
#     
#     seed.object.adj <- seed.object.unadj
#   }
#   
#   
#   # Finding respective nearest point on focus area border for every problematic seed location
#   
#   
#   sum(unlist(st_intersects(seed.object.adj, area$area.union))) ==
#     length(seed.object.unadj$seed) # check if all are within now
#   
#   # Using the seed object to calculate the Voronoi regions and their spatial densities per Voronoi region
#   
#   ### 3 ####
#   seed.voronoi.est <- seed.object.adj %>%  
#     st_geometry() %>% 
#     st_union() %>% 
#     st_voronoi() %>% 
#     st_collection_extract(type = "POLYGON") %>%
#     st_sf() %>% # check if crs is listed
#     st_join(seed.object.unadj) %>%  # rejoin with seed object to retain seed id
#     st_intersection(area$area.union) %>% 
#     mutate(vor.area = row_number()) %>% 
#     mutate(vor.area.size = as.numeric(st_area(.$geometry))) %>% 
#     mutate(vor.est = phones.sum / vor.area.size)
#   
#   Voronoi.regions.plot <- seed.voronoi.est %>%
#     ggplot() +
#     # geom_sf(aes(fill = phones.sum), color = "blue") +
#     geom_sf(color = "blue") +
#     theme(text = element_text(size = 13))
#   # scale_fill_viridis_c("Phones") +
#   
#   # Joining the regions with the tile specific data
#   seed.voronoi.tile <- seed.voronoi.est %>% 
#     st_join(area$area.sf) %>% # & re-connect the data items
#     st_set_agr("aggregate") %>% # clean up
#     group_by(tile.id) %>% 
#     mutate(count = n()) %>% 
#     ungroup()
#   
#   # identifiying tiles intersecting with multiple Voronoi regions
#   seed.multiple <- seed.voronoi.tile %>%
#     st_drop_geometry() %>% 
#     filter(count > 1) %>% 
#     distinct(tile.id) %>% 
#     deframe()
#   
#   # calculate area within competing voronoi regions of "multiple" tiles
#   seed.intersect.tiles <- area$area.sf %>% 
#     filter(tile.id %in% seed.multiple) %>%
#     st_intersection(seed.voronoi.est) %>% 
#     # st_collection_extract(type = "POLYGON") %>% # select the polygons
#     mutate(amount.tiles = as.numeric(st_area(.$geometry)) / base.tile.size^2) # checked if it adds up to 1
#   
#   # final datatset to calculate spatial density
#   seed.voronoi.final <- seed.intersect.tiles %>% 
#     st_drop_geometry() %>% 
#     dplyr::select(tile.id, seed, amount.tiles) %>% 
#     right_join(seed.voronoi.tile, by = c("tile.id", "seed")) %>% 
#     mutate(amount.tiles = case_when(is.na(amount.tiles) ~ 1,
#                                     TRUE ~ amount.tiles)) %>% 
#     group_by(tile.id) %>% 
#     summarise(u.VOR = weighted.mean(x = vor.est, w = amount.tiles) * base.tile.size^2)
#   # should result in same length as the raw tiles object and the sum of the voronoi est corrected should resemble the sum of the c.vec
#   
#   
#   return(list(seed.voronoi.final = seed.voronoi.final,
#               Voronoi.regions.plot = Voronoi.regions.plot))
#   
# }


### new
VOR_est <- function(area, cellplan.combined, signal.strength.comb.dt, C.vec.df, prior.var, seed = c("tower", "cell.offset", "cell.hotpoint"), offset = 20) {
  
  ### helper 
  
  base.tile.size <- area$area.params$base.tile.size
  crs.set <- st_crs(area$area.sf)
  cellplan.reduced <- cellplan.combined %>% 
    dplyr::select(cell, direction)
  
  if (seed == "tower") {
    
    ### tower ###
    seed.object.unadj <- C.vec.df %>% 
      left_join(cellplan.combined, by = "cell") %>% 
      mutate(tower = str_extract(cell, "[A-Z]+.[:digit:]+")) %>% 
      group_by(tower) %>% 
      mutate(phones.sum = sum(phones.sum)) %>% 
      distinct(tower, .keep_all = T) %>% 
      ungroup() %>% 
      dplyr::select(seed = tower, X.tow = x, Y.tow = y, phones.sum) %>%
      st_as_sf(coords = c("X.tow", "Y.tow")) %>% 
      st_sf(crs = crs.set) %>%  # optional
      mutate(within.fa = lengths(st_within(., area$area.union))) # find seeds outside the focus area
    
  } else if (seed == "cell.offset") {
    
    #### offset ####
    cellplan.offset <- move_cells_into_prop_direction(st_as_sf(cellplan.combined), offset = offset)
    
    seed.object.unadj <- C.vec.df %>% 
      left_join(cellplan.offset, by = "cell") %>% 
      dplyr::select(seed = cell, phones.sum, geometry) %>%
      st_sf() %>% 
      st_sf(crs = crs.set) %>%
      mutate(within.fa = lengths(st_within(., area$area.union))) # find seeds outside the focus area
    
  } else if (seed == "cell.hotpoint") {
    
    ### Hotpoint ###
    cell.max.location <- signal.strength.comb.dt %>% 
      as_tibble() %>% 
      rename(tile.id = rid) %>% 
      group_by(cell) %>% 
      mutate(max.cell.s = max(dBm)) %>% # find the max SIGNAL STRENGTH per cell
      filter(dBm == max.cell.s) %>%  # filter the rows where corresponds to the cell specific max.s to retain the tile.id
      ungroup() %>% 
      group_by(tile.id) %>% 
      mutate(count.same.centroids = n()) %>% # tiles that act as hotpoint for multiple cells
      ungroup() %>% 
      mutate(same.centroids = case_when(count.same.centroids > 1 ~ 1, # mark identical hotpoints for further adjustment into respective direction
                                        TRUE ~ 0)) %>% 
      left_join(area$area.sf, by = "tile.id") %>% 
      left_join(cellplan.reduced, by = "cell") %>% # join to receive direction info
      dplyr::select(cell, same.centroids, direction, geometry) %>% 
      st_sf(crs = crs.set) %>% 
      st_centroid() %>% 
      mutate(X = unlist(map(.$geometry, 1)),
             Y = unlist(map(.$geometry, 2))) %>% 
      mutate(X.adj = case_when(same.centroids == "1" ~ X + SIN(direction) * 10, # move point in cell defined direction with offset of 10m if identical hotpoints
                               TRUE ~ X),
             Y.adj = case_when(same.centroids == "1" ~ Y + COS(direction) * 10,
                               TRUE ~ Y)) %>% 
      st_drop_geometry() %>% 
      st_as_sf(coords = c("X.adj", "Y.adj"), crs = crs.set) %>% 
      dplyr::select(-X, -Y) # add again for consistency check plot if the adjutsment was right
    
    # cell.max.location %>% 
    #   ggplot() +
    #   geom_sf() +
    #   geom_point(aes(x = X, y = Y, color = factor(same.centroids)))
    
    
    seed.object.unadj <- C.vec.df %>% 
      left_join(cell.max.location, by = "cell") %>% 
      dplyr::select(seed = cell, phones.sum, geometry) %>%
      mutate(seed = as.character(seed)) %>% 
      st_sf(crs = crs.set) %>% 
      mutate(within.fa = lengths(st_within(., area$area.union))) # find seeds outside the focus area
    
  } else if (seed == "cell.barycenter") {
    
    ### barycenter ###
    
    # calculate the centroid of all tiles and save as point coordinates
    tile.centroid <- area$area.sf %>% 
      st_centroid() %>% 
      mutate(centroid.X = unlist(map(.$geometry, 1)),
             centroid.Y = unlist(map(.$geometry, 2))) %>% 
      dplyr::select(tile.id, centroid.X, centroid.Y) %>% 
      st_drop_geometry()
    
    # join signal strength with tile centroid. calculate weighted 2d mean of x and y coordinate with mean(sig.dom.) as weights
    cell.max.location <- signal.strength.comb.dt %>% 
      as_tibble() %>% 
      rename(tile.id = rid) %>% 
      left_join(tile.centroid, by = "tile.id") %>% 
      group_by(cell) %>% 
      summarise(X.barycenter = weighted.mean(centroid.X, s),
                Y.barycenter = weighted.mean(centroid.Y, s)) %>% # calculate for each cell the barycenter location
      ungroup() %>% 
      st_as_sf(coords = c("X.barycenter", "Y.barycenter"), crs = crs.set)
    
    
    seed.object.unadj <- C.vec.df %>% 
      left_join(cell.max.location, by = "cell") %>% 
      dplyr::select(seed = cell, phones.sum, geometry) %>%
      mutate(seed = as.character(seed)) %>% 
      st_sf(crs = crs.set) %>% 
      mutate(within.fa = lengths(st_within(., area$area.union))) # find seeds outside the focus area
    
  }
  
  
  # saving seed IDs of problematic seeds (2)
  
  ### 2 ####
  
  seed.outside <- seed.object.unadj %>% 
    filter(within.fa == 0) %>%
    st_drop_geometry() %>% 
    dplyr::select(seed) %>% 
    deframe()
  
  # filtering unproblematic seeds
  seed.inside <- seed.object.unadj %>% 
    filter(within.fa == 1)
  
  if (length(seed.outside) >= 1) {
    
    seed.object.adj <- seed.object.unadj %>% 
      filter(within.fa == 0) %>%
      st_nearest_points(., area$area.union) %>%
      st_cast("POINT") %>%
      .[seq(2, length(.), 2)] %>% #
      st_as_sf() %>%
      mutate(seed = names(seed.outside)) %>%
      mutate(geometry = x) %>%
      st_sf(sf_column_name = "geometry") %>%
      dplyr::select(-x) %>%
      bind_rows(seed.inside)
    
  } else {
    
    seed.object.adj <- seed.object.unadj
  }
  
  
  # Finding respective nearest point on focus area border for every problematic seed location
  
  
  sum(unlist(st_intersects(seed.object.adj, area$area.union))) ==
    length(seed.object.unadj$seed) # check if all are within now
  
  # Using the seed object to calculate the Voronoi regions and their spatial densities per Voronoi region
  
  ### 3 ####
  
  prior.multi.sf <- area$area.sf.raw %>% 
    group_by(!!as.name(prior.var)) %>% 
    summarise()
  
  seed.voronoi.est <- seed.object.adj %>%  
    st_geometry() %>% 
    st_union() %>% 
    st_voronoi() %>% 
    st_collection_extract(type = "POLYGON") %>%
    st_sf() %>% # check if crs is listed
    st_join(seed.object.unadj) %>%  # rejoin with seed object to retain seed id
    st_intersection(prior.multi.sf) %>%
    mutate(subpolygon.area.size = as.numeric(st_area(.$geometry))) %>% 
    mutate(subpolygon.area.size.prior = subpolygon.area.size * !!as.name(prior.var)) %>% 
    group_by(seed) %>% 
    mutate(den = sum(subpolygon.area.size.prior)) %>% 
    ungroup() %>% 
    mutate(c.subpolygon.per.unit = (!!as.name(prior.var) * phones.sum) / den) %>% 
    mutate(c.subpolygon = c.subpolygon.per.unit * subpolygon.area.size) %>% 
    mutate(subpolygon.name = paste(seed, !!as.name(prior.var), sep = "_"))
  
  Voronoi.regions.plot <- seed.voronoi.est %>%
    # filter(seed == "ME.100.C.2") %>% 
    ggplot() +
    # geom_sf(aes(fill = factor(c.subpolygon.per.unit))) +
    geom_sf(color = "blue") +
    theme(text = element_text(size = 13))
  # scale_fill_viridis_c("Phones") +
  
  # Joining the regions with the tile specific data
  seed.voronoi.tile <- seed.voronoi.est %>% 
    st_join(area$area.sf) %>% # & re-connect the data items
    st_set_agr("aggregate") %>% # clean up
    group_by(tile.id) %>% 
    mutate(count = n()) %>% 
    ungroup()
  
  # identifiying tiles intersecting with multiple Voronoi regions
  seed.multiple <- seed.voronoi.tile %>%
    st_drop_geometry() %>% 
    filter(count > 1) %>% 
    distinct(tile.id) %>% 
    deframe()
  
  # calculate area within competing voronoi regions of "multiple" tiles
  seed.intersect.tiles <- area$area.sf %>% 
    filter(tile.id %in% seed.multiple) %>%
    st_intersection(seed.voronoi.est) %>% 
    # st_collection_extract(type = "POLYGON") %>% # select the polygons
    mutate(amount.tiles = as.numeric(st_area(.$geometry)) / base.tile.size^2) # checked if it adds up to 1
  
  # final datatset to calculate spatial density
  seed.voronoi.final <- seed.intersect.tiles %>% 
    st_drop_geometry() %>% 
    dplyr::select(tile.id, subpolygon.name, amount.tiles) %>% 
    right_join(seed.voronoi.tile, by = c("tile.id", "subpolygon.name")) %>% 
    mutate(amount.tiles = case_when(is.na(amount.tiles) ~ 1,
                                    TRUE ~ amount.tiles)) %>% 
    group_by(tile.id) %>% 
    summarise(u.VOR = weighted.mean(x = c.subpolygon.per.unit, w = amount.tiles) * base.tile.size^2)
  # should result in same length as the raw tiles object and the sum of the voronoi est corrected should resemble the sum of the c.vec
  
  
  return(list(seed.voronoi.final = seed.voronoi.final,
              Voronoi.regions.plot = Voronoi.regions.plot))
  
}




###################
### Plots #########
###################

map_density <- function(data, var, label, pointsize = 1.9, pixels = c(900, 900)) {
  
  colors <- c("white", "light grey", "light blue", "blue", "light green", "yellow", "orange", "red", "#654321")
  var.label <- paste(label)
  
  plot <- data %>% 
    ggplot(aes(x = X.centroid, y = Y.centroid)) +
    # geom_sf(aes_string(fill = var), color = "transparent") +
    geom_scattermore(aes_string(color = var), pointsize = pointsize, pixels = pixels) +
    # scico::scale_fill_scico(palette = "bilbao", limits = c(0, 3.48), direction = 1) +
    scale_color_manual(values = colors, drop = F, name = label) +
    coord_sf() +
    theme_bw() +
    theme(text = element_text(size=13),
          legend.position = "right",
          # axis.text.x = element_blank(),
          # axis.text.y = element_blank(),
          # axis.ticks.x = element_blank(),
          # axis.ticks.y = element_blank(),
          plot.background=element_rect(fill="transparent", colour=NA),
          rect = element_rect(fill = "transparent"),
          plot.margin=grid::unit(c(0,0,0,0), "mm")) +
    labs(x = "", y = "") +
    guides(colour = guide_legend(override.aes = list(shape = 15, size = 5)))
  # theme(axis.title.x = element_blank(),
  #       axis.text.x = element_blank(),
  #       axis.ticks.x = element_blank(),
  #       axis.title.y = element_blank(),
  #       axis.text.y = element_blank(),
  #       axis.ticks.y = element_blank()) +
  
  
  return(plot)
}




r2d <- function(x) x * 180 / pi
d2r <- function(x) x / 180 * pi

COS <- function(x) cos(d2r(x))
SIN <- function(x) sin(d2r(x))
TAN <- function(x) tan(d2r(x))

ACOS <- function(x) r2d(acos(x))
ASIN <- function(x) r2d(asin(x))
ATAN <- function(x) r2d(atan(x))
ATAN2 <- function(y, x) r2d(atan2(y, x))



db2s <- function(dBm, midpoint, steepness) {
  scale <- (dBm - midpoint) * steepness
  1 / (1 + exp(1)^(-scale))
}


dBW2dBm <- function(dBW) {
  dBW + 30
}


dBW2W <- function(dBW) {
  10^(dBW / 10)
}


W2dBW <- function(W) {
  10 * log10(W)
}


W2dBm <- function(W) {
  dBW2dBm(W2dBW(W))
}


dBm2dBW <- function(dBm) {
  dBm - 30
}


dBm2W <- function(dBm) {
  dBW2W(dBm - 30)
}


### Signal Parameter plots (theoretical)
########################################

sig_param_plots <- function(param.df, range.max = 20000, base_size = 11) {
  distance <- fill <- xmin <- xmax <- ymin <- ymax <- NULL
  
  # define the helper parameters for the plot data frame creation
  cell.kind.unique <- n_distinct(param.df$cell.kind)
  range.total <- rep(seq(10, range.max, by = 10), cell.kind.unique)
  length.range.total <- rep(length(range.total) / cell.kind.unique, cell.kind.unique)
  
  # use the helpers to construct plot dataframe and join with the input params
  df <- tibble(cell.kind = factor(rep(param.df$cell.kind, length.range.total)),
               distance = range.total) %>% 
    left_join(param.df, by = "cell.kind") %>% 
    mutate(dBm = W2dBm(W)) %>% 
    mutate(distance.log10 = log10(distance)) %>% 
    mutate(dBm = distance2dB(distance, ple, W)) %>% 
    mutate(sig.dom = db2s(dBm, 
                          midpoint = midpoint, 
                          steepness = steepness)) %>% 
    mutate(below.dominance.th = case_when(sig.dom >= dominance.th ~ "Above", 
                                          sig.dom < dominance.th ~ "Below"))
  
  # special table to find maximum range distance per cell tile at defined sig dom thresholds
  df.reduced.output <- df %>% 
    dplyr::select(cell.kind, sig.dom, distance) %>% 
    mutate(dif.05 = abs(sig.dom - 0.5),
           dif.005 = abs(sig.dom - 0.05)) %>% 
    group_by(cell.kind) %>% 
    filter(dif.05 == min(dif.05) | 
             dif.005 == min(dif.005)) %>% 
    ungroup %>% 
    pivot_longer(cols = starts_with("dif"), names_to = "threshold", values_to = "dom.difference") %>% 
    group_by(cell.kind, threshold) %>% 
    filter(dom.difference == min(dom.difference)) %>% 
    ungroup() %>% 
    dplyr::select(cell.kind, threshold, max.distance = distance) %>% 
    pivot_wider(id_cols = cell.kind, names_from = threshold, values_from = max.distance)
  
  
  minor.breaks <- rep(1:9, 21) * (10^rep(-10:10, each = 9))
  
  
  strength.distance.plot <- ggplot(df, aes(x = distance, y = dBm)) + 
    geom_line(aes(color = label), size = 1.4) +
    scale_x_log10(labels = scales::trans_format("log10", 
                                                scales::math_format(10^.x)),
                  minor_breaks = minor.breaks) +
    annotation_logticks(sides = "b") +
    labs(title = "Distance vs. Signal strength", 
         x = "log10(Distance (m))",
         y = "Signal strength (dBm)",
         color = "Cell Kind") +
    theme_bw(base_size = base_size) + 
    theme(panel.grid.major = element_line("grey85"))
  
  strength.dominance.plot <- ggplot(df) + 
    geom_line(aes(x = dBm, y = sig.dom, color = label, 
                  alpha = below.dominance.th, linetype = below.dominance.th), size = 1.4) +
    scale_alpha_discrete(range = c(1, 0.4), guide = F) +
    scale_linetype_discrete() +
    labs(title = "Signal dominance vs. strength", 
         x = "Signal strength (dBm)",
         y = "Signal dominance",
         color = "Cell Kind",
         linetype = "Dominance Threshold") +
    guides(color = guide_legend(order = 1), 
           linetype = guide_legend(order = 2)) +
    theme_bw(base_size = base_size) + 
    theme(panel.grid.major = element_line("grey85"))
  
  dominance.distance.plot <- ggplot(df) + 
    geom_line(aes(x = distance, y = sig.dom, color = label, 
                  alpha = below.dominance.th, linetype = below.dominance.th), size = 1.4) +
    scale_alpha_discrete(range = c(1, 0.4), guide = F) +
    scale_linetype_discrete() +
    labs(title = "Distance vs. Signal dominance", 
         x = "Distance (m)",
         y = "Signal dominance",
         color = "Cell Kind",
         linetype = "Dominance Threshold") +
    guides(color = guide_legend(order = 1), 
           linetype = guide_legend(order = 2)) +
    theme_bw(base_size = base_size) + 
    theme(panel.grid.major = element_line("grey85"))
  
  table.gen <- param.df %>% 
    dplyr::select(label, W, ple) %>% 
    mutate(`Transmit Power (dBm)` = W2dBm(W)) %>% 
    dplyr::select(-W, `Path Loss Exponent (pos)` = ple) %>% 
    pivot_longer(-label, names_to = "Gen.Parameter", values_to = "Value") %>% 
    pivot_wider(id_cols = Gen.Parameter, names_from = label, values_from = Value) %>% 
    tableGrob(rows = NULL)
  
  table.mod <- param.df %>% 
    dplyr::select(label, Midpoint = midpoint, Steepness = steepness, `Dominance Threshold` = dominance.th) %>% 
    pivot_longer(-label, names_to = "Mod.Parameter", values_to = "Value") %>% 
    pivot_wider(id_cols = Mod.Parameter, names_from = label, values_from = Value) %>% 
    tableGrob(rows = NULL)
  
  final <- arrangeGrob(strength.distance.plot, table.gen, table.mod, strength.dominance.plot, dominance.distance.plot,
                       layout_matrix = rbind(c(1, 1, 2, 2),
                                             c(1, 1, 3, 3),
                                             c(4, 4, 5, 5),
                                             c(4, 4, 5, 5)))
  
  return(list(final = final,
              strength.distance.plot = strength.distance.plot,
              strength.dominance.plot = strength.dominance.plot,
              dominance.distance.plot = dominance.distance.plot,
              table.gen = table.gen,
              table.mod = table.mod,
              df.reduced.output = df.reduced.output))
  
}

### Type plot
#############

type_plot <- function(data) {
  
  data %>% 
    ggplot() +
    geom_sf(aes(fill = type, color = type)) +
    scale_fill_manual(values = c("white", "grey", "black"),
                      breaks = c("Hole", "Rural", "Urban")) +
    scale_color_manual(values = c("white", "grey", "black"),
                       breaks = c("Hole", "Rural", "Urban"),
                       guide = F) +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(title = "Simulated area by type",
         fill = "Type")
}


### Pop dist plot
#############

pop_plot <- function(data) {
  
  data %>% 
    ggplot() +
    geom_sf(aes(fill = pop, color = pop)) +
    scale_fill_gradient(low = "white", high = "black") +
    scale_color_gradient(low = "white", high = "black", 
                         guide = F) +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(title = "Simulated area by population density",
         fill = "Population")
} 


### pop summary results
###########

pop_summary_results <- function(data) {
  
  data %>% 
    st_drop_geometry() %>% 
    group_by(type) %>% 
    summarise(n.type = n(),
              prop.type = n() / length(data$area.sf$tile.id),
              mean.pop = mean(pop), 
              sd.pop = sd(pop), 
              min.pop = min(pop), 
              max.pop = max(pop),
              sum.pop = sum(pop)) %>% 
    mutate_if(is.numeric, round, 2)
  # left_join(pop.dist.df, by = "type")
  
}



### Pop Density plots 
#############

# custom_ecdf_prep <- function(data) {
#   dat <- data %>% 
#     mutate(pop.plot = pop + 1) %>%  
#     arrange(pop.plot) %>%  
#     mutate(prob = 1 / n()) %>%  
#     mutate(cum.prob = cumsum(prob)) %>%  
#     mutate(cum.prob.comp = 1 - cum.prob) %>%  
#     mutate(log10.cum.prob.comp = log10(cum.prob.comp)) %>% 
#     mutate(log10.pop = log10(pop.plot)) %>%  
#     mutate(cum.prob.comp = 1 - cum.prob)
#   
#   return(dat)
# }

density_plots <- function(data) {
  
  custom_ecdf_prep <- function(data) {
    dat <- data %>% 
      mutate(pop.plot = pop + 1) %>%  
      arrange(pop.plot) %>%  
      mutate(prob = 1 / n()) %>%  
      mutate(cum.prob = cumsum(prob)) %>%  
      mutate(cum.prob.comp = 1 - cum.prob) %>%  
      mutate(log10.cum.prob.comp = log10(cum.prob.comp)) %>% 
      mutate(log10.pop = log10(pop.plot)) %>%  
      mutate(cum.prob.comp = 1 - cum.prob)
    
    return(dat)
  }
  
  minor.breaks <- rep(1:9, 21) * (10^rep(-10:10, each = 9))
  
  pop.emp.dist <- data %>% 
    ungroup() %>% 
    custom_ecdf_prep()
  
  ECCDF.df <- pop.emp.dist %>% 
    dplyr::select(log10.cum.prob.comp, log10.pop, type) %>%
    mutate(log10.cum.prob.comp = round(log10.cum.prob.comp, 3)) %>% # effective plot sample --> faster plotting excluding overplot
    distinct()
  
  ECDF.df <- pop.emp.dist %>% 
    dplyr::select(cum.prob.comp, pop.plot, type) %>%
    mutate(cum.prob.comp = round(cum.prob.comp, 3)) %>% # effective plot sample --> faster plotting excluding overplot
    distinct()
  
  ECCDF.pop.plot <- ECDF.df %>%   
    ggplot() + 
    geom_point(aes(x = pop.plot, y = cum.prob.comp
                   # color = type
    )) + 
    # geom_hline(yintercept = -0.3010300, linetype = "dotted") + 
    # geom_hline(yintercept = -1, linetype = "dotted") + 
    # geom_text(x = 1.8, y = -0.15, label = "50% of the data") + 
    # geom_text(x = 1.8, y = -0.8, label = "90% of the data") + 
    scale_color_ptol() + 
    scale_y_log10(labels = scales::trans_format("log10", 
                                                scales::math_format(10^.x)),
                  minor_breaks = minor.breaks) +
    scale_x_log10(labels = scales::trans_format("log10", 
                                                scales::math_format(10^.x)),
                  minor_breaks = minor.breaks) +
    annotation_logticks(sides = "lb") +
    labs(y = "ECCDF", x = "Mobile phones per tile",  
         colour = "") + 
    theme_bw() +
    theme(legend.position = "bottom",
          text = element_text(size = 20))
  
  ECDF.pop.plot <- ECDF.df %>%   
    ggplot() + 
    geom_point(aes(x = pop.plot, y = cum.prob.comp
                   # color = type
    )) + 
    scale_color_ptol() +
    xlim(0, 30) +
    labs(title = "", y = "", x = "") +
    theme(legend.position = "none",
          plot.margin = unit(c(-0.5, 0, 0, -0.5), "cm")) 
  
  
  combined <- ECCDF.pop.plot +
    annotation_custom(ggplotGrob(ECDF.pop.plot), 
                      xmin = 0, xmax = 1.5, 
                      ymin = -3, ymax = -1.5)
  
  # return(list(ECDF = ECDF.pop.plot,
  #             ECCDF = ECCDF.pop.plot))
  return(combined)
  
}


scatter_density <- function(point, estimator.name){
  
  
  
  
  base.data.ls <- point %>% 
    filter(estimator %in% estimator.name) %>% 
    group_split(scale)
  
  complete.plot <- base.data.ls %>% 
    map(~ggplot(., aes(x = pop, y = estimate)) +
          geom_pointdensity(size = 0.4) +
          scale_color_viridis(guide = F) +
          geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
          # lims(x = c(0, 5), y = c(0, 5)) +
          labs(title = "",
               x = "",
               y = "") +
          facet_wrap(vars(scale)) +
          theme(plot.margin = unit(c(-1, -1, -1, -1), "lines"),
                axis.text = element_text(size = 4),
                strip.text.x = element_text(size = 8)))
  
  complete.plot.final <- arrangeGrob(complete.plot[[1]], complete.plot[[2]], complete.plot[[3]], complete.plot[[4]],
                                     # padding = 2,
                                     layout_matrix = rbind(c(1, 2, 3, 4)))
  # ggsave("2d_density/d.png", complete.plot.final, device = "png")
  # grid.arrange(complete.plot[[1]], complete.plot[[2]], complete.plot[[3]], complete.plot[[4]],
  #             padding = 0,
  #             layout_matrix = rbind(c(1, 2),
  #                                   c(3, 4)))
  
  
  zoom.xy <- base.data.ls %>% 
    map(~ggplot(., aes(x = pop, y = estimate)) +
          geom_pointdensity(size = 0.4) +
          scale_color_viridis(guide = F) +
          geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
          lims(x = c(0, 5), y = c(0, 5)) +
          labs(title = "",
               x = "",
               y = "") +
          facet_wrap(vars(scale)) +
          theme(plot.margin = unit(c(-1, -1, -1, -1), "lines"),
                axis.text = element_text(size = 4),
                strip.text.x = element_text(size = 8)))
  
  zoom.xy.final <- arrangeGrob(zoom.xy[[1]], zoom.xy[[2]], zoom.xy[[3]], zoom.xy[[4]],
                               # padding = 0,
                               top = textGrob("Zoom on XY", gp = gpar(fontsize = 10)),
                               layout_matrix = rbind(c(1, 2, 3, 4)))
  
  minor.breaks <- rep(1:9, 21) * (10^rep(-10:10, each = 9))
  
  log.both <- base.data.ls %>% 
    map(~ggplot(., aes(x = log10(pop + 1), y = log10(estimate + 1))) +
          geom_pointdensity(size = 0.4) +
          scale_color_viridis(guide = F) +
          geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
          scale_y_log10(labels = scales::trans_format("log10", 
                                                      scales::math_format(10^.x)),
                        minor_breaks = minor.breaks) +
          scale_x_log10(labels = scales::trans_format("log10", 
                                                      scales::math_format(10^.x)),
                        minor_breaks = minor.breaks) +
          annotation_logticks(sides = "lb") +
          labs(title = "",
               x = "",
               y = "") +
          facet_wrap(vars(scale)) +
          theme(plot.margin = unit(c(-1, -1, -1, -1), "lines"),
                axis.text = element_text(size = 4),
                strip.text.x = element_text(size = 8)))
  
  log.both.final <- arrangeGrob(log.both[[1]], log.both[[2]], log.both[[3]], log.both[[4]],
                                # padding = 0,
                                top = textGrob("Joint Density log10", gp = gpar(fontsize = 10)),
                                layout_matrix = rbind(c(1, 2, 3, 4)))
  
  scatter.density <- arrangeGrob(complete.plot.final, zoom.xy.final, log.both.final,
                                 # padding = 0,
                                 layout_matrix = rbind(c(1),
                                                       c(2),
                                                       c(3)))
  
  return(scatter.density)
  
}




P_equalizer <- function(P.long.df, signal.strength.comb.dt) {
  
  P.model.equal.start <- P.long.df %>% 
    left_join(signal.strength.comb.dt, by = c("tile.id.num" = "rid", "cell")) 
  
  P.model.summary <- P.model.equal.start %>% 
    mutate(dummy = 1) %>% 
    group_by(tile.id.num) %>% 
    summarise(TH.05 = sum(dummy[s > 0.5]),
              TH.025 = sum(dummy[s > 0.25 & s <= 0.5])) %>% 
    mutate(TH.025 = case_when(TH.05 != 0 ~ 0,
                              TRUE ~ TH.025))
  
  under.any.TH <- P.model.summary %>% 
    filter(TH.05 == 0 & TH.025 ==  0) %>% 
    distinct(tile.id.num) %>% 
    deframe()
  
  TH.05 <- P.model.summary %>% 
    filter(TH.05 >= 1 & TH.025 == 0) %>% 
    distinct(tile.id.num) %>% 
    deframe()
  
  TH.025 <- P.model.summary %>% 
    filter(TH.05 == 0 & TH.025 >= 1) %>% 
    distinct(tile.id.num) %>% 
    deframe()
  
  
  # calculate new p for each of the three groups
  P.model.equal.u.TH <- P.model.equal.start %>% 
    filter(tile.id.num %in% under.any.TH) %>% 
    mutate(pij.new = 1) %>% 
    group_by(tile.id.num) %>% 
    mutate(pij.equal = pij.new / sum(pij.new)) %>% 
    ungroup()
  
  P.model.equal.TH.05 <- P.model.equal.start %>% 
    filter(tile.id.num %in% TH.05) %>%
    filter(s > 0.5) %>%  
    mutate(pij.new = 1) %>% 
    group_by(tile.id.num) %>% 
    mutate(pij.equal = pij.new / sum(pij.new)) %>% 
    ungroup()
  
  P.model.equal.TH.025 <- P.model.equal.start %>% 
    filter(tile.id.num %in% TH.025) %>% 
    filter(s > 0.25) %>%  
    mutate(pij.new = 1) %>% 
    group_by(tile.id.num) %>% 
    mutate(pij.equal = pij.new / sum(pij.new)) %>% 
    ungroup()
  
  P.model.equal.final <- bind_rows(P.model.equal.u.TH, P.model.equal.TH.05, P.model.equal.TH.025)
  
  
  
  return(P.model.equal.final)
  
}


custom_ecdf_prep <- function(data) {
  dat <- data %>% 
    mutate(pop.plot = values + 1) %>%  
    arrange(pop.plot) %>%  
    mutate(prob = 1 / n()) %>%  
    mutate(cum.prob = cumsum(prob)) %>%  
    mutate(cum.prob.comp = 1 - cum.prob) %>%  
    mutate(log10.cum.prob.comp = log10(cum.prob.comp)) %>% 
    mutate(log10.pop = log10(pop.plot)) %>%  
    mutate(cum.prob.comp = 1 - cum.prob)
  
  return(dat)
}


## dev to cell
c.vec.sampler <- function(x) {
  data.table(sample(x = as.character(x$cell), size = mean(x$pop),
                    replace = T, prob = x$pij))
}


# create the c-vector
create_c_vector <- function(signal.strength.llh.combined) {
  
  # covered only by one tile
  C.vec.fixed.helper <- signal.strength.llh.combined %>% 
    filter(coverage.kind == "covered completely by one antenna") %>%
    dplyr::select(tile.id.num, cell, pop)
  
  # One object where tiles are covered by multiple cells
  C.vec.multiple.helper.new <- signal.strength.llh.combined %>% 
    filter(coverage.kind == "covered by multiple antennas") %>% 
    split(.$tile.id.num) 
  
  # Sampling mobile phones within tiles to cells depending on connection probability
  C.vec.multiple <- C.vec.multiple.helper.new %>% 
    map(c.vec.sampler) %>% 
    map(setattr, name = "class", value = "data.table") %>% 
    rbindlist(.) %>% 
    .[, .N, by = V1] %>% 
    as_tibble() %>% 
    dplyr::select(cell = V1, pop = N)
  
  # pulling all c-vec helper objects together and develop final c-vec dataframe
  C.vec.df <- C.vec.multiple %>% 
    bind_rows(C.vec.fixed.helper) %>%
    group_by(cell) %>% 
    summarise(phones.sum = sum(pop))
  
  return(C.vec.df)
  
}



## connection likelihood


create_strength_llh_custom <- function(signal.strength.comb.dt,
                                       signal.strength.llh.param, 
                                       smart.rounding.digits = 3,
                                       area.df) {
  
  # defining the connection probability
  signal.strength.llh.combined <- create_strength_llh(strength = signal.strength.comb.dt, 
                                                      param = signal.strength.llh.param) %>% 
    as_tibble() %>% 
    mutate(tile.id.chr = as.character(rid)) %>% 
    group_by(tile.id.chr) %>%
    mutate(pij = smart_round(pag, smart.rounding.digits)) %>% # round values to the third decimal and assuring that all columns (tiles) add up to 1 (column stocahsticity)
    ungroup() %>%
    left_join(area.df, by = "tile.id.chr") %>% 
    mutate(coverage.kind = case_when(pop == 0 ~ "0 population",
                                     pij == 1 ~ "covered completely by one antenna",
                                     pij > 0 & pij < 1 ~ "covered by multiple antennas",
                                     pij == 0 ~ "tile covered unsufficiently")) %>% 
    dplyr::select(-pag)
  
  # aggregating and specifying the tiles that are uncovered (if there are some)
  tiles.cat <- signal.strength.llh.combined %>% 
    filter(!pij == 0) %>% 
    dplyr::select(tile.id.num, coverage.kind) %>% 
    group_by(tile.id.num) %>% 
    summarise(count = n())
  
  # how many tiles are not sufficiently covered
  missings <- anti_join(area$area.df, tiles.cat, by = "tile.id.num") # implement non zero pop
  cat(paste("Number of tiles which are unsufficiently covered:", length(missings$tile.id.num)))
  
  return(list(signal.strength.llh.combined = signal.strength.llh.combined,
              tiles.cat = tiles.cat))
}


create_supertile_index <- function(P.long.df, elements, prior.var){
  
  
  P.dt <- P.long.df %>% 
    rename(prior = !!as.name(prior.var)) %>% 
    as.data.table(.) %>% 
    .[, order.var := factor(.I)]
  
  
  dat <- P.dt %>% 
    .[, ..elements] %>% 
    .[!pij == 0] %>%
    setorder(., cell.chr) %>%
    .[, cell.comp := paste0(cell.chr, collapse = ""), by = tile.id.chr] %>%
    .[, pij.comp := paste0(pij, collapse = ""), by = tile.id.chr] %>%
    .[, supertile.id := .GRP, by = .(cell.comp, pij.comp, prior)]
  
  final <- P.dt[dat, on = c("tile.id.chr", "cell.chr")] %>%
    setorder(., order.var) %>%
    .[, supertile.id]
  
  return(final)
}


quantize_mag <- function(x, n) {
  
  # Bit depth
  n <- n
  
  # number of quantized levels
  L <- 2^n
  
  # helpers
  x.max <- max(x)
  x.min <- min(x)
  
  # Step-size
  delta <- (x.max - x.min) / L
  
  # calculate index
  I <- ceiling( ((x - x.min) / delta))
  
  # Quantized magnitue
  x.quant <- x.min + I * delta
  
  return(x.quant)
  
  
}

con_llh_sens_custom <- function(strength, L.kind, digits) {
  
  e <- strength %>% 
    dplyr::select(tile.id.num, cell, s = .data[[L.kind]], sig_d_th) %>% 
    mutate(max_overlapping_cells = 100) %>% 
    as.data.table() %>% 
    .[, pag := s / sum(s), by = tile.id.num] %>% 
    .[, pag.rounded := smart_round(pag, digits), by = tile.id.num] # round values to the third decimal and assuring that all columns (tiles) add up to 1 (column stocahsticity)
  # .[, by = rid, .(os = order(s), s, max_overlapping_cells)] %>% 
  # .[os <= max_overlapping_cells, `:=`(pag, s/sum(s)), by = rid]
  
  final <- e$pag.rounded
  
  return(final)
  
}

EM_est_supertiles <- function(area, c.vec.dt, P.dt, prior.var, n.iter, selected.range, ldt.dt, message = T) {
  
  elements <- c("tile.id.chr", "cell.chr", "pij", "prior")
  
  P.dt.supertiles <- P.dt %>% 
    mutate(supertile.id = create_supertile_index(., elements = elements, prior.var = prior.var)) %>%
    mutate(supertile.id.chr = as.numeric(supertile.id)) %>%
    mutate(supertile.id.fac = factor(supertile.id.chr)) %>%
    mutate(supertile.id.num = as.numeric(supertile.id)) %>%
    dplyr::select(-supertile.id) %>%
    as.data.table()
  
  P.dt.star <- P.dt.supertiles %>% 
    dplyr::select(i = cell.num, j = contains("supertile.id.num"), pij) %>% 
    distinct() %>% 
    as.data.table()
  
  
  supertile.joiner.prior <- P.dt.supertiles %>%
    right_join(area$area.df, by = c("tile.id.num", "tile.id.fac", "tile.id.chr", prior.var)) %>% # to assure that uncovered tiles are also included
    dplyr::select(tile.id.num, !!as.name(prior.var), contains("supertile.id.num")) %>%
    distinct() %>%
    arrange(tile.id.num)
  
  
  supertile.joiner <- supertile.joiner.prior %>%
    dplyr::select(tile.id.num, contains("supertile.id.num"))
  
  
  a.tile.helper <- supertile.joiner.prior %>%
    dplyr::select(contains("supertile.id.num"), !!as.name(prior.var)) %>%
    group_by(across(contains("supertile.id.num"))) %>%
    summarise(a = sum(!!as.name(prior.var))) %>% # uniform vector of number of normal tiles
    drop_na()# uncovered tiles
  
  a.tile.vec <- a.tile.helper %>%
    deframe()
  
  a.vec.dt <- a.tile.helper %>%
    dplyr::select(j = contains("supertile.id.num"), u = a) %>%
    as.data.table(.)
  
  
  cdt <- c.vec.dt
  pij <- cdt[P.dt.star, on = "i"]
  pij <- pij[c > 0] # remove those lines where c==0 because it will create 0 division
  # adt <- data.table(a = a.vec)
  # tiles <- adt[, .(j = 1:.N, u = a)]
  tiles <- a.vec.dt
  keep <- a.vec.dt # base dataframe for the selected iterations
  
  for(m in 1:(n.iter)){
    
    if(message == T) {
      cat(format(Sys.time()), paste0("---- calculating u", m), "----\n")
    }
    
    cols <- c("j", paste0("u"))
    ju <- tiles[, cols, with = F]
    setnames(ju, c("j", "u"))
    pij <- ju[pij, on = "j"]
    denom <- pij[, .(sum_pik_uk = sum(u * pij)), by = i]
    pij <- denom[pij, on = "i"]
    faktor <- pij[, .(f = sum(c * pij / sum_pik_uk)), by = j]
    faktor.adj <- faktor[, f := fifelse(test = {is.na(f) | is.nan(f) | is.infinite(f)}, 1, f)] # if else to assure that the posterior is 1 to secure the same estimand value after ldt
    pij[, c("u", "sum_pik_uk") := NULL]
    tiles <- faktor.adj[tiles, on = "j"]
    # tiles <- eval(parse(text = paste0("tiles[, u := u * f]")))
    tiles <- eval(parse(text = paste0("tiles[,  u := fifelse(u * f < ldt.dt, 0, u * f)]")))
    tiles[, "f" := NULL]
    
    if(m %in% selected.range) {
      keep <- tiles[keep, on = "j"]
      keep <- eval(parse(text = paste0("keep[, u_", m, ":= u]")))
      keep[, "u" := NULL]
    }
  }
  
  final <- keep %>% 
    rename(supertile.id.num = j) %>% 
    right_join(supertile.joiner, by = "supertile.id.num") %>% 
    group_by(across(contains("supertile.id.num"))) %>% 
    mutate(across(starts_with("u_"), ~ . / n())) %>%
    ungroup() %>%
    dplyr::select(tile.id.num, prior = i.u, starts_with("u_")) %>%
    mutate(across(starts_with("u_"), ~if_else(is.na(.), 0, .)))
  
  
  return(final)
  
}



DF_est_relaxed_iter_supertiles <- function(area, c.vec.dt, P.dt, prior.var = "prior.uninformative", n.iter, selected.range, ldt.dt, message = T){
  
  
  elements <- c("tile.id.chr", "cell.chr", "pij", "prior")
  
  P.dt.supertiles <- P.dt %>% 
    mutate(supertile.id = create_supertile_index(., elements = elements, prior.var = prior.var)) %>%
    mutate(supertile.id.chr = as.numeric(supertile.id)) %>%
    mutate(supertile.id.fac = factor(supertile.id.chr)) %>%
    mutate(supertile.id.num = as.numeric(supertile.id)) %>%
    dplyr::select(-supertile.id) %>%
    as.data.table()
  
  P.dt.star <- P.dt.supertiles %>% 
    dplyr::select(i = cell.num, j = contains("supertile.id.num"), pij) %>% 
    distinct() %>% 
    as.data.table()
  
  
  supertile.joiner.prior <- P.dt.supertiles %>%
    right_join(area$area.df, by = c("tile.id.num", "tile.id.fac", "tile.id.chr", prior.var)) %>% # to assure that uncovered tiles are also included
    dplyr::select(tile.id.num, !!as.name(prior.var), contains("supertile.id.num")) %>%
    distinct() %>%
    arrange(tile.id.num)
  
  
  supertile.joiner <- supertile.joiner.prior %>%
    dplyr::select(tile.id.num, contains("supertile.id.num"))
  
  
  a.tile.helper <- supertile.joiner.prior %>%
    dplyr::select(contains("supertile.id.num"), !!as.name(prior.var)) %>%
    group_by(across(contains("supertile.id.num"))) %>%
    summarise(a = sum(!!as.name(prior.var))) %>% # uniform vector of number of normal tiles
    drop_na()# uncovered tiles
  
  a.tile.vec <- a.tile.helper %>%
    deframe()
  
  names.tile.vec <- a.tile.vec %>% 
    names(.) %>% as.numeric(.)
  
  
  P.star.spm.helper <- P.dt.supertiles %>%
    dplyr::select(i = cell.num, j = contains("supertile.id.num"), x = pij)
  P.star.spm <- sparseMatrix(i = P.star.spm.helper$i, j = P.star.spm.helper$j, x = P.star.spm.helper$x)
  c.vec <- c(c.vec.dt)$c
  A.spm <- .sparseDiagonal(n = length(a.tile.vec), x = a.tile.vec)
  
  Y <- P.star.spm %*% A.spm %*% t(P.star.spm) 
  Y1 <- VCA::MPinv(Y) %*% (c.vec - P.star.spm %*% a.tile.vec)
  u <- as.vector(A.spm %*% t(P.star.spm) %*% Y1 + a.tile.vec)
  
  # DF raw estimate
  u.pos.dt <- data.table(j = names.tile.vec, u = u) %>% 
    .[, u := fifelse(u < 1, 1, u)]
  
  
  cdt <- c.vec.dt
  pij <- cdt[P.dt.star, on = "i"]
  pij <- pij[c > 0] # remove those lines where c==0 because it will create 0 division
  # adt <- data.table(a = a.vec)
  # tiles <- adt[, .(j = 1:.N, u = a)]
  tiles <- u.pos.dt
  keep <- u.pos.dt # base dataframe for the selected iterations
  
  for(m in 1:(n.iter)){
    
    if(message == T) {
      cat(format(Sys.time()), paste0("---- calculating u", m), "----\n")
    }
    
    cols <- c("j", paste0("u"))
    ju <- tiles[, cols, with = F]
    setnames(ju, c("j", "u"))
    pij <- ju[pij, on = "j"]
    denom <- pij[, .(sum_pik_uk = sum(u * pij)), by = i]
    pij <- denom[pij, on = "i"]
    faktor <- pij[, .(f = sum(c * pij / sum_pik_uk)), by = j]
    faktor.adj <- faktor[, f := fifelse(test = {is.na(f) | is.nan(f) | is.infinite(f)}, 1, f)] # if else to assure that the posterior is 1 to secure the same estimand value after ldt
    pij[, c("u", "sum_pik_uk") := NULL]
    tiles <- faktor.adj[tiles, on = "j"]
    # tiles <- eval(parse(text = paste0("tiles[, u := u * f]")))
    tiles <- eval(parse(text = paste0("tiles[,  u := fifelse(u * f < ldt.dt, 0, u * f)]")))
    tiles[, "f" := NULL]
    
    if(m %in% selected.range) {
      keep <- tiles[keep, on = "j"]
      keep <- eval(parse(text = paste0("keep[, u_", m, ":= u]")))
      keep[, "u" := NULL]
    }
  }
  
  final <- keep %>% 
    rename(supertile.id.num = j) %>% 
    right_join(supertile.joiner, by = "supertile.id.num") %>% 
    group_by(across(contains("supertile.id.num"))) %>% 
    mutate(across(starts_with("u_"), ~ . / n())) %>%
    ungroup() %>%
    dplyr::select(tile.id.num, prior = i.u, starts_with("u_")) %>%
    mutate(across(starts_with("u_"), ~if_else(is.na(.), 0, .)))
  
  
  return(final)
  
}





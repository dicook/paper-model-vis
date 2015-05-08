# Fix jittering

som_iterate <- function(df, grid, nsteps = 100, stepsize = 10, alpha = 0.05, radius = NULL, ...) {
  if (is.null(radius)) {
    radius <- c(quantile(unit.distances(grid, FALSE), 0.67), 0)
  }

  # Alpha decreases linearly
  alpha_step <- function(i) alpha - (alpha - 0.01) * (i + c(-1, 0)) / nsteps
  # Radius decraeses linearly to 1 by 1/3 of steps
  radius_step <- function(i) {
    r <- radius[1] - (radius[1] - radius[2]) * 3.0 * (i - 1) / nsteps;
    ifelse(r < radius[2], max(0.5, radius[2]), r)
  }

  print(radius_step(1:nsteps))

  i <- 1
  fit <- kohonen::som(df, grid, rlen = stepsize, alpha = alpha_step(i), radius = radius_step(i), ...)

  step <- function() {
    i <<- i + 1
    fit <<- kohonen::som(df, grid, rlen = stepsize, init = fit$codes,
      alpha = alpha_step(i), radius=radius_step(i), keep.data = TRUE
    )

    fit
  }

  structure(c(list(fit), replicate(nsteps - 1, step(), simplify=FALSE)), class="somiter")
}


summary.somiter <- function(x, ...) {
  interesting <- function(fit) {
    df <- data.frame(
      alpha_start = fit$alpha[1], alpha_end = fit$alpha[2],
      radius = fit$radius[1],
      change_mean = mean(fit$changes),
      dist_mean = mean(fit$distances),
      dist_sd = sd(fit$distances)
    )
    df$codes <- list(fit$codes)
    df$map <- list(fit$grid$pts[fit$unit.classif, ])
    df
  }
  df <- do.call("rbind", lapply(x, interesting))
  df$step <- 1:nrow(df)
  class(df) <- c("somitersum", class(df))
  rownames(df) <- paste("step", 1:nrow(df), sep="")
  df
}

ggobi.somiter <- function(data, cls, map, extra = NULL, ...) {

  all_fits <- summary(data)
  fits <- subset(all_fits, select= -c(codes, map))

#  jittering <- jitter(all_fits[[1, "map"]]) - all_fits[[1, "map"]]

  codes <- all_fits[[1, "codes"]]
  #map <- all_fits[[1, "map"]]
  somdata <- rbind(codes, data[[1]]$data)
  rownames(somdata) <- 1:dim(somdata)[1]

  g <- ggobi(somdata)
  glyph_colour(g[[1]]) <- c(8, 1, 5, 6)[c(rep(1,nrow(codes)), as.numeric(cls)+1)]
  edges(g[1]) <- map

#  distances <- melt(sapply(data, function(x) x$distances))
#  names(distances) <- c("oid", "step", "value")
#  distances$oid <- factor(distances$oid)
#  ggobi_longitudinal(distances, step, oid, g = g)

#  ggobi_longitudinal(fits, step, g = g)
#  d <- display(g["fits"], vars = list(X = "step", Y = "dist_mean"))
#  edges(d) <- g["fits-edges"]
  g["fits"] <- fits

  RGtk2::gSignalConnect(g, "identify-point", function(gg, plot, id, data) {
    if (id == -1) return()
    id <- id + 1

    codes <- all_fits[[id, "codes"]]
    gg[1][1:nrow(codes),1:ncol(codes)] <- codes
  })

  invisible(g)
}

ggobi.som <- function(data, cls, extra = NULL, ...) {

  codes <- data$codes
  map <- list(data$grid$pts[data$unit.classif, ])
  somdata <- rbind(codes, data$data)
  rownames(somdata) <- 1:dim(somdata)[1]

  g <- ggobi(somdata)
  glyph_colour(g[[1]]) <- c(8, 1, 5, 6)[c(rep(1,dim(codes)[1]), as.numeric(cls)+1)]
  edges(g[1]) <- map
}

ggobi.som <- function(data, id, cls, map, extra = NULL, ...) {

  all_fits <- summary(data)

  codes <- all_fits[[id, "codes"]]
  somdata <- rbind(codes, data[[id]]$data)
  rownames(somdata) <- 1:dim(somdata)[1]

  g <- ggobi(somdata)
  glyph_colour(g[[1]]) <- c(8, 1, 5, 6)[c(rep(1,dim(codes)[1]), as.numeric(cls)+1)]
  edges(g[1]) <- map
}



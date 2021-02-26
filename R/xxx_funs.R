require(sf)
require(rayshader)
require(magick)


square_bbox <- function(x, expand = .1) {
  bbx <- st_bbox(st_transform(x, 3857))
  xtick <- bbx[1] + (bbx[3] - bbx[1]) / 2
  ytick <- bbx[2] + (bbx[4] - bbx[2]) / 2
  
  x_dim <- (bbx[3] - bbx[1])
  y_dim <- (bbx[4] - bbx[2])
  
  
  max_dim <- (max(x_dim, y_dim) / 2) * (1 + expand)
  
  square <-
    c(xtick - max_dim, ytick - max_dim, xtick + max_dim, ytick + max_dim)
  names(square) <- names(bbx)
  class(square) <- "bbox"
  
  
  bbx_end <- st_as_sfc(square)
  bbx_end <- st_set_crs(bbx_end, 3857)
  bbx_end <- st_transform(bbx_end, st_crs(x))
  
  return(bbx_end)
}


render_movie2 = function(filename,
                      type = "orbit",
                      frames = 360,
                      fps = 30,
                      phi = 30,
                      theta = 0,
                      zoom = NULL,
                      fov = NULL,
                      title_text = NULL,
                      title_offset = c(20, 20),
                      title_color = "black",
                      title_size = 30,
                      title_font = "sans",
                      title_bar_color = NULL,
                      title_bar_alpha = 0.5,
                      image_overlay = NULL,
                      vignette = FALSE,
                      title_position = "northwest",
                      loop = TRUE,
                      progbar = interactive(),
                      subtitle_text = NULL,
                      subtitle_offset = c(20, 20),
                      subtitle_color = "black",
                      subtitle_size = 30,
                      subtitle_font = "sans",
                      subtitle_bar_color = NULL,
                      subtitle_bar_alpha = 0.5,
                      subtitle_position = "northwest",
                      ...) {
  if (rgl::rgl.cur() == 0) {
    stop("No rgl window currently open.")
  }
  if (!("gifski" %in% rownames(utils::installed.packages()))) {
    stop("`gifski` package required for render_gif()")
  }
  if (is.null(filename)) {
    stop("render_gif requires a filename")
  }
  if (!is.null(title_text)) {
    has_title = TRUE
  } else {
    has_title = FALSE
  }
  if (!is.null(subtitle_text)) {
    has_subtitle = TRUE
  } else {
    has_subtitle = FALSE
  }
  
  if (length(title_offset) != 2) {
    stop("`title_offset` needs to be length-2 vector")
  }
  if (!is.null(image_overlay)) {
    if ("character" %in% class(image_overlay)) {
      image_overlay_file = image_overlay
      has_overlay = TRUE
    } else if ("array" %in% class(image_overlay)) {
      image_overlay_file = tempfile()
      png::writePNG(image_overlay_file)
      has_overlay = TRUE
    }
  } else {
    has_overlay = FALSE
  }
  if (substring(filename, nchar(filename) - 3, nchar(filename)) != ".mp4") {
    filename = paste0(filename, ".mp4")
  }
  windowsize = rgl::par3d()$viewport
  if (is.null(fov)) {
    fov = rgl::par3d()$FOV
  }
  if (is.null(zoom)) {
    zoom = rgl::par3d()$zoom
  }
  if (is.null(phi) || is.null(theta)) {
    rotmat = rot_to_euler(rgl::par3d()$userMatrix)
    if (is.null(phi)) {
      phi = rotmat[1]
    }
    if (is.null(theta)) {
      if (0.001 > abs(abs(rotmat[3]) - 180)) {
        theta = -rotmat[2] + 180
      } else {
        theta = rotmat[2]
      }
    }
  }
  png_files = file.path(tempdir(), sprintf("image%d.png", seq_len(frames)))
  on.exit(unlink(png_files))
  if (type == "orbit") {
    theta_vector = seq(0, 360, length.out = frames + 1)[-(frames + 1)]
    for (i in seq_len(frames)) {
      render_camera(
        theta = theta_vector[i],
        phi = phi,
        zoom = zoom,
        fov = fov
      )
      rgl::snapshot3d(filename = png_files[i], top = FALSE)
    }
  } else if (type == "oscillate") {
    theta_vector = theta + 45 * sin(seq(0, 360, length.out = frames + 1)[-(frames +
                                                                             1)] * pi / 180)
    for (i in seq_len(frames)) {
      render_camera(
        theta = theta_vector[i],
        phi = phi,
        zoom = zoom,
        fov = fov
      )
      rgl::snapshot3d(filename = png_files[i], top = FALSE)
    }
  } else if (type == "custom") {
    if (length(theta) == 1)
      theta = rep(theta, frames)
    if (length(phi) == 1)
      phi = rep(phi, frames)
    if (length(zoom) == 1)
      zoom = rep(zoom, frames)
    if (length(fov) == 1)
      fov = rep(fov, frames)
    if (!all(c(length(theta), length(phi), length(zoom), length(fov)) == frames)) {
      stop("All camera vectors must be the same length (or fixed values)")
    }
    for (i in seq_len(frames)) {
      render_camera(
        theta = theta[i],
        phi = phi[i],
        zoom = zoom[i],
        fov = fov[i]
      )
      rgl::snapshot3d(filename = png_files[i], top = FALSE)
    }
  } else {
    stop("Unknown type: ", type)
  }
  temp = png::readPNG(png_files[1])
  dimensions = dim(temp)
  if (dimensions[1] %% 2 != 0) {
    dimensions[1] = dimensions[1] - 1
  }
  if (has_overlay) {
    if (!("magick" %in% rownames(utils::installed.packages()))) {
      stop("`magick` package required for adding overlay")
    }
    if (progbar) {
      pb = progress::progress_bar$new(format = "  Adding overlay image [:bar] :percent eta: :eta",
                                      total = frames,
                                      width = 60)
    }
    for (i in seq_len(frames)) {
      if (progbar) {
        pb$tick()
      }
      rayimage::add_image_overlay(png_files[i],
                                  image_overlay = image_overlay_file,
                                  filename = png_files[i])
    }
  }
  if (vignette || is.numeric(vignette)) {
    if (!("magick" %in% rownames(utils::installed.packages()))) {
      stop("`magick` package required for adding overlay")
    }
    if (progbar) {
      pb = progress::progress_bar$new(format = "  Adding vignetting [:bar] :percent eta: :eta",
                                      total = frames,
                                      width = 60)
    }
    for (i in seq_len(frames)) {
      if (progbar) {
        pb$tick()
      }
      rayimage::add_vignette(png_files[i], vignette = vignette, filename = png_files[i])
    }
  }
  if (has_title) {
    if (!("magick" %in% rownames(utils::installed.packages()))) {
      stop("`magick` package required for adding title")
    }
    if (progbar) {
      pb = progress::progress_bar$new(format = "  Adding title text [:bar] :percent eta: :eta",
                                      total = frames,
                                      width = 60)
    }
    for (i in seq_len(frames)) {
      if (progbar) {
        pb$tick()
      }
      rayimage::add_title(
        png_files[i],
        filename = png_files[i],
        title_text = title_text,
        title_bar_color = title_bar_color,
        title_bar_alpha = title_bar_alpha,
        title_offset = title_offset,
        title_color = title_color,
        title_position = title_position,
        title_size = title_size,
        title_font = title_font
      )
    }
  }
  if (has_subtitle) {
    if (!("magick" %in% rownames(utils::installed.packages()))) {
      stop("`magick` package required for adding subtitle")
    }
    if (progbar) {
      pb = progress::progress_bar$new(format = "  Adding subtitle text [:bar] :percent eta: :eta",
                                      total = frames,
                                      width = 60)
    }
    for (i in seq_len(frames)) {
      if (progbar) {
        pb$tick()
      }
      rayimage::add_title(
        png_files[i],
        filename = png_files[i],
        title_text = subtitle_text,
        title_bar_color = subtitle_bar_color,
        title_bar_alpha = subtitle_bar_alpha,
        title_offset = subtitle_offset,
        title_color = subtitle_color,
        title_position = subtitle_position,
        title_size = subtitle_size,
        title_font = subtitle_font,
        title_style = "italic"
      )
    }
  }
  # gifski::gifski(
  #   png_files = png_files,
  #   gif_file = filename,
  #   height = dimensions[1],
  #   width = dimensions[2],
  #   delay = 1 / fps,
  #   loop = loop,
  #   progress = progbar
  # )
  
  av::av_encode_video(png_files, output = filename, framerate = fps, 
                      vfilter = paste0("scale=",dimensions[1],":-2"), audio=NULL)
}


transition_values <- function(from,
                              to,
                              steps = 10,
                              one_way = FALSE,
                              type = "cos") {
  if (!(type %in% c("cos", "lin")))
    stop("type must be one of: 'cos', 'lin'")
  
  range <- c(from, to)
  middle <- mean(range)
  half_width <- diff(range) / 2
  
  # define scaling vector starting at 1 (between 1 to -1)
  if (type == "cos") {
    scaling <-
      cos(seq(0, 2 * pi / ifelse(one_way, 2, 1), length.out = steps))
  } else if (type == "lin") {
    if (one_way) {
      xout <- seq(1, -1, length.out = steps)
    } else {
      xout <- c(seq(1, -1, length.out = floor(steps / 2)),
                seq(-1, 1, length.out = ceiling(steps / 2)))
    }
    scaling <- approx(x = c(-1, 1),
                      y = c(-1, 1),
                      xout = xout)$y
  }
  
  middle - half_width * scaling
}
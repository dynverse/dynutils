#' Read/write R objects to a H5 file.
#'
#' @param x R object to write.
#' @param path Path to read from/write to.

#' @export
read_h5 <- function(path) {
  requireNamespace("hdf5r")
  file_h5 <- hdf5r::H5File$new(path, "r")

  lis <- .read_h5(file_h5)

  file_h5$close_all()

  lis
}

.read_h5 <- function(file_h5) {
  requireNamespace("hdf5r")
  requireNamespace("Matrix")
  if (!"object_class" %in% hdf5r::h5attr_names(file_h5)) {
    stop("Object class not found (path=", file_h5$get_filename(), ", obj_name=", file_h5$get_obj_name())
  }
  object_class <- hdf5r::h5attr(file_h5, "object_class")

  ## VECTOR ##
  if (object_class == "null") {
    NULL
  } else if (object_class == "vector") {
    data_file <- file_h5[["data"]]

    # out <- data_file[]
    # if ("names" %in% names(file_h5)) names(out) <- file_h5[["names"]][]

    # workaround
    out <- .read_h5_vec(data_file)
    if ("names" %in% names(file_h5)) names(out) <- .read_h5_vec(file_h5[["names"]])

    out

    ## DENSE MATRIX ##
  } else if (object_class == "dense_matrix") {
    out <- file_h5[["data"]][,]
    # if ("rownames" %in% names(file_h5)) rownames(out) <- file_h5[["rownames"]][]
    # if ("colnames" %in% names(file_h5)) colnames(out) <- file_h5[["colnames"]][]

    # workaround
    if ("rownames" %in% names(file_h5)) rownames(out) <- .read_h5_vec(file_h5[["rownames"]])
    if ("colnames" %in% names(file_h5)) colnames(out) <- .read_h5_vec(file_h5[["colnames"]])
    out

    ## LIST ##
  } else if (object_class == "list") {
    subfile <- file_h5[["data"]]

    has_names <- "names" %in% names(file_h5)
    # nms <- if (has_names) file_h5[["names"]][] else names(subfile)
    # workaround
    nms <- if (has_names) .read_h5_vec(file_h5[["names"]]) else names(subfile)

    out <- map(nms, ~.read_h5(subfile[[.]]))
    if (has_names) names(out) <- nms
    if ("class" %in% names(file_h5)) class(out) <- file_h5[["class"]][]

    out

    ## DATA FRAME ##
  } else if (object_class == "data_frame") {
    # rownames <- file_h5[["rownames"]][]
    # colnames <- file_h5[["colnames"]][]

    # workaround
    rownames <- .read_h5_vec(file_h5[["rownames"]])
    colnames <- .read_h5_vec(file_h5[["colnames"]])

    data <- file_h5[["data"]]
    out <- map(colnames, ~ .read_h5_vec(data[[.]])) %>% data.frame(check.names = FALSE, stringsAsFactors = FALSE)
    rownames(out) <- rownames
    colnames(out) <- colnames
    out

    ## SPARSE MATRIX ##
  } else if (object_class == "sparse_matrix") {
    i <- file_h5[["i"]][]
    p <- file_h5[["p"]][]
    x <- file_h5[["x"]][]
    dims <- file_h5[["dims"]][]

    # rn <- if ("rownames" %in% names(file_h5)) file_h5[["rownames"]][] else NULL
    # cn <- if ("colnames" %in% names(file_h5)) file_h5[["colnames"]][] else NULL

    rn <- if ("rownames" %in% names(file_h5)) .read_h5_vec(file_h5[["rownames"]]) else NULL
    cn <- if ("colnames" %in% names(file_h5)) .read_h5_vec(file_h5[["colnames"]]) else NULL

    Matrix::sparseMatrix(
      i = i,
      p = p,
      x = x,
      dims = dims,
      dimnames = list(rn, cn),
      index1 = FALSE
    )
  }
}

.read_h5_vec <- function(file_h5)
  # workaround for https://github.com/hhoeflin/hdf5r/issues/118
  if (file_h5$dims == 0 && "H5T_STRING" %in% class(file_h5$get_type())) {
    character(0)
  } else {
    file_h5[]
  }

#' @rdname read_h5
#' @export
write_h5 <- function(x, file) {
  requireNamespace("hdf5r")
  file_h5 <- hdf5r::H5File$new(file, "w")

  .write_h5(x, file_h5, "")

  file_h5$close_all()
}

.write_h5 <- function(x, file_h5, name) {
  requireNamespace("hdf5r")
  requireNamespace("Matrix")
  if (is.null(x)) {
    file_h5[[name]] <- 0
    hdf5r::h5attr(file_h5[[name]], "object_class") <- "null"
  } else if (any(grepl("^[dlniz]..Matrix$", class(x)))) {
    ipx <- as(x, "dgCMatrix")
    subfile <- file_h5$create_group(name)
    h5attr(subfile, "object_class") <- "sparse_matrix"
    subfile[["i"]] <- ipx@i
    subfile[["p"]] <- ipx@p
    subfile[["x"]] <- ipx@x
    subfile[["dims"]] <- dim(ipx)
    if (!is.null(rownames(ipx))) {
      subfile[["rownames"]] <- rownames(ipx)
    }
    if (!is.null(colnames(ipx))) {
      subfile[["colnames"]] <- colnames(ipx)
    }
  } else if (is.matrix(x)) {
    subfile <- file_h5$create_group(name)
    hdf5r::h5attr(subfile, "object_class") <- "dense_matrix"
    if (!is.null(rownames(x))) subfile[["rownames"]] <- rownames(x)
    if (!is.null(colnames(x))) subfile[["colnames"]] <- colnames(x)
    subfile[["data"]] <- x
  } else if (is.data.frame(x)) {
    subfile <- file_h5$create_group(name)
    h5attr(subfile, "object_class") <- "data_frame"
    subfile[["rownames"]] <- rownames(x)
    subfile[["colnames"]] <- colnames(x)
    subsubfile <- subfile$create_group("data")
    for (xn in names(x)) {
      subsubfile[[xn]] <- x[[xn]]
    }
  } else if (is.atomic(x)) {
    subfile <- file_h5$create_group(name)
    h5attr(subfile, "object_class") <- "vector"
    if (!is.null(names(x))) subfile[["names"]] <- names(x)
    subfile[["data"]] <- x
  } else if (is.list(x)) {
    if (name == "") {
      subfile <- file_h5
    } else {
      subfile <- file_h5$create_group(name)
    }
    h5attr(subfile, "object_class") <- "list"
    subfile[["class"]] <- class(x)
    if (!is.null(names(x))) subfile[["names"]] <- names(x)

    subsubfile <- subfile$create_group("data")

    if (is.null(names(x)) && length(x) > 0) names(x) <- paste0("elem", seq_along(x))

    for (xn in names(x)) {
      .write_h5(x[[xn]], subsubfile, xn)
    }
  }
}


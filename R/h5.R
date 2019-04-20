#' Read/write R objects to a H5 file.
#'
#' @param x R object to write.
#' @param path Path to read from/write to.
#' @param file_h5 A H5 file to read from/write to.
#' @export
read_h5 <- function(path) {
  requireNamespace("hdf5r")
  file_h5 <- hdf5r::H5File$new(path, "r")
  on.exit(file_h5$close_all())

  read_h5_(file_h5)
}

#' @rdname read_h5
#' @export
read_h5_ <- function(file_h5) {
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

    out <- map(nms, ~read_h5_(subfile[[.]]))
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

.read_h5_vec <- function(file_h5) {
  # workaround for https://github.com/hhoeflin/hdf5r/issues/118
  if (file_h5$dims == 0 && "H5T_STRING" %in% class(file_h5$get_type())) {
    character(0)
  } else {
    x <- file_h5[]

    # workaround for https://github.com/dynverse/dyno/issues/43
    is_workaround <- is.integer(x) && "is_logical" %in% hdf5r::h5attr_names(file_h5) && hdf5r::h5attr(file_h5, "is_logical") == "true"
    if (is_workaround) {
      x <- ifelse(x == 2L, NA, ifelse(x == 1L, TRUE, FALSE))
    }

    x
  }
}

.write_h5_vec <- function(x, file_h5, name) {
  # workaround for https://github.com/dynverse/dyno/issues/43
  was_logical <- is.logical(x)
  if (is.logical(x)) {
    if (length(x) == 0) {
      x <- integer(0)
    } else {
      x <- ifelse(is.na(x), 2L, ifelse(x, 1L, 0L))
    }
  }

  file_h5[[name]] <- x

  # workaround for https://github.com/dynverse/dyno/issues/43
  subfile <- file_h5[[name]]
  hdf5r::h5attr(subfile, "is_logical") <- ifelse(was_logical, "true", "false")

  return()
}

#' @rdname read_h5
#' @export
write_h5 <- function(x, path) {
  requireNamespace("hdf5r")
  file_h5 <- hdf5r::H5File$new(path, "w")
  on.exit(file_h5$close_all())

  write_h5_(x, file_h5, "")
}

#' @rdname read_h5
#' @importFrom methods as
#' @export
write_h5_ <- function(x, file_h5, path) {
  requireNamespace("hdf5r")
  requireNamespace("Matrix")

  if (path == "") {
    subfile <- file_h5
  } else {
    subfile <- file_h5$create_group(path)
  }

  if (is.null(x)) {
    hdf5r::h5attr(subfile, "object_class") <- "null"
  } else if (is_sparse(x)) {
    ipx <- as(x, "dgCMatrix")
    hdf5r::h5attr(subfile, "object_class") <- "sparse_matrix"
    subfile[["i"]] <- ipx@i
    subfile[["p"]] <- ipx@p
    # subfile[["x"]] <- ipx@x
    # workaround
    .write_h5_vec(ipx@x, subfile, "x")
    subfile[["dims"]] <- dim(ipx)
    if (!is.null(rownames(ipx))) {
      subfile[["rownames"]] <- rownames(ipx)
    }
    if (!is.null(colnames(ipx))) {
      subfile[["colnames"]] <- colnames(ipx)
    }
  } else if (is.matrix(x)) {
    hdf5r::h5attr(subfile, "object_class") <- "dense_matrix"
    if (!is.null(rownames(x))) subfile[["rownames"]] <- rownames(x)
    if (!is.null(colnames(x))) subfile[["colnames"]] <- colnames(x)
    subfile[["data"]] <- x
  } else if (is.data.frame(x)) {
    hdf5r::h5attr(subfile, "object_class") <- "data_frame"
    subfile[["rownames"]] <- rownames(x)
    subfile[["colnames"]] <- colnames(x)
    subsubfile <- subfile$create_group("data")
    for (xn in names(x)) {
      # subsubfile[[xn]] <- x[[xn]]

      # workaround
      .write_h5_vec(x[[xn]], subsubfile, xn)
    }
  } else if (is.atomic(x)) {
    hdf5r::h5attr(subfile, "object_class") <- "vector"
    if (!is.null(names(x))) subfile[["names"]] <- names(x)
    # subfile[["data"]] <- x

    # workaround
    .write_h5_vec(x, subfile, "data")

  } else if (is.list(x)) {
    hdf5r::h5attr(subfile, "object_class") <- "list"
    subfile[["class"]] <- class(x)
    if (!is.null(names(x))) subfile[["names"]] <- names(x)

    subsubfile <- subfile$create_group("data")

    if (is.null(names(x)) && length(x) > 0) names(x) <- paste0("elem", seq_along(x))

    for (xn in names(x)) {
      write_h5_(x[[xn]], subsubfile, xn)
    }
  } else {
    stop("Cannot write ", x)
  }
}




#' Tests whether hdf5 is correctly installed and can load/write data
#'
#' @param detailed Whether top do a detailed check
#'
#' @importFrom crayon red green bold
#' @importFrom stringr str_pad
#'
#' @export
test_h5_installation <- function(detailed = FALSE) {
  obj <- get_h5_test_data()
  file <- test_h5_installation_write(detailed, obj)
  obj2 <- test_h5_installation_read(detailed, file)
  test_h5_installation_equal(detailed, obj, obj2)

  if (detailed)
    message(crayon::green(crayon::bold(str_pad("\u2714 HDF5 test successful ", 90, side = "right", "-"))))

  TRUE
}

#' @rdname test_h5_installation
#' @importFrom stats runif
#' @export
get_h5_test_data <- function() {
  m <- matrix(1:20, ncol = 4, dimnames = list(letters[1:5], LETTERS[1:4]))

  obj <-
    list(
      charone = "a",
      charmany = c("one", "two", "three"),
      charnone = character(0),
      logicalone = TRUE,
      logicalnone = logical(0),
      even = c(one = FALSE, two = TRUE, three = FALSE),
      listone = list(a = 1, b = 2),
      listtwo = list(mat = matrix(1:10, ncol = 2), df = data.frame(a = 1, b = c(1, 2)), null = NULL),
      listmany = list(list(list())),
      df = data.frame(a = letters[1:4], b = runif(4), c = c(T, F, T, T), d = 2L:5L),
      mat = m,
      spmat = Matrix::Matrix(m, sparse = TRUE),
      null = NULL
    )
  class(obj) <- "tenten"

  obj
}

test_h5_installation_write <- function(detailed = FALSE, obj = get_h5_test_data(), file = tempfile()) {
  tryCatch(
    write_h5(obj, file),
    error = function(e) {
      paste0("\u274C Unable to write hdf5 files\n") %>%
        crayon::red() %>%
        cat()
      stop(e)
    }
  )
  if (detailed) message(crayon::green("\u2714 HDF5 files can be written"))

  file
}

test_h5_installation_read <- function(detailed = FALSE, file) {
  obj2 <- tryCatch(
    read_h5(file),
    error = function(e) {
      paste0("\u274C Unable to read hdf5 files\n") %>%
        crayon::red() %>%
        cat()
      stop(e)
    }
  )
  if (detailed) message(crayon::green("\u2714 HDF5 files can be read"))

  obj2
}


test_h5_installation_equal <- function(detailed = FALSE, obj, obj2) {
  if (!isTRUE(all.equal(obj2, obj, check.attributes = FALSE))) {
    paste0("\u274C R objects written and read through hdf5 are not the same\n") %>%
      crayon::red() %>%
      cat()
    stop()
  }
  if (detailed)
    message(crayon::green("\u2714 An R object that is written and read with HDF5 is the same"))
}

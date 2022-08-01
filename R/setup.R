# global reference to packages
# mtcnn <- NULL

.onLoad <- function(libname, pkgname) {
  # mtcnn <<- reticulate::import("mtcnn", delay_load = TRUE)
  
  reticulate::source_python('python/test.py')
}

#' Setup python packages
#'
#' @param envname The name, or full path, of the environment in which Python
#'   packages are to be installed. When NULL (the default), the active
#'   environment as set by the RETICULATE_PYTHON_ENV variable will be used; if
#'   that is unset, then the r-reticulate environment will be used.
#' @param method Installation method. By default, "auto" automatically finds a
#'   method that will work in the local environment. Change the default to force
#'   a specific installation method. Note that the "virtualenv" method is not
#'   available on Windows.
#' @param conda The path to a conda executable. Use "auto" to allow reticulate
#'   to automatically find an appropriate conda binary. See Finding Conda and
#'   conda_binary() for more details.
#'
#' @export
#' @import reticulate
install_py_packs <- function(envname = NULL, method = "auto", conda = "auto") {
  reticulate::py_install(c("mtcnn", 
                           "tensorflow",
                           "keras"),
                           envname = envname, method = method, conda = conda)
}

# create a new environment 
# conda_create("r-reticulate-36", python_version = '3.6')

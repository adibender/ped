tidyr_new_interface <- function() FALSE
.onLoad <- function(libname=find.package("ped"), pkgname="ped") {

  if (getRversion() >= "2.5.1") {

    utils::globalVariables(".")

    tidyr_new_interface <<- function() {
      utils::packageVersion("tidyr") > "0.8.99"
    }

  }



  invisible()

}

.onAttach <- function(libname=find.package("ped"), pkgname="ped") {

  # packageStartupMessage("")

  invisible()
}

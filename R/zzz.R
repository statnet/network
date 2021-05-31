######################################################################
#
# zzz.R
#
# Written by Carter T. Butts <buttsc@uci.edu>.
#
# Last Modified 11/30/19
# Licensed under the GNU General Public License version 2 (June, 1991)
# or greater
#
# Part of the R/network package
#
######################################################################

.onAttach <- function(libname, pkgname){
  #' @importFrom statnet.common statnetStartupMessage
  sm <- statnetStartupMessage("network", c("statnet","ergm","ergm.count","tergm"), TRUE)
  if(!is.null(sm)) packageStartupMessage(sm)
}

######################################################################
#
# zzz.R
#
# Written by Carter T. Butts <buttsc@uci.edu>; portions contributed by
# David Hunter <dhunter@stat.psu.edu> and Mark S. Handcock
# <handcock@u.washington.edu>.
#
# Last Modified 10/04/10
# Licensed under the GNU General Public License version 2 (June, 1991)
#
# Part of the R/network package
#
# .onLoad is run when the package is loaded with library(network)
#
######################################################################

.onLoad <- function(libname, pkgname){
  library.dynam("network", package=pkgname, lib.loc=libname)
    if(R.version$major=="1"){
     ehelp <- help(package="network")$info[[2]][[2]]
     cat(paste("'",ehelp[4],"'\n",
               "Version ",ehelp[2],
               " created on ",ehelp[3],".\n", sep=""))
    }else{
     ehelp <- help(package="network")$info[[1]]
     cat(paste(substring(ehelp[4],first=16),"\n",
               "Version ",substring(ehelp[2],first=16),
               " created on ",
                substring(ehelp[3],first=16),".\n", sep=""))
    }
    cat(paste("copyright (c) 2005, Carter T. Butts, University of California-Irvine\n",
"                    Mark S. Handcock, University of Washington\n",
"                    David R. Hunter, Penn State University\n",
"                    Martina Morris, University of Washington\n",sep=""))
    cat('For citation information, type citation("network").\n')
    cat('Type help("network-package") to get started.\n')
}

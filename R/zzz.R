######################################################################
#
# zzz.R
#
# Written by Carter T. Butts <buttsc@uci.edu>; portions contributed by
# David Hunter <dhunter@stat.psu.edu> and Mark S. Handcock
# <handcock@u.washington.edu>.
#
# Last Modified 8/12/05
# Licensed under the GNU General Public License version 2 (June, 1991)
#
# Part of the R/network package
#
# .First.lib is run when the package is loaded with library(network)
#
######################################################################

.First.lib <- function(lib, pkg){
   library.dynam("network", pkg, lib)
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
    cat('Type help(package="network") to get started.\n')
}

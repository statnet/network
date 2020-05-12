######################################################################
#
# misc.R
#
# Written by Carter T. Butts <buttsc@uci.edu>; portions contributed by
# David Hunter <dhunter@stat.psu.edu> and Mark S. Handcock
# <handcock@u.washington.edu>.
#
# Last Modified 02/26/13
# Licensed under the GNU General Public License version 2 (June, 1991)
# or greater
#
# Part of the R/network package
#
# This file contains various network routines which don't fit anywhere
# else (generally, utilities and the like).
#
# Contents:
#
#   is.discrete
#   is.discrete.character
#   is.discrete.numeric
#
######################################################################



#' Internal Network Package Functions
#' 
#' Internal network functions.
#' 
#' Most of these are not to be called by the user.
#' 
#' @name network-internal
#' 
#' @param x an object to be designated either discrete or continuous, or a
#' network.
#' @param y a network or something coercible to one.
#' @param \dots further arguments passed to or used by methods.
#' 
#' @seealso network
#' 
#' @keywords internal

#' @rdname network-internal
#' @export
is.discrete.numeric<-function(x){
 (is.numeric(x)|is.logical(x)) && mean(duplicated(x)) > 0.8
}

#' @rdname network-internal
#' @export
is.discrete.character<-function(x){
 (is.character(x)|is.logical(x)) && mean(duplicated(x)) > 0.8
}

#' @rdname network-internal
#' @export
is.discrete<-function(x){
 (is.numeric(x)|is.logical(x)|is.character(x)) && mean(duplicated(x)) > 0.8
}

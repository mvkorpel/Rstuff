### File colorbar.R
### Add colorbars to plots created with R, http://www.r-project.org/
### The document is in file colorbar.Rd. Compile a PDF by running
###   R CMD Rd2pdf colorbar.Rd
###
### Copyright 2013 Mikko Korpela
###
### This program is free software: you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation, either version 2 of the License, or
### (at your option) any later version.
###
### This program is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.
###
### You should have received a copy of the GNU General Public License
### along with this program.  If not, see <http://www.gnu.org/licenses/>.

colorbar <- function(x, y = NULL, col = palette(), labels = TRUE,
                     tick = !any(is.na(c(minlabel, maxlabel))) ||
                     !is.logical(labels),
                     horiz = "auto", minlabel = NA, maxlabel = NA,
                     nticks = 2, tickat = NULL, margin = rep(-0.03, 4),
                     longside = -0.6, shortside = -0.05,
                     axisloc = c("in", "out"), reverse = "auto",
                     xjust = 0.5, yjust = 0.5, labeljust = TRUE,
                     noaxissize = FALSE, totalsize = FALSE, ...) {

    if (is.character(x)) {
        xauto <- match.arg(x, c("bottomright", "bottom", "bottomleft",
                                "left", "topleft", "top", "topright",
                                "right", "center", "standalone"))
        autoxy <- TRUE
    } else {
        thepar <- par()
        xlog <- thepar$xlog
        ylog <- thepar$ylog
        xy <- xy.coords(x, y)
        xusr <- xy$x
        if (xlog) {
            if (any(xusr <= 0)) {
                stop("positive 'x' required")
            }
            xusr <- log10(xusr)
        }
        yusr <- xy$y
        if (ylog) {
            if (any(yusr <= 0)) {
                stop("positive 'y' required")
            }
            yusr <- log10(yusr)
        }
        nx <- length(xusr)
        if (nx < 1 || nx > 2) {
            stop("need one or two (x, y) points")
        }
        autoxy <- FALSE
    }
    standalone <- autoxy && xauto == "standalone"

    axisloc2 <- match.arg(axisloc)
    stopifnot(is.numeric(nticks), is.numeric(xjust), is.numeric(yjust),
              is.numeric(longside), is.numeric(shortside), is.numeric(margin),
              length(nticks) == 1, length(xjust) == 1, length(yjust) == 1,
              length(longside) == 1, length(shortside) == 1,
              length(margin) == 4,
              identical(reverse, TRUE) || identical(reverse, FALSE) ||
              identical(reverse, "auto"),
              identical(horiz, TRUE) || identical(horiz, FALSE) ||
              identical(horiz, "auto"),
              length(minlabel) == 1, length(maxlabel) == 1,
              is.null(tickat) || is.numeric(tickat),
              identical(labeljust, TRUE) || identical(labeljust, FALSE),
              identical(tick, TRUE) || identical(tick, FALSE),
              identical(noaxissize, TRUE) || identical(noaxissize, FALSE),
              identical(totalsize, TRUE) || identical(totalsize, FALSE),
              is.vector(col))

    ncols <- length(col)
    if (ncols == 0) {
        stop("at least one color needed")
    }
    if (standalone) {
        par(mai = rep(0, 4))
        margin2 <- margin
        ds <- dev.size(units = "in")
        for (i in c(1, 3)) {
            if (margin2[i] < 0) {
                margin2[i] <- -margin2[i]
            } else {
                margin2[i] <- margin2[i] / ds[2]
            }
        }
        for (i in c(2, 4)) {
            if (margin2[i] < 0) {
                margin2[i] <- -margin2[i]
            } else {
                margin2[i] <- margin2[i] / ds[1]
            }
        }
        par(omd = c(margin2[2], 1 - margin2[4], margin2[1], 1 - margin2[3]))
        plot(1:2, ann = FALSE, xaxt = "n", yaxt = "n", type = "n", bty = "n")
    }
    if (autoxy) {
        thepar <- par()
        xlog <- FALSE
        ylog <- FALSE
    }
    usr <- thepar$usr
    usrlin <- usr
    if (xlog) {
        usrlin[1:2] <- 10^usr[1:2]
    }
    if (ylog) {
        usrlin[3:4] <- 10^usr[3:4]
    }
    usrwidth <- usr[2] - usr[1]
    usrwidthlin <- usrlin[2] - usrlin[1]
    usrheight <- usr[4] - usr[3]
    usrheightlin <- usrlin[4] - usrlin[3]

    if (autoxy && horiz == "auto") {
        horiz2 <- xauto %in% c("top", "bottom")
    } else if (horiz == "auto") {
        horiz2 <- FALSE
    } else {
        horiz2 <- horiz
    }
    if (reverse == "auto") {
        reverse2 <- FALSE
    } else {
        reverse2 <- reverse
    }
    pin <- thepar$pin
    ## Negative numbers in longside, shortside are proportional,
    ## positive numbers are inches
    if (standalone) {
        if (horiz2) {
            usrlong <- usrwidth
            usrshort <- usrheight
        } else {
            usrlong <- usrheight
            usrshort <- usrwidth
        }
        xauto <- "center"
    } else {
        if (longside < 0) {
            longside2 <- -longside
        } else if (horiz2) {
            longside2 <- longside / pin[1]
        } else {
            longside2 <- longside / pin[2]
        }
        if (shortside < 0) {
            shortside2 <- -shortside
        } else if (horiz2) {
            shortside2 <- shortside / pin[2]
        } else {
            shortside2 <- shortside / pin[1]
        }
        if (!autoxy && nx == 2) {
            xdiff <- abs(xusr[2] - xusr[1])
            ydiff <- abs(yusr[2] - yusr[1])
            if (horiz == "auto") {
                xdiffinches <- xdiff / usrwidth * pin[1]
                ydiffinches <- ydiff / usrheight * pin[2]
                horiz2 <- xdiffinches > ydiffinches
                if (longside >= 0) {
                    if (horiz2) {
                        longside2 <- longside / pin[1]
                    } else {
                        longside2 <- longside / pin[2]
                    }
                }
                if (shortside >= 0) {
                    if (horiz2) {
                        shortside2 <- shortside / pin[2]
                    } else {
                        shortside2 <- shortside / pin[1]
                    }
                }
            }
            if (horiz2) {
                usrlong <- xdiff
                if (reverse == "auto") {
                    reverse2 <- xusr[2] < xusr[1]
                }
            } else {
                usrlong <- ydiff
                if (reverse == "auto") {
                    reverse2 <- yusr[2] < yusr[1]
                }
            }
        } else {
            if (horiz2) {
                usrlong <- longside2 * usrwidth
                usrshort <- shortside2 * usrheight
            } else {
                usrlong <- longside2 * usrheight
                usrshort <- shortside2 * usrwidth
            }
        }
    }

    ## Tick labels and their locations (values from 0.5 to ncols + 0.5)
    if (tick) {
        ## According to ?par,
        ##(1) 'par("cxy") is par("cin")/par("pin") scaled to user coordinates.'
        ## That would make:
        ##(2) cxy <- thepar$cin[2] / pin[2] * usrheightlin
        ## Also from ?par,
        ##(3) par("csi") is 'The same as par("cin")[2]'.
        ## So, the equivalent of (1) is:
        cxy <- thepar$csi / pin[2] * usrheightlin
        ## ... which seems to work better than using par("cxy") or
        ## (1), because apparently the equivalence in (3) doesn't
        ## always hold. Example (R trunk r61876):
        ## > x11()
        ## > plot(1:5)
        ## > par(cex=2)
        ## > par1 <- par()
        ## > par1$csi == par1$cin[2]
        ## [1] TRUE
        ## > x11()
        ## > par(cex=2)
        ## > plot(1:5)
        ## > par2 <- par()
        ## > par2$csi == par2$cin[2]
        ## [1] FALSE
        axisArg <- list(...)
        mgp <- axisArg[["mgp"]]
        if (is.null(mgp)) {
            mgp <- thepar$mgp
        }
        tcl <- axisArg[["tcl"]]
        if (is.null(tcl)) {
            tcl <- thepar$tcl
        }
        cex.axis <- axisArg[["cex.axis"]]
        if (is.null(cex.axis)) {
            cex.axis <- 1
        }
        las <- axisArg[["las"]]
        if (is.null(las)) {
            las <- thepar$las
        }

        if (is.logical(labels)) {
            if (isTRUE(labels) && nticks >= 2 &&
                is.numeric(minlabel) && is.numeric(maxlabel) &&
                is.finite(minlabel) && is.finite(maxlabel) &&
                maxlabel > minlabel) {

                labels2 <- pretty(x = c(minlabel, maxlabel), n = nticks)
                tickat2 <- (labels2 - minlabel) / (maxlabel - minlabel) *
                    (ncols - 1) + 1
                keepers <- tickat2 >= 0.5 & tickat2 <= ncols + 0.5
                tickat2 <- tickat2[keepers]
                labels2 <- labels2[keepers]
            } else if (nticks >= 2 && is.null(tickat)) {
                labels2 <- FALSE
                tickat2 <- seq(from = 1, to = ncols, length.out = nticks)
            } else if (!is.null(tickat)) {
                labels2 <- FALSE
                tickat2 <- tickat[tickat >= 0.5 & tickat <= ncols + 0.5]
            } else {
                labels2 <- FALSE
                tickat2 <- NULL
            }
        } else {
            if (length(tickat) != length(labels)) {
                stop("if 'labels' is not logical, 'at' and 'labels' must be of the same length")
            }
            keepers <- tickat >= 0.5 & tickat <= ncols + 0.5
            tickat2 <- tickat[keepers]
            labels2 <- labels[keepers]
        }

        ## Approximate space requirement of axis.
        ## May go wrong if axis() drops labels.
        if (!is.null(tickat2)) {
            ## Convert from linear scale horizontal or vertical (dirin)
            ## distance to log or linear scale (depending on xlog, ylog)
            ## horizontal or vertical (dirout) distance.  If propin is
            ## TRUE, the input 'd' is assumed to be proportional to plot
            ## dimensions, otherwise an actual distance.
            convdist <- function(d, dirin, dirout, propin=FALSE) {
                ## 1. distance proportional to input axis
                if (propin) {
                    dout <- d
                } else if (dirin == "h") {
                    dout <- d / usrwidthlin
                } else {
                    dout <- d / usrheightlin
                }
                ## 2. distance proportional to output axis
                if (dirin == "h" && dirout == "v") {
                    dout <- dout * pin[1] / pin[2]
                } else if (dirin == "v" && dirout == "h") {
                    dout <- dout / pin[1] * pin[2]
                }
                ## 3. native distance along output axis
                if (dirout == "h") {
                    dout * usrwidth
                } else {
                    dout * usrheight
                }
            }

            if (horiz2) {
                cxy <- convdist(cxy, "v", "v")
            } else {
                cxy <- convdist(cxy, "v", "h")
            }
            if (!identical(labels2, FALSE)) {
                if ((horiz2 && las %in% c(0, 1)) ||
                    (!horiz2 && las %in% c(0, 3))) {
                    labelsizes <- vapply(labels2, strheight, 1,
                                         units = "inches",
                                         cex = cex.axis,
                                         USE.NAMES=FALSE)
                    maxsize <- max(labelsizes) / pin[2]
                    if (horiz2) {
                        maxsize <- convdist(maxsize, "v", "v", TRUE)
                    } else {
                        maxsize <- convdist(maxsize, "v", "h", TRUE)
                    }
                    axissize <- (mgp[2] * thepar$mex + 1) * cxy + maxsize
                } else {
                    labelsizes <- vapply(labels2, strwidth, 1,
                                         units = "inches",
                                         cex = cex.axis,
                                         USE.NAMES=FALSE)
                    maxsize <- max(labelsizes) / pin[1]
                    if (horiz2) {
                        maxsize <- convdist(maxsize, "h", "v", TRUE)
                    } else {
                        maxsize <- convdist(maxsize, "h", "h", TRUE)
                    }
                    axissize <- mgp[2] * thepar$mex * cxy + maxsize
                }
            } else {
                axissize <- max(0, -tcl) * cxy
            }
        } else {
            axissize <- 0
        }
    } else {
        tickat2 <- NULL
        axissize <- 0
    }
    ## Record true axissize for computing xleft, ybottom, xright, ytop
    trueaxissize <- axissize
    if (noaxissize) {
        axissize <- 0
    }
    ## Ensure that axis and labels fit in the standalone picture
    if (standalone || ((autoxy || nx != 2) && totalsize)) {
        usrshort <- max(0, usrshort - axissize)
    }

    ## (Log) user coordinates for colorbar
    axisside <- NULL
    if (autoxy) {
        margin2 <- margin
        for (i in c(1, 3)) {
            if (margin2[i] < 0) {
                margin2[i] <- -margin2[i]
            } else {
                margin2[i] <- margin2[i] / pin[2]
            }
        }
        for (i in c(2, 4)) {
            if (margin2[i] < 0) {
                margin2[i] <- -margin2[i]
            } else {
                margin2[i] <- margin2[i] / pin[1]
            }
        }
        if (horiz2) {
            if (grepl("right", xauto, fixed = TRUE)) {
                seqstart <- usr[2] - margin2[4] * usrwidth - usrlong
            } else if (grepl("left", xauto, fixed = TRUE)) {
                seqstart <- usr[1] + margin2[2] * usrwidth
            } else {
                seqstart <- usr[1] + 0.5 * (usrwidth - usrlong)
            }
            if (grepl("bottom", xauto, fixed = TRUE)) {
                y0 <- usr[3] + margin2[1] * usrheight +
                    axissize * (axisloc2 == "out")
                y1 <- y0 + usrshort
            } else if (grepl("top", xauto, fixed = TRUE)) {
                y1 <- usr[4] - margin2[3] * usrheight -
                    axissize * (axisloc2 == "out")
                y0 <- y1 - usrshort
                ## Top: out means top side
                if (axisloc2 == "out") {
                    axisside <- 3
                } else {
                    axisside <- 1
                }
            } else {
                y0 <- usr[3] + 0.5 * (usrheight - usrshort - axissize)
                if (axisloc2 == "out") {
                    y0 <- y0 + axissize
                }
                y1 <- y0 + usrshort
            }
            if (is.null(axisside)) {
                ## Bottom or middle (default): out means bottom side
                if (axisloc2 == "out") {
                    axisside <- 1
                } else {
                    axisside <- 3
                }
            }
        } else { # vertical colorbar
            if (grepl("right", xauto, fixed = TRUE)) {
                x1 <- usr[2] - margin2[4] * usrwidth -
                    axissize * (axisloc2 == "out")
                x0 <- x1 - usrshort
            } else if (grepl("left", xauto, fixed = TRUE)) {
                x0 <- usr[1] + margin2[2] * usrwidth +
                    axissize * (axisloc2 == "out")
                x1 <- x0 + usrshort
                ## Left: out means left side
                if (axisloc2 == "out") {
                    axisside <- 2
                } else {
                    axisside <- 4
                }
            } else {
                x0 <- usr[1] + 0.5 * (usrwidth - usrshort - axissize)
                if (axisloc2 == "in") {
                    x0 <- x0 + axissize
                }
                x1 <- x0 + usrshort
            }
            if (is.null(axisside)) {
                ## Right or center (default): out means right side
                if (axisloc2 == "out") {
                    axisside <- 4
                } else {
                    axisside <- 2
                }
            }
            if (grepl("bottom", xauto, fixed = TRUE)) {
                seqstart <- usr[3] + margin2[1] * usrheight
            } else if (grepl("top", xauto, fixed = TRUE)) {
                seqstart <- usr[4] - margin2[3] * usrheight - usrlong
            } else {
                seqstart <- usr[3] + 0.5 * (usrheight - usrlong)
            }
        }
    } else if (horiz2) { # horizontal, numeric location
        if (!autoxy && nx == 2) {
            seqstart <- min(xusr)
            y0 <- min(yusr)
            y1 <- max(yusr)
        } else {
            seqstart <- xusr - xjust * usrlong
            y0 <- yusr - yjust * usrshort
            y1 <- y0 + usrshort
        }
        ymid <- (y0 + y1) / 2
        usrmid <- (usr[3] + usr[4]) / 2
        ## Bottom half or middle: out means bottom side
        if ((axisloc2 == "out" && ymid <= usrmid) ||
            (axisloc2 == "in" && ymid > usrmid)) {
            axisside <- 1
            adjustment <- axissize - yjust * axissize
        } else {
            axisside <- 3
            adjustment <- -yjust * axissize
        }
        if (!autoxy && nx == 2) {
            if (axisside == 1) {
                y0 <- min(y0 + axissize, y1)
            } else {
                y1 <- max(y1 - axissize, y0)
            }
        } else {
            y0 <- y0 + adjustment
            y1 <- y1 + adjustment
        }
    } else { # vertical, numeric location
        if (!autoxy && nx == 2) {
            seqstart <- min(yusr)
            x0 <- min(xusr)
            x1 <- max(xusr)
        } else {
            seqstart <- yusr + usrlong - yjust * usrlong - usrlong
            x0 <- xusr - xjust * usrshort
            x1 <- x0 + usrshort
        }
        xmid <- (x0 + x1) / 2
        usrmid <- (usr[1] + usr[2]) / 2
        ## Right half or center: out means right side
        if ((axisloc2 == "out" && xmid >= usrmid) ||
            (axisloc2 == "in" && xmid < usrmid)) {
            axisside <- 4
            adjustment <- -xjust * axissize
        } else {
            axisside <- 2
            adjustment <- axissize - xjust * axissize
        }
        if (!autoxy && nx == 2) {
            if (axisside == 4) {
                x1 <- max(x1 - axissize, x0)
            } else {
                x0 <- min(x0 + axissize, x1)
            }
        } else {
            x0 <- x0 + adjustment
            x1 <- x1 + adjustment
        }
    }
    seqpoints <- seq(from = seqstart, to = seqstart + usrlong,
                     length.out = ncols + 1)
    slice <- usrlong / ncols

    ## - Reverse colors and tick locations if required
    ## - Convert tick locations from (0.5, ncols+0.5) to (log) user coordinates
    if (reverse2) {
        col2 <- rev(col)
        if (!is.null(tickat2)) {
            tickat2 <- seqstart + usrlong + slice * 0.5 - tickat2 * slice
        }
    } else {
        col2 <- col
        if (!is.null(tickat2)) {
            tickat2 <- seqstart - slice * 0.5 + tickat2 * slice
        }
    }

    ## Draw colorbar
    if (horiz2) {
        if (axisside == 1) {
            axispos <- y0
            ybottom <- y0 - trueaxissize
            ytop <- y1
        } else {
            axispos <- y1
            ybottom <- y0
            ytop <- y1 + trueaxissize
        }
        if (xlog) {
            seqpoints <- 10^seqpoints
            if (!is.null(tickat2)) {
                tickat2 <- 10^tickat2
            }
        }
        if (ylog) {
            axispos <- 10^axispos
            y0 <- 10^y0
            y1 <- 10^y1
            ybottom <- 10^ybottom
            ytop <- 10^ytop
        }
        xleft <- seqpoints[1]
        xright <- seqpoints[ncols + 1]
        rect(seqpoints[-(ncols+1)], y0, seqpoints[-1], y1,
             col = col2, border=NA)
    } else {
        if (axisside == 2) {
            axispos <- x0
            xleft <- x0 - trueaxissize
            xright <- x1
        } else {
            axispos <- x1
            xleft <- x0
            xright <- x1 + trueaxissize
        }
        if (ylog) {
            seqpoints <- 10^seqpoints
            if (!is.null(tickat2)) {
                tickat2 <- 10^tickat2
            }
        }
        if (xlog) {
            axispos <- 10^axispos
            x0 <- 10^x0
            x1 <- 10^x1
            xleft <- 10^xleft
            xright <- 10^xright
        }
        ybottom <- seqpoints[1]
        ytop <- seqpoints[ncols + 1]
        rect(x0, seqpoints[-(ncols+1)], x1, seqpoints[-1],
             col = col2, border=NA)
    }

    ## Draw axis
    if (!is.null(tickat2)) {
        axisArg[["side"]] <- axisside
        axisArg[["at"]] <- tickat2
        axisArg[["pos"]] <- axispos
        if (!identical(labels2, FALSE) && labeljust) {
            if ((axisside == 1 && las %in% c(0, 1)) ||
                (axisside == 4 && las %in% c(0, 3))) {
                axisArg[["padj"]] <- 1
                mgp[2] <- mgp[2] - 0.5
                axisArg[["mgp"]] <- mgp
            } else if ((axisside == 2 && las %in% c(0, 3)) ||
                       (axisside == 3 && las %in% c(0, 1))) {
                axisArg[["padj"]] <- 0
            }
        }
        axisArg[["labels"]] <- labels2
        do.call(axis, axisArg)
    }
    invisible(c(xleft = xleft, ybottom = ybottom,
                xright = xright, ytop = ytop))
}

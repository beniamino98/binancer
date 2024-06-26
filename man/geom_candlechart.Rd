% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_candlestick.R
\name{geom_candlechart}
\alias{geom_candlechart}
\title{Candlestick plot}
\usage{
geom_candlechart(mapping = NULL, 
                 data = NULL, 
                 stat = "candle", 
                 position = "identity", 
                 linejoin = "mitre",..., 
                 na.rm = FALSE, 
                 show.legend = NA, 
                 bargap = 6, 
                 method = "candle",
                 col_up = "green", 
                 col_dw = "red", 
                 inherit.aes = TRUE)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{stat}{The statistical transformation to use on the data for this layer, either as a
ggproto Geom subclass or as a string naming the stat stripped of the stat_ prefix
(e.g. "candle" rather than "stat_candle" or "heikin_ashi" rather than "stat_heikin_ashi")}

\item{position}{Position adjustment, either as a string naming the adjustment
(e.g. \code{"jitter"} to use \code{position_jitter}), or the result of a call to a
position adjustment function. Use the latter if you need to change the
settings of the adjustment.}

\item{linejoin}{Line join style (round, mitre, bevel).}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{bargap}{Numeric, positive number to regulate the distance between candles.
Increasing the \code{"bargap"} reduce the distance between candles. Default is \code{6}.}

\item{col_up}{Character, color of the candle when open price is greater than close price.}

\item{col_dw}{Character, color of the candle when open price is lower than close price.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}
}
\description{
Candlestick plot for all time frames within the ggplot2 framework
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_flex_query.R
\name{fetch_flex_query}
\alias{fetch_flex_query}
\title{Fetches the specified flex query from IBKR}
\usage{
fetch_flex_query(q_id)
}
\description{
Operates in sync; will wait until it gets its result (5 min max). You have
to first save a flex query in client portal and get its ID.
}

\name{xo.priceobjective.processor}
\alias{xo.priceobjective.processor}
\title{This function adds Vertical Price Objectives calculated with the
Bullish Breakout and Bearish Breakdown Method (BM) to an P&F Table.}
\usage{
  xo.priceobjective.processor(data, reversal, boxsize, log)
}
\description{
  Finding the appropriate price objectives is explained
  very good at
  \url{http://stockcharts.com/school/doku.php?id=chart_school:chart_analysis:point_and_figure_pri}.
  The function adds columns vpo_bm_boxnumber and
  vpo_bm_price to the given P&F Table. vpo_bm_bonumber
  contains the boxnumber of the price objective, while
  vpo_bm_price contains the real price objective.
}
\seealso{
  \url{http://stockcharts.com/school/doku.php?id=chart_school:chart_analysis:point_and_figure_pri}
}


# Fully Worked Scoring & Ranking Example

# An error occurred.

Unable to execute JavaScript.

If you’ve made it to this page, you’re looking to understand how we
assign rankings to each trader to determine a winner. In order to
proceed, you should have already read and understood the [General Idea
behind our scoring
philosophy](https://gothic-hedge-society.github.io/fintech.trading.competition/articles/Scoring.html)
to get an idea of what we’re seeking to do; to wit: reward those traders
who make a lot of money, but do so intelligently rather than just
getting lucky.

The process described below is performed daily when standings are
updated.

## Step 1: START

For every participaiting trader, every day as soon as new end-of-day NAV
values become available, we calculate excess return and Sharpe ratio as
described in [Sharpe Ratio
Calculation](https://gothic-hedge-society.github.io/fintech.trading.competition/articles/Sharpe%20Ratio%20Calculation.html).

Let’s suppose that we have 50 traders in the competition and on this
particular day we run the query, do the calculations, and end up with
the following stats:

## Step 1: Sort by excess return

The first thing we do is arrange everyone in the table by decreasing
excess return. Highest return goes at the top and the lowest goes to the
bottom, leaving us with:

## Step 2: Break the traders down into brackets of 10

Prizes are awarded to the top 5 traders, so we form brackets of 10
traders each (=5\*2) based on excess return. That leaves us with the
below, where each page of the datatable is a single bracket:

### Step 3: Sort by Sharpe within brackets

Each individual group of 10 traders is sorted with highest Sharpe at the
top, lowest at the bottom.

## Step 4: Assign bracket ranks

Everyone is given a rank within their bracket:

## Step 5: Recombine into one table and assign overall trader rank

This determines overall rank for the competition:

## Conclusion

Traders 18, 16, 11, 36, and 10 take the top 5 spots in the competition
overall.

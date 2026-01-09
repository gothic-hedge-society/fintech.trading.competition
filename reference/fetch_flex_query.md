# Fetches the specified flex query from IBKR

Operates in sync; will wait until it gets its result (5 min max). You
have to first save a flex query in client portal and get its ID.

## Usage

``` r
fetch_flex_query(q_id)
```

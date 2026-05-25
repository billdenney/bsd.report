# Create new columns in a data.frame with sprintf results

Create new columns in a data.frame with sprintf results

## Usage

``` r
sprintf_data_frame(data, ..., factor_out_if_factor_in = TRUE, ordered = NULL)

sprintf_data_frame_single(
  data,
  format,
  factor_out_if_factor_in = TRUE,
  ordered = NULL
)
```

## Arguments

- data:

  the data to use for formatting

- ...:

  a named list of character vectors. Names are new columns for `data`,
  and values are sent to `format` in `sprintf_data_frame_single`.

- factor_out_if_factor_in:

  If any of the input columns are factors, make the output column a
  factor in the same order as the input column factors

- ordered:

  If `factor_out_if_factor_in` converts the output to a factor, pass to
  [`base::factor`](https://rdrr.io/r/base/factor.html). If `NULL`, then
  it is set to `TRUE` if any of the input columns are ordered factors.

- format:

  A named character vector where the names are column names in `data`
  and the values are sprintf format strings for the column.

## Value

The data frame with columns added for the names of `...`.

A character vector with one element per row of `data`.

## Functions

- `sprintf_data_frame_single()`: Generate a character vector based on
  sprintf input formats

## Examples

``` r
sprintf_data_frame(
  data=mtcars,
  cyl_mpg=c(mpg="%g miles/gallon, ", cyl="%g cylinders"),
  disp_hp=c(disp="%g cu.in. displacement, ", hp="%g hp")
)
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
#>                                            cyl_mpg
#> Mazda RX4             21 miles/gallon, 6 cylinders
#> Mazda RX4 Wag         21 miles/gallon, 6 cylinders
#> Datsun 710          22.8 miles/gallon, 4 cylinders
#> Hornet 4 Drive      21.4 miles/gallon, 6 cylinders
#> Hornet Sportabout   18.7 miles/gallon, 8 cylinders
#> Valiant             18.1 miles/gallon, 6 cylinders
#> Duster 360          14.3 miles/gallon, 8 cylinders
#> Merc 240D           24.4 miles/gallon, 4 cylinders
#> Merc 230            22.8 miles/gallon, 4 cylinders
#> Merc 280            19.2 miles/gallon, 6 cylinders
#> Merc 280C           17.8 miles/gallon, 6 cylinders
#> Merc 450SE          16.4 miles/gallon, 8 cylinders
#> Merc 450SL          17.3 miles/gallon, 8 cylinders
#> Merc 450SLC         15.2 miles/gallon, 8 cylinders
#> Cadillac Fleetwood  10.4 miles/gallon, 8 cylinders
#> Lincoln Continental 10.4 miles/gallon, 8 cylinders
#> Chrysler Imperial   14.7 miles/gallon, 8 cylinders
#> Fiat 128            32.4 miles/gallon, 4 cylinders
#> Honda Civic         30.4 miles/gallon, 4 cylinders
#> Toyota Corolla      33.9 miles/gallon, 4 cylinders
#> Toyota Corona       21.5 miles/gallon, 4 cylinders
#> Dodge Challenger    15.5 miles/gallon, 8 cylinders
#> AMC Javelin         15.2 miles/gallon, 8 cylinders
#> Camaro Z28          13.3 miles/gallon, 8 cylinders
#> Pontiac Firebird    19.2 miles/gallon, 8 cylinders
#> Fiat X1-9           27.3 miles/gallon, 4 cylinders
#> Porsche 914-2         26 miles/gallon, 4 cylinders
#> Lotus Europa        30.4 miles/gallon, 4 cylinders
#> Ford Pantera L      15.8 miles/gallon, 8 cylinders
#> Ferrari Dino        19.7 miles/gallon, 6 cylinders
#> Maserati Bora         15 miles/gallon, 8 cylinders
#> Volvo 142E          21.4 miles/gallon, 4 cylinders
#>                                               disp_hp
#> Mazda RX4             160 cu.in. displacement, 110 hp
#> Mazda RX4 Wag         160 cu.in. displacement, 110 hp
#> Datsun 710             108 cu.in. displacement, 93 hp
#> Hornet 4 Drive        258 cu.in. displacement, 110 hp
#> Hornet Sportabout     360 cu.in. displacement, 175 hp
#> Valiant               225 cu.in. displacement, 105 hp
#> Duster 360            360 cu.in. displacement, 245 hp
#> Merc 240D            146.7 cu.in. displacement, 62 hp
#> Merc 230             140.8 cu.in. displacement, 95 hp
#> Merc 280            167.6 cu.in. displacement, 123 hp
#> Merc 280C           167.6 cu.in. displacement, 123 hp
#> Merc 450SE          275.8 cu.in. displacement, 180 hp
#> Merc 450SL          275.8 cu.in. displacement, 180 hp
#> Merc 450SLC         275.8 cu.in. displacement, 180 hp
#> Cadillac Fleetwood    472 cu.in. displacement, 205 hp
#> Lincoln Continental   460 cu.in. displacement, 215 hp
#> Chrysler Imperial     440 cu.in. displacement, 230 hp
#> Fiat 128              78.7 cu.in. displacement, 66 hp
#> Honda Civic           75.7 cu.in. displacement, 52 hp
#> Toyota Corolla        71.1 cu.in. displacement, 65 hp
#> Toyota Corona        120.1 cu.in. displacement, 97 hp
#> Dodge Challenger      318 cu.in. displacement, 150 hp
#> AMC Javelin           304 cu.in. displacement, 150 hp
#> Camaro Z28            350 cu.in. displacement, 245 hp
#> Pontiac Firebird      400 cu.in. displacement, 175 hp
#> Fiat X1-9               79 cu.in. displacement, 66 hp
#> Porsche 914-2        120.3 cu.in. displacement, 91 hp
#> Lotus Europa         95.1 cu.in. displacement, 113 hp
#> Ford Pantera L        351 cu.in. displacement, 264 hp
#> Ferrari Dino          145 cu.in. displacement, 175 hp
#> Maserati Bora         301 cu.in. displacement, 335 hp
#> Volvo 142E            121 cu.in. displacement, 109 hp
sprintf_data_frame_single(
  data=mtcars,
  format=c(mpg="%g miles/gallon, ", cyl="%g cylinders")
)
#>  [1] "21 miles/gallon, 6 cylinders"   "21 miles/gallon, 6 cylinders"  
#>  [3] "22.8 miles/gallon, 4 cylinders" "21.4 miles/gallon, 6 cylinders"
#>  [5] "18.7 miles/gallon, 8 cylinders" "18.1 miles/gallon, 6 cylinders"
#>  [7] "14.3 miles/gallon, 8 cylinders" "24.4 miles/gallon, 4 cylinders"
#>  [9] "22.8 miles/gallon, 4 cylinders" "19.2 miles/gallon, 6 cylinders"
#> [11] "17.8 miles/gallon, 6 cylinders" "16.4 miles/gallon, 8 cylinders"
#> [13] "17.3 miles/gallon, 8 cylinders" "15.2 miles/gallon, 8 cylinders"
#> [15] "10.4 miles/gallon, 8 cylinders" "10.4 miles/gallon, 8 cylinders"
#> [17] "14.7 miles/gallon, 8 cylinders" "32.4 miles/gallon, 4 cylinders"
#> [19] "30.4 miles/gallon, 4 cylinders" "33.9 miles/gallon, 4 cylinders"
#> [21] "21.5 miles/gallon, 4 cylinders" "15.5 miles/gallon, 8 cylinders"
#> [23] "15.2 miles/gallon, 8 cylinders" "13.3 miles/gallon, 8 cylinders"
#> [25] "19.2 miles/gallon, 8 cylinders" "27.3 miles/gallon, 4 cylinders"
#> [27] "26 miles/gallon, 4 cylinders"   "30.4 miles/gallon, 4 cylinders"
#> [29] "15.8 miles/gallon, 8 cylinders" "19.7 miles/gallon, 6 cylinders"
#> [31] "15 miles/gallon, 8 cylinders"   "21.4 miles/gallon, 4 cylinders"
```

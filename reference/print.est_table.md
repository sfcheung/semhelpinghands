# Print an 'est_table' Object

Print method for an 'est_table' object

## Usage

``` r
# S3 method for class 'est_table'
print(x, ..., nd = 3, empty_cells = "--", group_first = FALSE)
```

## Arguments

- x:

  Object of the class `est_table`.

- ...:

  Optional arguments to be passed to
  [`print()`](https://rdrr.io/r/base/print.html) methods.

- nd:

  The number of digits to be printed. Default is 3. (Scientific notation
  will never be used.)

- empty_cells:

  String to be printed for empty cells or cells with no values. Default
  is `"--"`.

- group_first:

  Not used.

## Value

`x` is returned invisibly. Called for its side effect.

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

# drhutools 0.1.2

- `folderSystem` create the folder system automatically at the project's root directory.

# drhutools 0.1.0

The initial release of the package.


# drhutools 0.1.1

- Removed the useless functions:
  - `textWrapper` has been superseded by [`ggfittext`](https://github.com/wilkox/ggfittext) functions.
  - `fitPage` has been deprecated because with the recent update of `gt` package, it support word output well. Dr. Hu's team lean on `gt` tables.
  - `addLine` has been deprecated. The recent version of `modelsummary` has a convenient build-in function[ `add_rows`](https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html#add_rows) to add custom lines into model summary tables. Together with the "data.frame" type of output, users can easily locate and add lines at the position they want.
- `ksplot` is superseded by the `cdplot` function
- Added the `folderSystem` function
- Clean the document

# Feed Similarity Calculator

A Shiny application for screening feed ingredients by nutritional
similarity, using standardized Euclidean distance.

For each nutrient *j*, values are standardized as
`z = (x - mean_j) / sd_j` with parameters estimated from the full dataset.
The distance between a candidate feed *i* and a reference feed *r* is
`d(i, r) = sqrt(sum_j (z_ij - z_rj)^2)`. Only candidates with complete values
for every selected nutrient are ranked; the reference itself is excluded, and
ties share a rank. The app also reports excluded candidates and per-nutrient
contributions to the distance.

**Similarity is not proof of safe substitution.** Rankings describe
compositional closeness only. Candidates must still be checked for
antinutritional factors, digestibility, amino acid balance, palatability,
inclusion limits, and regulatory status.

## Quick start

You do not need to know Git or GitHub. Install [R](https://cran.r-project.org/)
(and optionally [RStudio](https://posit.co/download/rstudio-desktop/)), open the
R console, and paste these two commands:

```r
install.packages(c("shiny", "reactable", "shinyWidgets", "shinycssloaders",
                   "readxl"))
shiny::runGitHub("feed-similarity", "YoungjunNa")
```

The first command installs the required packages (needed only once).
The second command downloads this repository and opens the app in your
web browser. The app starts immediately with the bundled demonstration
datasets. A notice in the sidebar shows which dataset is in use.

If you have downloaded the repository manually (e.g., with the green
"Code > Download ZIP" button), unzip it and run:

```r
shiny::runApp("path/to/feed-similarity")
```

## Data

This repository ships two **synthetic demonstration datasets**:

- `data/demo_database_1.xlsx`
- `data/demo_database_2.xlsx`

The values are synthetic (randomly perturbed) and are intended only to
demonstrate the application. They must not be cited as real feed
composition values, and results obtained with them will differ from
results based on real data.

### Using your own data

Save your feed composition table as an Excel file with one row per feed,
a character `feed` column, and numeric nutrient columns. See `feed_files`
in `R/feed_similarity.R` for the expected file names under `data/`. When
such a file is present, it is used automatically instead of the demo file.

## Repository layout

| Path | Purpose |
|---|---|
| `app.R` | Shiny application |
| `R/feed_similarity.R` | Engine: data loading, validation, standardization, distance, ranking |
| `data/` | Synthetic demonstration datasets |

## Citation

A manuscript describing the method is in preparation
(Na, Y., and Y. Choi). Until it is published, please cite this repository.

## License

MIT (see `LICENSE`).

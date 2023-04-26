# Shiny app: plot mi-hd results

[![DOI: coming soon]]()

You can interact with the results from the study [mi-hd](<https://arxiv.org/abs/2208.13656>) by:

- Installing this app locally as an R package:

    - Install from GitHub:

        ```
        devtools::install_github("https://github.com/EdoardoCostantini/mihd.results")
        ```

    - Install from Zenodo version:
    
    - Install the package from the local unzipped folder

            ```
            install.packages(
                "path to folder with the package",
                repos = NULL,
                type = "source"
            )
            ```

## Plots

To start the shiny apps and interact with the plots, open an R session and load the package:

```
library("mihd.results")
```

### Main simulation study results

Coming soon

### Collinearity simulation study results 

#### Understanding MI-PCA behavior

Run the following command in the R console:

```
mihd.results::plot_colli_mipca()
```

You can then click on the tabs in the `Interpretation` panel to understand the plots.

### EVS resampling study results

Coming soon

### Convergence plots

Coming soon
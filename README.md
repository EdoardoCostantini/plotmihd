# Shiny app: plot mi-hd results

[![DOI: coming soon]]()

This Shiny app allows you to interact with the results in the study [mi-hd](<https://arxiv.org/abs/2208.13656>).

## 1. Installing the Shiny app as an R package

### 1.1 Install from GitHub

- Open an R session and run the following command:

```
devtools::install_github("https://github.com/EdoardoCostantini/mihd.results")
```

### 1.2 Install from a local folder

1. Download the package from GitHub or Zendo.

2. Unzip the package.
    
3. Open an R session, and run the following command.

    ```
    install.packages(
        "path to the folder containing the package",
        repos = NULL,
        type = "source"
    )
    ```

    For example, on a Windows computer, this could be

    ```
    install.packages(
        "path to the folder containing the package",
        repos = NULL,
        type = "source"
    )
    ```

## 2. Using the Shiny app

To start the shiny apps and interact with the plots, open an R session and load the package:

```
library("mihd.results")
```

Then, run the following command in the R console:

```
start_app()
```

The app interface will explain how to interact with it.
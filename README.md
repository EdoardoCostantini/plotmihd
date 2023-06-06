# Shiny app: plot mi-hd results

This Shiny app allows you to interact with the results in the study ["High-dimensional imputation for the social sciences: a comparison of state-of-the-art methods"](<https://arxiv.org/abs/anonymized>).

## 1. Installing the Shiny app as an R package

You can install the Shiny app from the local zipped folder by:

1. Download zipped directory called "mihd.results".

2. Unzip the folder.
    
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
        "C:/Users/Name/Downloads/mihd.results/",
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
# partial.plot: An R package to visualize partial relationships

This package provides standard way to visualize partial predicted
relationship by several statistical/machine learning methods.
Currently, `lm()`, `glm()`, `glm.nb()`, `lme()`, `lmer()`, `glmer()`, `glmer.nb()`, `glmmadmb()`, `MCMCglmm()`, `cforest()`, `ctree()`, `svm()`, `randomForest()`, `ranger()`, `rpart()`, `tree()` are supported.
Handling of models are based on
[model.adapter](https://github.com/Marchen/model.adapter) class.

To install the package, please copy&paste following code into your R terminal.
If your don't have `devtools` package, please install before running install script.

```{r}
library(devtools)
install_github("https://github.com/Marchen/model.adapter")
install_github("https://github.com/Marchen/partial.plot")
```

Older version could be installed using following code.

```{r}
install.packages(
    c("model.adapter", "partial.plot"), type = "source",
    repos = c(
        "http://florivory.net/R/repos", options()$repos
    )
)
```

To build the package from source codes, use `source("build.r")`.

For the details see Quick start guide ([English](http://florivory.net/R/partial.plot/partial.plot.html), [Source Code](https://github.com/Marchen/partial.plot/blob/master/vignettes/partial.plot.rmd)) or ([Japanese](http://florivory.net/R/partial.plot/partial.plot.j.html), [Source Code](https://github.com/Marchen/partial.plot/blob/master/vignettes/partial.plot.j.rmd)).

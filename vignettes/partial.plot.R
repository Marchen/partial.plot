## ---- message = FALSE, warning = FALSE, eval = FALSE--------------------------
#  install.packages(
#      c("model.adapter", "partial.plot"), type = "source",
#      repos = c(
#          "http://florivory.net/R/repos", options()$repos
#      )
#  )

## ---- echo = FALSE, message = FALSE-------------------------------------------
library(partial.plot)

## -----------------------------------------------------------------------------
# Load dataset.
data(iris)

# Create a prediction model by GLM.
model <- glm(
    Petal.Length ~ (Sepal.Length + Petal.Width) * Species, data = iris
)

# Load the library.
library(partial.plot)

# Draw relationship between sepal length and petal length.
info <- partial.plot(model, c("Sepal.Length", "Species"), pch = 16)

# Add a legend.
pp.legend(info, "topleft")

# Draw 3D plot.
info <- partial.plot(
    model, c("Sepal.Length", "Petal.Width"), col = terrain.colors,
    theta = 20
)

## -----------------------------------------------------------------------------
# Load the dataset.
data(iris)

# See the structure of the dataset.
str(iris)

# Visualize data.
par(mfrow = c(2, 2))
plot(
    Petal.Length ~ Sepal.Length, data = iris,
    pch = as.numeric(Species) + 14, col = as.numeric(Species) + 1
)
plot(
    Petal.Length ~ Sepal.Width, data = iris,
    pch = as.numeric(Species) + 14, col = as.numeric(Species) + 1
)
plot(
    Petal.Length ~ Petal.Width, data = iris,
    pch = as.numeric(Species) + 14, col = as.numeric(Species) + 1
)

## -----------------------------------------------------------------------------
# Make prediction model.
model <- glm(
    Petal.Length ~ (Sepal.Length + Petal.Width) * Species, data = iris
)

# Show summary of the model.
summary(model)

## -----------------------------------------------------------------------------
# Load library.
library(partial.plot)

# Visualize predicted relationship between sepal length and petal length.
partial.plot(model, c("Sepal.Length", "Species"), pch = 16)

## -----------------------------------------------------------------------------
# Visualize predicted relationship between petal width and petal length.
partial.plot(model, c("Petal.Width", "Species"), pch = 16)

## ---- eval = FALSE------------------------------------------------------------
#  # Store settings of partial.plot into info.
#  info <- partial.plot(model, c("Sepal.Length", "Species"), pch = 16)

## -----------------------------------------------------------------------------
# Add legend.
info <- partial.plot(model, c("Sepal.Length", "Species"), pch = 16)
pp.legend(info, "topleft")

## -----------------------------------------------------------------------------
partial.plot(
    model, c("Sepal.Length", "Species"), draw.residual = FALSE
)

## -----------------------------------------------------------------------------
partial.plot(
    model, c("Sepal.Length", "Species"), draw.relationship = FALSE,
    pch = 16
)

## -----------------------------------------------------------------------------
info <- partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16, extrapolate = TRUE
)
pp.legend(info, "topleft")

## -----------------------------------------------------------------------------
# Change colors of the graph.
info <- partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16, col = rainbow
)
# Colors in the legend is automatically adjusted.
pp.legend(info, "topleft")

## -----------------------------------------------------------------------------
# Prepare a named color vector.
col <- c(
    setosa = "darkgreen", versicolor = "blue", virginica = "orange2"
)
# Specify the vector for col argument.
info <- partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16, col = col
)
pp.legend(info, "topleft")

## -----------------------------------------------------------------------------
# Set other graphic parameters
info <- partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16, cex = 1.5,
    xlab = "Sepal length (mm)", ylab = "Petal Length"
)
pp.legend(info, "topleft")

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  data$column1 <- c(scale(data$column1))

## ---- eval = FALSE------------------------------------------------------------
#  # Example
#  model <- glmer.nb(y ~ log(x) + (1 | random), data = dat)
#  partial.plot(model, "x", data = dat)

## -----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#   Prepare dataset and model.
#-----------------------------------------------------------------------------

# Load 'Insurance' data containing number of claims from MASS package.
# For the details, see '?Insurance' after loading MASS package.
utils::data(Insurance, package="MASS")

# Convert age from factor to numeric.
Insurance$Age.numeric <- c(25, 27, 32.5, 35)[as.numeric(Insurance$Age)]

# Model the number of insurance claims by district, type of cars and age.
# The number of policyholders is used for the offset term.
model <- glm(
    Claims ~ District + Group + Age.numeric + offset(log(Holders)),
    data = Insurance, family = poisson
)

#-----------------------------------------------------------------------------
#   Plotting 1: case of failure.
#-----------------------------------------------------------------------------

# Simply draw partial dependence plot.
# Predicted relationship and partial residual don't match each other.
pp <- partial.plot(model, c("Age.numeric", "Group"))
pp.legend(pp, "topright")

#-----------------------------------------------------------------------------
#   Solution 1: disable drawing partial residual.
#   In this case, predicted relationship with mean number of policyholders
#   is drawn.
#-----------------------------------------------------------------------------
pp <- partial.plot(model, c("Age.numeric", "Group"), draw.residual = FALSE)
pp.legend(pp, "topright")

#-----------------------------------------------------------------------------
#   Solution 2: use response variable/offset term.
#   In this case, we calculate claims count/number of policyholders and set 1
#   for number of policyholders of all records  before plotting.
#-----------------------------------------------------------------------------

# Create a copy of the dataset.
Insurance2 <- Insurance

# Convert the values of response variable from number of claims to number of
# claims per number of policyholders.
Insurance2$Claims <- Insurance2$Claims / Insurance2$Holders

# Set 1 for number of policyholders of all records.
Insurance2$Holders <- 1

# Draw partial dependence plot for number of claims per number of policyholders.
pp <- partial.plot(model, c("Age.numeric", "District"), data = Insurance2)
pp.legend(pp, "topright")

## -----------------------------------------------------------------------------
library(partial.plot)
plot(
    Petal.Length ~ Sepal.Length, col = color.ramp(Species),
    data = iris, pch = 16
)

## -----------------------------------------------------------------------------
plot(
    Petal.Length ~ Sepal.Length, col = color.ramp(Species, rainbow),
    data = iris, pch = 16
)

## -----------------------------------------------------------------------------
col <- c(
    setosa = "darkgreen", versicolor = "blue", virginica = "orange2"
)
plot(
    Petal.Length ~ Sepal.Length, col = color.ramp(Species, col),
    data = iris, pch = 16
)

## -----------------------------------------------------------------------------
barplot(1:10, col = gg.colors(10))


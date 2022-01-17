## ---- echo = FALSE, message = FALSE-------------------------------------------
library(partial.plot)
library(viridis)
library(knitr)
library(rgl)
knit_hooks$set(webgl = hook_webgl)

## ----no_interaction_2d--------------------------------------------------------
model <- glm(Petal.Length ~ Sepal.Length + Petal.Width, data = iris)
info <- partial.plot(model, "Sepal.Length", pch = 16)
pp.legend(info, "topleft")

## -----------------------------------------------------------------------------
model <- glm(
    Petal.Length ~ (Sepal.Length + Petal.Width) * Species, data = iris
)

## ----interaction_2d-----------------------------------------------------------
info <- partial.plot(model, c("Sepal.Length", "Species"), pch = 16)
pp.legend(info, "topleft")

## ----type---------------------------------------------------------------------
# Test using iris.
model <- glm(Petal.Length ~ Sepal.Length + Petal.Width, data = iris)
info <- partial.plot(model, "Sepal.Length", pch = 16)

model <- glm(
    Petal.Length ~ Sepal.Length + Petal.Width, data = iris,
    family = Gamma(log)
)
info <- partial.plot(model, "Sepal.Length", pch = 16, type = "response")
info <- partial.plot(model, "Sepal.Length", pch = 16, type = "link")

# Test using ChickWeight
model <- glm(
    weight ~ Time * Diet, family = Gamma, data = as.data.frame(ChickWeight)
)
info <- partial.plot(model, c("Time", "Diet"), pch = 16, type = "response")
points(ChickWeight$Time, ChickWeight$weight, col = "black", pch = 16)
info <- partial.plot(model, c("Time", "Diet"), pch = 16, type = "link")

# Test from glmmML example.
id <- factor(rep(1:20, rep(5, 20)))
y <- rbinom(100, prob = rep(runif(20), rep(5, 20)), size = 1)
x <- rnorm(100)
dat <- data.frame(y = y, x = x, id = id)
model <- glm(y ~ x + id, data = dat, family = binomial)
partial.plot(model, c("x", "id"))

# Test probability
library(randomForest)
model <- randomForest(Species ~ ., data = iris)
partial.plot(
    model, "Petal.Length", positive.class = "setosa", type = "prob",
    col = "red", resolution = 20, draw.residual = FALSE
)
partial.plot(
    model, "Petal.Length", positive.class = "versicolor", add = TRUE,
    type = "prob", col = "blue", resolution = 20, draw.residual = FALSE
)
partial.plot(
    model, "Petal.Length", positive.class = "virginica", add = TRUE,
    type = "prob", col = "green", resolution = 20, draw.residual = FALSE
)

## ----hide_points--------------------------------------------------------------
model <- glm(
    Petal.Length ~ (Sepal.Length + Petal.Width) * Species, data = iris
)
info <- partial.plot(
    model, c("Sepal.Length", "Species"), draw.residual = FALSE
)
pp.legend(info, "topleft")

## ----hide_interval------------------------------------------------------------
info <- partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16, draw.interval = FALSE
)
pp.legend(info, "topleft")

## ----hide_lines---------------------------------------------------------------
info <- partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16, draw.relationship = FALSE
)
pp.legend(info, "topleft")

## ----hist---------------------------------------------------------------------
info <- partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16, draw.hist = TRUE
)
pp.legend(info, "topleft")

## ----draw_extrapolation-------------------------------------------------------
info <- partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16, extrapolate = TRUE
)
pp.legend(info, "topleft")

## ----set_labels_2d------------------------------------------------------------
info <- partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16,
    xlab = "X Label", ylab = "Y Label"
)
pp.legend(info, "topleft")

## ----change_colors------------------------------------------------------------
par(mfrow = c(2, 2))
info <- partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16,
    main = "col = 'black'", col = "black"
)
pp.legend(info, "topleft", cex = 0.7)
info <- partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16,
    main = "col = viridis", col = viridis
)
pp.legend(info, "topleft", cex = 0.7)
info <- partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16,
    main = "col = c('red', 'blue', 'cyan')", col = c("red", "blue", "cyan")
)
pp.legend(info, "topleft", cex = 0.7)
info <- partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16,
    main = "col = c(versicolor='blue', \nsetosa='red', virginica='cyan')",
    col = c(versicolor = "blue", setosa = "red", virginica = "cyan")
)
pp.legend(info, "topleft", cex = 0.7)
par(mfrow = c(1, 1))

## ----change_lwd---------------------------------------------------------------
info <- partial.plot(
    model, c("Sepal.Length", "Species"), lwd = c(1, 4, 8),
    main = "col = 'black'", col = "black"
)
pp.legend(info, "topleft")

## ----change_lty---------------------------------------------------------------
info <- partial.plot(
    model, c("Sepal.Length", "Species"), lty = c("solid", "dashed", "dotted"),
    main = "col = 'black'", col = "black", lwd = 2
)
pp.legend(info, "topleft")

## ----change_pch---------------------------------------------------------------
info <- partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16:18,
    main = "col = 'black'", col = "black", lwd = 2
)
pp.legend(info, "topleft")

## ----change_intervals---------------------------------------------------------
par(mfrow = c(2, 2))
partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16,
    main = "interval = 0.95 (Default)"
)
partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16,
    main = "interval = 0.8", interval.levels = 0.8
)
partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16,
    main = "interval = 0.7", interval.levels = 0.7
)
partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16,
    main = "interval = 0.6", interval.levels = 0.6
)
par(mfrow = c(1, 1))

## ----reuse_information--------------------------------------------------------
par(mfrow = c(2, 2))

info <- partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16, n.cores = 1
)
pp.legend(info, "topleft", cex = 0.5)

info <- partial.plot(info, pch = 6, draw.relationship = FALSE)
pp.legend(info, "topleft", cex = 0.5)

info <- partial.plot(info, col = rainbow, draw.interval = FALSE)
pp.legend(info, "topleft", cex = 0.5)

info <- partial.plot(info, col = heat.colors, draw.residual = FALSE)
pp.legend(info, "topleft", cex = 0.5)

par(mfrow = c(1, 1))

## ----use_colon----------------------------------------------------------------
model <- stats::glm(
    Petal.Length ~ (Sepal.Length + Petal.Width) * Species, data = iris
)
info <- partial.plot(model, c("Sepal.Length", "Species"), pch = 16)
pp.legend(info, "topleft")

## ----persp--------------------------------------------------------------------
info <- partial.plot(
    model, c("Sepal.Length", "Petal.Width"), col = viridis, fun.3d = persp
)

## ----image--------------------------------------------------------------------
info <- partial.plot(
    model, c("Sepal.Length", "Petal.Width"), col = viridis, pch = 16,
    fun.3d = image
)

## ----contour------------------------------------------------------------------
info <- partial.plot(
    model, c("Sepal.Length", "Petal.Width"), fun.3d = contour
)

## ---- persp3d, webgl = TRUE, rgl = TRUE---------------------------------------
info <- partial.plot(
    model, c("Sepal.Length", "Petal.Width"), col = viridis,
    fun.3d = persp3d
)

## ----change_label_persp-------------------------------------------------------
info <- partial.plot(
    model, c("Sepal.Length", "Petal.Width"), col = viridis,
    fun.3d = persp, xlab = "X label", ylab = "Y label", zlab = "Z label"
)

## ---- change_label_persp_3d, webgl = TRUE, rgl = TRUE-------------------------
info <- partial.plot(
    model, c("Sepal.Length", "Petal.Width"), col = viridis,
    fun.3d = persp3d, xlab = "X label", ylab = "Y label", zlab = "Z label"
)

## ---- cforest, warning = FALSE, message = FALSE-------------------------------
library(party)
# 2D plot.
model <- cforest(
    Petal.Length ~ Sepal.Length + Petal.Width + Species, data = iris,
    controls = cforest_unbiased(ntree = 10, mtry = 2)
)
info <- partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16, n.cores = 2
)
pp.legend(info, "topleft")

# 3D plot.
model <- cforest(
    Petal.Length ~ Sepal.Length + Petal.Width, data = iris,
    controls = cforest_unbiased(ntree = 10, mtry = 2)
)
info <- partial.plot(
    model, c("Sepal.Length", "Petal.Width"), pch = 16, n.cores = 2,
    fun.3d = persp, col = viridis, theta = 40
)

## ---- ctree, warning = FALSE, message = FALSE---------------------------------
library(party)
# 2D plot.
model <- ctree(
    Petal.Length ~ Sepal.Length + Petal.Width + Species, data = iris,
    controls = ctree_control(mtry = 2)
)
info <- partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16, n.cores = 2
)
pp.legend(info, "topleft")

# 3D plot.
model <- ctree(
    Petal.Length ~ Sepal.Length + Petal.Width, data = iris,
    controls = ctree_control(mtry = 2)
)
info <- partial.plot(
    model, c("Sepal.Length", "Petal.Width"), pch = 16, n.cores = 2,
    fun.3d = persp, col = viridis, theta = 40
)

## ----gam_gam------------------------------------------------------------------
#library(gam)
#model <- gam(
    #Petal.Length ~ (Sepal.Length + Petal.Width) * Species, data = iris
#)
#info <- partial.plot(model, c("Sepal.Length", "Species"), pch = 16, n.cores = 1)
#pp.legend(info, "topleft")
#info <- partial.plot(
    #model, c("Sepal.Length", "Petal.Width"), pch = 16, col = viridis,
    #fun.3d = persp
#)

## ----mgcv_gam-----------------------------------------------------------------
#library(mgcv)
#model <- gam(
    #Petal.Length ~ (Sepal.Length + Petal.Width) * Species, data = iris
#)
#info <- partial.plot(model, c("Sepal.Length", "Species"), pch = 16)
#pp.legend(info, "topleft")
#info <- partial.plot(
    #model, c("Sepal.Length", "Petal.Width"), pch = 16, col = viridis,
    #fun.3d = persp
#)

## ----gamm---------------------------------------------------------------------
#library(mgcv)
#model <- gamm(
    #Petal.Length ~ Sepal.Length * Species + s(Petal.Width), data = iris
#)
#info <- partial.plot(model, c("Sepal.Length", "Species"), pch = 16)
#pp.legend(info, "topleft")
#info <- partial.plot(
    #model, c("Sepal.Length", "Petal.Width"), pch = 16, col = viridis,
    #fun.3d = persp
#)

## ----gbm----------------------------------------------------------------------
#library(gbm)
#model <- gbm(
    #Petal.Length ~ Sepal.Length + Petal.Width + Species, data = iris,
    #n.trees = 100
#)
#info <- partial.plot(model, c("Sepal.Length", "Species"), pch = 16, n.cores = 1)
#pp.legend(info, "topleft")
#info <- partial.plot(
    #model, c("Sepal.Length", "Petal.Width"), pch = 16, col = viridis,
    #fun.3d = persp, n.cores = 1
#)

## ----glm.nb-------------------------------------------------------------------
library(MASS)
data(quine)
temp <- quine
temp$Age <- as.numeric(temp$Age)
model <- glm.nb(Days ~ Sex * Age, data = temp)
partial.plot(model, c("Age", "Sex"))

## ----glmer--------------------------------------------------------------------
library(lme4)

# Prepare Data
iris2 <- iris
iris2$random <- factor(rep(c("a", "b", "c"), 50))
iris2$Seeds <- rpois(150, iris2$Petal.Length*3 + as.numeric(iris2$Species))

# Run test.
model <- glmer(
    Seeds ~ (Petal.Length + Petal.Width) + Species + (1 | random),
    data = iris2, family = poisson
)
info <- partial.plot(model, c("Petal.Length", "Species"), pch = 16)
pp.legend(info, "topleft")
info <- partial.plot(
    model, c("Petal.Length", "Petal.Width"), pch = 16, col = viridis,
    fun.3d = persp
)

## ----glmer.nb-----------------------------------------------------------------
library(lme4)
data(quine)
temp <- quine
temp$Age <- as.numeric(temp$Age)
model <- glmer.nb(Days ~ Sex * Age + (1 | Eth), data = temp)
partial.plot(model, c("Age", "Sex"))

## ---- glmmTMB, message = FALSE, warning = FALSE-------------------------------
library(glmmTMB)
model <- glmmTMB(
    Petal.Length ~ (Sepal.Length + Petal.Width) * Species, data = iris,
    family = "gaussian"
)
# 2D plot
info <- partial.plot(model, c("Sepal.Length", "Species"), pch = 16)
pp.legend(info, "topleft")
# 3D plot
info <- partial.plot(
    model, c("Sepal.Length", "Petal.Width"), pch = 16, col = viridis,
    fun.3d = persp
)

## ---- glmmadmb, message = FALSE, warning = FALSE------------------------------
library(glmmADMB)
model <- glmmadmb(
    Petal.Length ~ (Sepal.Length + Petal.Width) * Species, data = iris,
    family = "gaussian"
)
# 2D plot
info <- partial.plot(model, c("Sepal.Length", "Species"), pch = 16)
pp.legend(info, "topleft")
# 3D plot
info <- partial.plot(
    model, c("Sepal.Length", "Petal.Width"), pch = 16, col = viridis,
    fun.3d = persp
)

## ----glmmML-------------------------------------------------------------------
library(party)
library(glmmML)
library(viridis)
model <- glmmML(
    age ~ (shoeSize + score) * nativeSpeaker, cluster = 1:200,
    data = party::readingSkills, family = poisson
)
# 2D plot.
info <- partial.plot(model, c("score", "nativeSpeaker"), pch = 16, n.cores = 1)
info <- partial.plot(
    model, c("score", "nativeSpeaker"), pch = 16, type = "link", n.cores = 1
)
pp.legend(info, "topleft")
# 3D plot.
info <- partial.plot(
    model, c("shoeSize", "score"), pch = 16, col = viridis,
    fun.3d = persp
)

## ----lm-----------------------------------------------------------------------
model <- lm(
    Petal.Length ~ (Sepal.Length + Petal.Width) * Species, data = iris
)
info <- partial.plot(model, c("Sepal.Length", "Species"), pch = 16)
pp.legend(info, "topleft")
info <- partial.plot(
    model, c("Sepal.Length", "Petal.Width"), pch = 16, col = viridis,
    fun.3d = persp
)

## ---- lme, message = FALSE----------------------------------------------------
library(nlme)
# 2D plot.
model <- lme(
    MEANSES ~ (DISCLIM + PRACAD) * Sector, random = ~ 1 | School,
    data =  MathAchSchool
)
info <- partial.plot(model, c("DISCLIM", "Sector"), pch = 16)
pp.legend(info, "topleft")

# 3D plot.
model <- lme(
    MEANSES ~ DISCLIM + PRACAD, random = ~ 1 | School,
    data = MathAchSchool
)
info <- partial.plot(
    model, c("DISCLIM", "PRACAD"), col = viridis, fun.3d = persp
)

## ----lmer---------------------------------------------------------------------
library(lme4)
iris2 <- iris
iris2$random <- factor(rep(c("a", "b", "c"), 50))
model <- lmer(
    Petal.Length ~ (Sepal.Length + Petal.Width) * Species + (1 | random),
    data = iris2, REML = FALSE, na.action = na.fail
)
info <- partial.plot(model, c("Sepal.Length", "Species"), pch = 16)
pp.legend(info, "topleft")
info <- partial.plot(
    model, c("Sepal.Length", "Petal.Width"), pch = 16, col = viridis,
    fun.3d = persp
)

## ---- MCMCglmm, message = FALSE-----------------------------------------------
# library(MCMCglmm)
# model <- MCMCglmm(
#     Petal.Length ~ (Sepal.Length + Petal.Width), data = iris,
#     verbose = FALSE, nitt = 50000
# )
# # 2D plot
# info <- partial.plot(
#     model, c("Sepal.Length"), pch = 16, data = iris, n.cores = 1
# )
# pp.legend(info, "topleft")
# # 3D plot
# info <- partial.plot(
#     model, c("Sepal.Length", "Petal.Width"), pch = 16, col = viridis,
#     fun.3d = persp, data = iris
# )

## ----randomForest-------------------------------------------------------------
library(randomForest)
model <- randomForest(
    Petal.Length ~ Sepal.Length + Petal.Width + Species, data = iris,
    ntree = 100
)
info <- partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16, n.cores = 1
)
pp.legend(info, "topleft")
info <- partial.plot(
    model, c("Sepal.Length", "Petal.Width"), pch = 16, col = viridis,
    fun.3d = persp, n.cores = 1
)

## ----ranger-------------------------------------------------------------------
library(ranger)
model <- ranger(
    Petal.Length ~ Sepal.Length + Petal.Width + Species, data = iris,
    write.forest = TRUE
)
info <- partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16, n.cores = 1
)
pp.legend(info, "topleft")
info <- partial.plot(
    model, c("Sepal.Length", "Petal.Width"), pch = 16, col = viridis,
    fun.3d = persp, n.cores = 1
)

## ----rpart--------------------------------------------------------------------
library(rpart)
model <- rpart(
    Petal.Length ~ Sepal.Length + Petal.Width + Species, data = iris
)
info <- partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16, n.cores = 1
)
pp.legend(info, "topleft")
info <- partial.plot(
    model, c("Sepal.Length", "Petal.Width"), pch = 16, col = viridis,
    fun.3d = persp, n.cores = 1
)

## ----svm----------------------------------------------------------------------
library(e1071)
model <- svm(
    Petal.Length ~ Sepal.Length + Petal.Width + Species, data = iris
)
info <- partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16, n.cores = 1
)
pp.legend(info, "topleft")
info <- partial.plot(
    model, c("Sepal.Length", "Petal.Width"), pch = 16, col = viridis,
    fun.3d = persp, n.cores = 1
)

## ----tree---------------------------------------------------------------------
library(tree)
model <- tree(
    Petal.Length ~ Sepal.Length + Petal.Width + Species, data = iris
)
info <- partial.plot(
    model, c("Sepal.Length", "Species"), pch = 16, n.cores = 1
)
pp.legend(info, "topleft")
info <- partial.plot(
    model, c("Sepal.Length", "Petal.Width"), pch = 16, col = viridis,
    fun.3d = persp, n.cores = 1
)



### Libraries
library(rio)
library(here)
library(tidyverse)
library(haven)
library(janitor)
library(knitr)
library(surveytoolbox)
library(sjPlot)
library(kableExtra)
library(psych)
library(sjmisc)
library(MVN)

### The dataset ###
parent_w1 <- read_sav(here("outcomes paper/data_outcomes", "primary_parent_w1.sav"))

################################################
# From Flora, LaBrish, 
################################################
# Check collineraity: "refers to perfect or near-perfect linear relationships among observed variables. With multiple regression, the focus is on collinearity among explanatory variables, but with factor analysis, the concern is collinearity among dependent variables,that is, the set of variables being factor analyzed. When collinear variables are included, the product-moment correlation matrix R will be singular, or non-positive definite.
# Collinearity in factor analysis is relatively simple to diagnose: if any eigen values of a product-moment R equal zero or are negative,then R is non-positive definite and collinearity is present(and software will likely produce a warning message).

# Heywood cases: "Improper solutions (e.g.,a model solution with at least one negative estimated residual variance term, or“Heywoodcase”) are also likely to occur in the presence of one or more unusual cases (Bollen, 1987), which can lead to a researcher unwittingly revising a model or removing an observed variable from the analysis."

# # #
# 1. Check for collinearity between items included on a EFA model
################################################


#######################################################################################
############################# Positive relationship ###################################

### The items ###
# 116.	Cuando mi joven me pide hablar o cuando necesita hablar conmigo, escucho atentamente.
# 117.	Sé escuchar atentamente, aun cuando no esté de acuerdo con lo que dice la otra persona.
# 118.	Con regularidad, mi joven y yo hacemos cosas juntos que ambos disfrutamos
# 119.	Mi joven y yo tenemos una relación cercana
# 120.	Hago y digo cosas que le muestran a mi joven que a él/ella me importa y que yo la/lo amo (por ejemplo, ...)
# 121.	Como madre/padre, es mi trabajo reconocer y apoyar las fortalezas de mi joven

# The scale to factor analyze
posi_rel <- parent_w1 %>%
  select(q116, q117, q118, q119, q120, q121)

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(posi_rel) # 0.88 ok 

# Poly corr matrix
poly <- polychoric(posi_rel)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F)

### EFA analyses: using oblimin: it's best to assume that factors are correlated.  

# EFA using ULS 
factor_test_uls <- fa(posi_rel, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 
# Model above throws warning message: "The estimated weights for the factor scores are probably incorrect.  Try a different factor score estimation method."

fa.diagram(factor_test_uls) # 119 and 120 have really high factorial loads 

# EFA using PA
factor_test_pa <- fa(posi_rel, n.obs = 95, rotate = "oblimin", fm = "pa", cor = "poly", nfactors = 1) 
# Model above throws warning message: "The estimated weights for the factor scores are probably incorrect.  Try a different factor score estimation method."

fa.diagram(factor_test_pa) # 119 and 120 have really high factorial loads 

#### Conclusion here: estimation method (uls or pa) shows the same factor loadings. Item 120 has a very high factor loading (0.99) and by checking the poly cor matrix, item 120 shows that it has very high correlations with all items

# Removing item 120

# The scale to factor analyze
posi_rel_2 <- parent_w1 %>%
  select(q116, q117, q118, q119, q121)

# Poly corr matrix
poly <- polychoric(posi_rel_2)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F) # The correlations are not as elevated

# EFA using ULS 
factor_test_uls <- fa(posi_rel_2, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) # no warning message :)

# scree plot
scree(posi_rel_2, factors = TRUE, pc = FALSE, main = "Scree plot", hline = NULL, add = FALSE)
# Scree plot clearly shows 1 factor solution

# parallel test, anyways
fa.parallel(posi_rel_2, n.obs=NULL, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE)
# Parallel analysis suggests that the number of factors =  1, BUT it throws warnings about 

# alpha
alpha(posi_rel_2) # 0.89

# Omega 
omega(posi_rel_2, nfactors = 1, n.obs = 95, flip = T, plot = T) # likes more than 1 factor, but still good to know Omega Total 0.89 (same as alpha)

# The final set of items would be
# 116.	Cuando mi joven me pide hablar o cuando necesita hablar conmigo, escucho atentamente.
# 117.	Sé escuchar atentamente, aun cuando no esté de acuerdo con lo que dice la otra persona.
# 118.	Con regularidad, mi joven y yo hacemos cosas juntos que ambos disfrutamos
# 119.	Mi joven y yo tenemos una relación cercana
# --
# 121.	Como madre/padre, es mi trabajo reconocer y apoyar las fortalezas de mi joven

# el siguiente paso seria usar lavaan, especificar el modelo 
rp <- ("rp=~ q116 + q117 + q118 + q119 + q121") # aqui necesito un long format (116 seria variable y row seria tiempo 1, 2 y 3)

#######################################################################################
############################# Monitoring ##############################################

### The items ###
# 137.	A menudo hablo con mi joven acerca de sus planes para el día siguiente
# 138.	Hablo con mi joven en muchas ocasiones acerca de lo que él/ella aprendió en la escuela
# 139.	A menudo hablo con mi joven acerca de sus amigos. 
# 140.	Conozco muy bien a los amigos de mi joven. 
# 141.	Los amigos de mi joven tienen una buena influencia en su vida. 
# 142.	Los amigos de mi joven se apoyan positivamente entre sí.
# 143.	Por lo general yo sé con quién está mi joven
# 144.	Sé lo que hace mi joven y dónde va cuando no está en casa

# The scale to factor analyze
monit <- parent_w1 %>%
  select(q137, q138, q139, q140, q141, q142, q143, q144)

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(monit) # 0.78

# Poly corr matrix
poly <- polychoric(monit)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F)

# EFA using ULS 
factor_test_uls <- fa(monit, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 
# No warnings
# all factor loadings above 0.4

# scree plot
scree(monit, factors = TRUE, pc = FALSE, main = "Scree plot", hline = NULL, add = FALSE)
# Scree plot clearly shows 1 factor solution

# parallel test, anyways
fa.parallel(monit, n.obs=NULL, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE)
# Parallel analysis suggests that the number of factors =  2, warnings about ulta-heywood case

# EFA using ULS - More than 1 factors 
factor_test_uls_2 <- fa(monit, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 2) 

monit_fa_1 <- parent_w1 %>%
  select(q140, q141, q142, q143)
# 140.	Conozco muy bien a los amigos de mi joven. 
# 141.	Los amigos de mi joven tienen una buena influencia en su vida. 
# 142.	Los amigos de mi joven se apoyan positivamente entre sí.
# 143.	Por lo general yo sé con quién está mi joven

monit_fa_2 <- parent_w1 %>%
  select(q138, q139, q144)
# 138.	Hablo con mi joven en muchas ocasiones acerca de lo que él/ella aprendió en la escuela
# 139.	A menudo hablo con mi joven acerca de sus amigos. 
# 144.	Sé lo que hace mi joven y dónde va cuando no está en casa

# alpha
alpha(monit_fa_1) # 0.76
alpha(monit_fa_2) # 0.67

# Omega I AM NOT SURE IF THIS IS THE WAY TO COMPUTE THIS !!!
omega(monit, nfactors = 2, n.obs = 95, flip = T, plot = T) # likes more than 1 factor, but still good to know Omega Total 0.82 (same as alpha)

#######################################################################################
############################# Homework Involvement ##############################################

### The items ###
# 82	Mi joven ha tenido la plena responsabilidad de completar su tarea; no me involucro.
# 83	Dejé de intentar (de ayudarlo con su tarea).
# 84	Me aseguré de que tenga un cierto tiempo para hacer la tarea
# 85	Me aseguré de que él / ella tenga un cierto lugar para hacer la tarea
# 86	Me senté con mi joven mientras él / ella hacía su tarea
# 87	Me hice disponible para mi joven cuando tenía preguntas sobre su tarea
# 88	Ayudé a mi joven cuando estaba atascado/a en una tarea
# 89	Ayudé a mi joven a prepararse para los exámenes haciéndole preguntas sobre el tema, etc.
# 90	Le di a mi joven incentivos como privilegios especiales, regalos especiales, etc.
# 91	Utilicé consecuencias, castigos, acciones disciplinarias
# 92	Le recordé a mi joven que hiciera su tarea
# 93	Comprobé con mi joven para asegurarme de que él / ella hiciera su tarea
# 94	Comprobé y corregí la tarea de mi hija/o 
# 95	Comprobé con la maestra/o para asegurarme que mi joven terminó su tarea 
# 96	Le ayudé a mi joven acceder a otros recursos (biblioteca, materiales de arte, laboratorio de computación, etc.)
# 97	Le ayudé a mi joven practicar habilidades (lectura, revisión, problemas de práctica, etc.)
# 98	Limité el ruido y/o las distracciones mientras hacia su tarea

# The scale to factor analyze
hw_inv <- parent_w1 %>%
  select(q82, q83, q84, q85, q86, q87, q88, q89, q90, q91, q92, q93, q94, q95, q96, q97, q98)

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(hw_inv) # 0.69

# Poly corr matrix
poly <- polychoric(hw_inv)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F)
# items 82, 83, 92 appear problematic

# EFA using ULS 
factor_test_uls <- fa(hw_inv, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 
# items 82 and 83 neg. correlated with factor
# items 91 and 92 load at or below .31 

# new efa iteration without problematic items
hw_inv_2 <- parent_w1 %>%
  select(q84, q85, q86, q87, q88, q89, q90, q93, q94, q95, q96, q97, q98)

# Poly corr matrix
poly <- polychoric(hw_inv_2)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F) # looks better

# EFA using ULS 
factor_test_uls <- fa(hw_inv_2, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 

# scree plot
scree(hw_inv_2, factors = TRUE, pc = FALSE, main = "Scree plot", hline = NULL, add = FALSE)
# Scree plot clearly shows 2 factor solution

# parallel test, anyways
fa.parallel(hw_inv_2, n.obs=NULL, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE)
# Parallel analysis suggests that the number of factors =  3

# EFA using ULS - More than 1 factors 
factor_test_uls <- fa(hw_inv_2, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 3) 

hw_inv_fa_1 <- parent_w1 %>% # tutoring
  select(q87, q88, q89, q90, q96, q97) # q86 loads in fa 1 and fa 3; q98 loads in fa 1 and fa 2

# -- decided to "give" 86 to fa 3 bec. the factor feels more theoretically cohesive: checking completion
# 87	Me hice disponible para mi joven cuando tenía preguntas sobre su tarea
# 88	Ayudé a mi joven cuando estaba atascado/a en una tarea
# 89	Ayudé a mi joven a prepararse para los exámenes haciéndole preguntas sobre el tema, etc.
# 90	Le di a mi joven incentivos como privilegios especiales, regalos especiales, etc.
# 96	Le ayudé a mi joven acceder a otros recursos (biblioteca, materiales de arte, laboratorio de computación, etc.)
# 97	Le ayudé a mi joven practicar habilidades (lectura, revisión, problemas de práctica, etc.)
# -- decided to "give" 98 to fa 2 bec. the factor feels more theoretically cohesive: facilitating environment

hw_inv_fa_2 <- parent_w1 %>% # facilitating
  select(q84, q85, q98) #q93 load in fa 2 and fa 3; q98 loads in fa 1 and fa 2

# 84	Me aseguré de que tenga un cierto tiempo para hacer la tarea
# 85	Me aseguré de que él / ella tenga un cierto lugar para hacer la tarea
# -- decided to "give" 93 to fa 3 bec. the factor feels more theoretically cohesive: checking completion 
# 98	Limité el ruido y/o las distracciones mientras hacia su tarea

hw_inv_fa_3 <- parent_w1 %>% # checking
  select(q86, q93, q94, q95) # q86 loads in fa 1 and fa 3; #q93 load in fa 2 and fa 3
# 86	Me senté con mi joven mientras él / ella hacía su tarea
# 93	Comprobé con mi joven para asegurarme de que él / ella hiciera su tarea
# 94	Comprobé y corregí la tarea de mi hija/o 
# 95	Comprobé con la maestra/o para asegurarme que mi joven terminó su tarea 


# alpha
alpha(hw_inv_fa_1) # 0.74
# alpha
alpha(hw_inv_fa_2) # 0.65
# alpha
alpha(hw_inv_fa_3) # 0.66

# Omega 
omega(hw_inv_2, nfactors = 3, n.obs = 95, flip = T, plot = T) # likes more than 1 factor, but still good to know Omega Total X (same as alpha)

# the omega figure is different than my conceptual "giving" of items. Yields just 2 factors. What is best to do?

### 
### 
### 
### 
### 
### 
### 
### 

### CODE TO COPY PASTE ###

#######################################################################################
############################# Scale name ##############################################

### The items ###
#
#
#

# The scale to factor analyze
scale_name <- datafile_name %>%
  select(item1, item2, item3, item...n)

## kmo Kaiser-Meyer-Olkin factor SAMPLING ADEQUACY (ABOVE 0.5, CLOSER TO 1 BEST)
KMO(scale_name) # 

# Poly corr matrix
poly <- polychoric(scale_name)
cor.plot(poly$rho, numbers = T, upper = F, main = "Polychoric", show.legend = F)

# EFA using ULS 
factor_test_uls <- fa(scale_name, n.obs = 95, rotate = "oblimin", fm = "uls", cor = "poly", nfactors = 1) 

# scree plot
scree(scale_name, factors = TRUE, pc = FALSE, main = "Scree plot", hline = NULL, add = FALSE)
# Scree plot clearly shows X factor solution

# parallel test, anyways
fa.parallel(scale_name, n.obs=NULL, fm="uls", fa="fa", main="Parallel Analysis Scree Plots", cor="poly", use="pairwise", plot=TRUE)
# Parallel analysis suggests that the number of factors =  X

# alpha
alpha(scale_name) # X

# Omega 
omega(scale_name, nfactors = 1, n.obs = 95, flip = T, plot = T) # likes more than 1 factor, but still good to know Omega Total X (same as alpha)
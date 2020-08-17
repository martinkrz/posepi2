# Points of Significance - The SEIRS model for infectious disease dynamics
# Ottar Bjørnstad (1,2), Katriona Shea (1), Martin Krzywinski (3*), Naomi Altman (4)
#
# 1. Department of Biology, The Pennsylvania State University, State College, PA, USA.
# 2. Department of Entomology, The Pennsylvania State University, State College, PA, USA.
# 3. Canada’s Michael Smith Genome Sciences Centre, Vancouver, British Columbia, Canada.
# 4. Department of Statistics, The Pennsylvania State University, State College, PA, USA.
# * martink@bcgsc.ca
#
# CITATION
# Bjørnstad, O., Shea, K., Krzywinski, M. & Altman, N. 
# Points of Significance: Adding realism to the SIR model for infectious disease epidemics (2020) Nature Methods 17:557–558.
#
# DOWNLOAD
# https://martinkrz.github.io/posepi2
#
# REMOTE ACCESS
#
# http://shiny.bcgsc.ca/posepi2
#
# RUN LOCALLY
#
# 1. Requirements
#
library(shiny)
library(deSolve)
library(ggplot2)
library(stringr)
library(tidyverse)
library(shinyjs)
library(shinyWidgets)
#
# 2. Getting started
#
# Load this file in R Studio and click "Run App" in top right of this pane.
#
# CHANGELOG
#
# 22-04-2020  spawned from posepi1
# 04-05-2020  refactored plots and text into separate files
# 06-05-2020  refactored LaTeX
# 07-05-2020  refined text, added time points and SIR trajectory
# 28-05-2020  first release
# 22-06-2020  updated links
# 17-06-2020  updated links

# CUSTOM SETTINGS
# The colors of the SEIR groups and some greys (Cn)
palette              = c(S="#333333",
                         E="#a0d848",
                         I="#f15a24",
                         R="#29abe2",
                         G="#8cc63f",
                         M="#ed1e79",
                         MD="#720f3f",
                         C="#6eccdc",
                         CD="#3f747a",
                         C1="#333333",
                         C2="#cccccc",
                         C3="#cccccc")
# Plot height and line width for trajectories
plot_line_width      = 1.5
plot_text_size       = 12
# Initial value for I(0). S(0) = 1 - sir_init_i - vaccination_fraction and R(0) = vaccination_fraction
sir_init_i           = 0.001
sir_system_steps     = 1000
sir_system_time_step = 0.01 # last resort default, shouldn't be used
# Infectious period max and default. Slider step is 1.
ip_max               = 28
ip_default           = 14
ip_step              = 1
# R0 max and slider step
R0_max               = 5
R0_step              = 0.1
# Latent period max and default. Slider step is 1.
lp_max               = 28
lp_default           = 7
lp_min               = 1
lp_step              = 1
# Immunity duration max and default. Slider step is 0.1. Units are years.
id_min               = 1
id_max               = 10
id_max               = 10
id_default           = 1
id_step              = 0.1
# Life expectancy max and default. Slider step is 1. Units are years.
le_min               = 10
le_max               = 100
le_default           = 76
le_step              = 1
# Alpha
al_min               = 0
al_max               = 1000
al_default           = 0
al_step              = 1
# do timings
do_timing            = FALSE

interpretive_default = FALSE
caption_default      = FALSE

source("R/format.helpers.R",local=TRUE)
source("R/ggplot.helpers.R",local=TRUE)
source("R/ui.R",local=TRUE)
source("R/server.R",local=TRUE)

shinyApp(ui,server)

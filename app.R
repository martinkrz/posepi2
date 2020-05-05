# Points of Significance - Adding realism to the SIR model for infectious disease epidemics
# Ottar Bjøornstad (1,2), Katriona Shea (1), Martin Krzywinski (3*), Naomi Altman (4)
#
# 1. Department of Biology, The Pennsylvania State University, State College, PA, USA.
# 2. Department of Entomology, The Pennsylvania State University, State College, PA, USA.
# 3. Canada’s Michael Smith Genome Sciences Centre, Vancouver, British Columbia, Canada.
# 4. Department of Statistics, The Pennsylvania State University, State College, PA, USA.
# * martink@bcgsc.ca
#
# CITATION
# Bjornstad, O., Shea, K., Krzywinski, M. & Altman, N. 
# Points of Significance: Adding realism to the SIR model for infectious disease epidemics (2020) Nat Meth 17 (in press)
#
# DOWNLOAD
# https://github.com/martinkrz/posepi2
#
# REMOTE ACCESS
#
# For the URL running the latest version of this app check the GitHub page (link above).
#
# RUN LOCALLY
#
# 1. Requirements
#
require(shiny)
require(deSolve)
require(ggplot2)
library(grid)
library(stringr)
#
# 2. Getting started
#
# Load this file in R Studio and click "Run App" in top right of this pane.
#
# CHANGELOG
#
# 22-04-2020   spawned from posepi1
# 04-05-2020   working on it

# CUSTOM SETTINGS
# The colors of the suscetible (S), infected  (I) and recovered (R) trajectories. 
palette              = c(S="#333333",E="#22b573",I="#f15a24",R="#29abe2",G="#8cc63f",C="#333333")
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
# R0 max and slider step
R0_max               = 5
R0_step              = 0.1
# Latent period max and default. Slider step is 1.
lp_max               = 28
lp_default           = 7
lp_min               = 1
# Immunity duration max and default. Slider step is 0.1. Units are years.
id_max               = 10
id_default           = 2
id_step              = 0.1
# Life expectancy max and default. Slider step is 1. Units are years.
le_max               = 100
le_default           = 76
# do timings
do_timing            = FALSE

my.plot_legend = list(
  scale_colour_manual("GROUP", 
                      breaks = c("S", "E", "I", "R","C"),
                      labels = c("susceptible","exposed","infected","recovered","capacity"),
                      values = palette)
)  

source("R/format.helpers.R",local=T)
source("R/ggplot.helpers.R",local=T)
source("R/ui.R",local=T)
source("R/server.R",local=T)

shinyApp(ui,server)

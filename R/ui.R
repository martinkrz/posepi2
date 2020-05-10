read_latex = function(file) {
  str = readChar(file, file.info(file)$size)
  #str = str_replace_all(str,"/","//")
  return(str)
}

eq_seirs = read_latex("latex/seirs.tex")
eq_equil = read_latex("latex/seirs.equilibrium.tex")
eq_param = read_latex("latex/seirs.parameters.tex")
eq_age   = read_latex("latex/seirs.age-dependent.tex")
eq_r0    = read_latex("latex/seirs.r0.tex")
eq_period= read_latex("latex/seirs.period.tex")
eq_endemic_age= read_latex("latex/seirs.endemic-age.tex")
eq_jacobian= read_latex("latex/seirs.jacobian.tex")
eq_eigenvalue= read_latex("latex/seirs.eigenvalue.tex")

ui = fluidPage( theme=("css/style.css"),
                shinyjs::useShinyjs(),
                htmlOutput("masthead"),
                navbarPage("Adding realism to the SIR model for infectious disease epidemics",id="tabs",
                           tabPanel("The SEIRS model",value=1,id=1,
                                    fluidRow(
                                      column(4,id="sidebar1",
                                      #sidebarPanel(
                                        div(id="form1",
                                            sliderInput("R01", HTML("<i>R</i><sub>0</sub>"), value = 3,
                                                        min = 1, max = R0_max, step = R0_step),
                                        sliderInput("ip1", HTML("Infectious period, 1/<i>&gamma;</i> (days)"), 
                                                    value = ip_default,
                                                    min = 1, max = ip_max, step = ip_step),
                                        sliderInput("lp1", HTML("Latent period, 1/<i>&sigma;</i> (days)"),
                                                    value = lp_default,
                                                    min = lp_min, max = lp_max, step = lp_step),
                                        sliderInput("id1", HTML("Immunity duration, 1/<i>&omega;</i> (years)"),
                                                    value = id_default,
                                                    min = id_step, max = id_max, step = id_step),
                                        sliderInput("le1", HTML("Life expectancy, 1/<i>&mu;</i> (years)"),
                                                    value = le_default,
                                                    min = 1, max = le_max, step = le_step),
                                        sliderInput("al1", HTML("Death onset, 1/<i>&alpha;</i> (days)"),
                                                    value = al_default,
                                                    min = 0, max = al_max, step = al_step),
                                        sliderInput("p1", HTML("vaccination level, <i>p</i> (%)"), 0,
                                                    min = 0, max = 99, step = 1),
                                        checkboxInput("log1", HTML("log<sub>10</sub> axes"), FALSE),
                                        checkboxInput("points1", HTML("show time points"), TRUE),
                                        checkboxInput("sir1",HTML("show SIR <i>I</i>(<i>t</i>) trajectory"),TRUE),
                                        checkboxInput("text1",HTML("interpretive text"),TRUE),
                                        #checkboxInput("table1",HTML("parameter table"),TRUE),
                                        checkboxInput("captions1",HTML("figure captions"),TRUE),
                                        actionButton("refresh1","Reset")
                                        )
                                      ),
                                      column(8,id="main1",
                                      #mainPanel(
                                        div(HTML("The SEIRS model of infection spread"),class="paneltitle"),
                                        div(htmlOutput("text1intro"),class="copy"),
                                        tabsetPanel(
                                          tabPanel("Trajectory",
                                                div(htmlOutput("text1a"),class="copy copy1"),
                                                div(
                                                  div(htmlOutput("title1a"),class="title"),
                                                  div(plotOutput("plot1a",height=500,width="auto"),class="plot"),
                                                  div(htmlOutput("caption1a"),class="caption caption1"),
                                                class="plotbox")
                                          ),
                                          tabPanel("Phase plane",
                                                   div(htmlOutput("text1b"),class="copy copy1"),
                                                div(
                                                  div(htmlOutput("title1b"),class="title"),
                                                  div(plotOutput("plot1b",height=500,width="auto"),class="plot"),
                                                  div(htmlOutput("caption1b"),class="caption caption1"),
                                                class="plotbox")
                                          )
                                        )
                                      )
                                    )),
                           
                           tabPanel("Phase Plane",value=2,id=2,
                                    fluidRow(
                                    
                                      column(4,id="side2",
                                      fluidRow(
                                        column(6,HTML("<h5>First trajectory</h5>")),
                                        column(6,HTML("<h5>Second trajectory</h5>"))
                                      ),
                                      fluidRow(
                                        column(12,class="header",HTML("<i>R</i><sub>0</sub>"))
                                      ),
                                      fluidRow(
                                        column(6,sliderInput("R021", NA,value = 3,
                                                                 min = 1, max = R0_max, step = R0_step)),
                                        column(6,sliderInput("R022", NA,value = 3,
                                                                 min = 1, max = R0_max, step = R0_step))
                                      ),
                                      fluidRow(
                                        column(12,class="header",HTML("Infectious period, 1/<i>&gamma;</i> (days)"))
                                      ),
                                      fluidRow(
                                        column(6,sliderInput("ip21",NA, value = ip_default,
                                                                 min = 1, max = ip_max, step = 1)),
                                        column(6,sliderInput("ip22",NA, value = ip_default,
                                                                 min = 1, max = ip_max, step = 1))
                                      ),
                                      fluidRow(
                                        column(12,class="header",HTML("Latent period, 1/<i>&sigma;</i> (days)"))
                                      ),
                                      fluidRow(
                                        column(6,sliderInput("lp21", NA, value = lp_default,
                                                                     min = lp_min, max = lp_max, step = 1)),
                                        column(6,sliderInput("lp22", NA, value = lp_default,
                                                             min = lp_min, max = lp_max, step = 1))
                                      ),
                                      fluidRow(
                                        column(12,class="header",HTML("Immunity duration 1/<i>&omega;</i> (years)"))
                                      ),
                                      fluidRow(
                                        column(6,sliderInput("id21", NA, value = id_default,
                                                             min = id_min, max = id_max, step = id_step)),
                                        column(6,sliderInput("id22", NA, value = 2*id_default,
                                                             min = id_min, max = id_max, step = id_step))
                                      ),
                                      fluidRow(
                                        column(12,class="header",HTML("Life expectancy 1/<i>&mu;</i> (years)"))
                                      ),
                                      fluidRow(
                                        column(6,sliderInput("le21", NA, value = le_default,
                                                             min = le_min, max = le_max, step = le_step)),
                                        column(6,sliderInput("le22", NA, value = le_default,
                                                             min = le_min, max = le_max, step = le_step))
                                      ),
                                      fluidRow(
                                        column(12,class="header",HTML("Disease death onset 1/<i>&alpha;</i> (days)"))
                                      ),
                                      fluidRow(
                                        column(6,sliderInput("al21", NA, value = al_default,
                                                             min = al_min, max = al_max, step = al_step)),
                                        column(6,sliderInput("al22", NA, value = al_default,
                                                             min = al_min, max = al_max, step = al_step))
                                      ),
                                      fluidRow(
                                        column(12,class="header",HTML("Vaccination level, <i>p</i> (%)"))
                                      ),
                                      fluidRow(
                                        column(6,sliderInput("p21", NA, value = 0,
                                                             min = 0, max = 99, step = 1)),
                                        column(6,sliderInput("p22", NA, value = 0,
                                                             min = 0, max = 99, step = 1))
                                      ),
                                      fluidRow(
                                        column(8,offset=2,id="buttons",
                                            checkboxInput("log2", HTML("log<sub>10</sub> axes"), TRUE),
                                            checkboxInput("points2", HTML("show time points"),   TRUE),
                                            checkboxInput("sir2",HTML("show SIR <i>I</i>(<i>t</i>) trajectory"),TRUE),
                                            checkboxInput("text2",HTML("interpretive text"),     TRUE),
                                            checkboxInput("captions2",HTML("figure captions"),   TRUE),
                                            actionButton("refresh2","Reset")
                                      )
                                      )
                                      ),
                                      column(8,id="main2",
                                                div(HTML("Exploring the SEIRS phase plane"),class="paneltitle"),
                                                div(htmlOutput("text2"),class="copy copy2"),
                                                div(
                                                  div(htmlOutput("title2a"),class="title"),
                                                  div(plotOutput("plot2a",height=600,width="auto"),class="plot"),
                                                  div(htmlOutput("caption2a"),class="caption caption2"),
                                                  class="plotbox")
                                      )
                                    )),
                           
                           tabPanel("Equations",value=2,id=2,
                                    withMathJax(
                                      helpText(paste("SEIRS system (dot indicates time derivative) $$",eq_seirs,"$$")),
                                      helpText(paste("Parameters (mean values) $$",eq_param,"$$")),
                                      helpText("Basic reproduction number $$",eq_r0,"$$"),
                                      helpText("Endemic mean age of infection $$",eq_endemic_age,"$$"),
                                      #helpText("Mean infectious period $$G_\\textrm{I} = 1/(\\gamma + \\mu + \\alpha)$$"),
                                      helpText(paste("Endemic equilibrium $$",eq_equil,"$$")),
                                      p(paste("The inter-epidemic period is calculated from the largest imaginary part of eigenvalues of the Jacobian matrix evaluated at the endemic equilibrium.")),
                                      helpText(paste("Jacobian matrix $$",eq_jacobian,"$$")),
                                      helpText(paste("Eigenvalue equation and inter-epidemic period $$",eq_eigenvalue,"$$")),
                                      helpText("Approximation of inter-epidemic period $$",eq_period,"$$")
                                      #,helpText(paste("Age-dependent model $$",eq_age,"$$"))
                                    ),
                                    
                                    p(paste("Infection trajectories show a numerical solution to the SEIRS equations with",formatC(sir_system_steps,format="f",big.mark=",",digits=0),"time steps and initial parameters",sep=" ")),
                                    withMathJax(
                                      helpText("$$S(0) = 0.999 - p$$"),
                                      helpText("$$E(0) = 0.001$$"),
                                      helpText("$$I(0) = 0$$"),
                                      helpText("$$R(0) = p$$"),
                                      helpText("$$S + E + I + R = N = 1$$")
                                    ),
                                    p("where",tags$i("p"),"is the vaccination fraction.")
                          ),
                           
                           tabPanel("Download & Credits",value=3,id=3,
                                    mainPanel(
                                      h3("Points of Significance: Adding realism to the SIR model for infectious disease epidemics"),
                                      p(HTML("Ottar Bjørnstad<sup>1,2</sup>, Katriona Shea<sup>1</sup>, Martin Krzywinski<sup>3*</sup>, Naomi Altman<sup>4</sup>")),
                                      div(
                                      p("1. Department of Biology, The Pennsylvania State University, State College, PA, USA."),
                                      p("2. Department of Entomology, The Pennsylvania State University, State College, PA, USA."),
                                      p("3. Canada’s Michael Smith Genome Sciences Center, Vancouver, British Columbia, Canada."),
                                      p("4. Department of Statistics, The Pennsylvania State University, State College, PA, USA."),
                                        class="affiliations"),
                                      p("*",tags$a(href="mailto:martink@bcgsc.ca",tags$i("martink@bcgsc.ca"))),

                                      hr(),
                                      h4("Download code"),
                                      p(tags$a(href="https://github.com/martinkrz/posepi2","https://github.com/martinkrz/posepi2")),
                                      
                                      hr(),
                                      h4("Citation"),
                                      p(HTML("Bjørnstad, O., Shea, K., Krzywinski, M. & Altman, N. Points of Significance: Adding realism to the SIR model for infectious disease epidemics. (2020) <i>Nature Methods</i> <b>17</b> (in press).")),
                                      
                                      h4("Background reading"),
                                      p(HTML("Bjørnstad, O., Shea, K., Krzywinski, M. & Altman, N. <a href='https://www.nature.com/articles/s41592-020-0822-z'>Points of Significance: Modelling infectious epidemics.</a> (2020) <i>Nature Methods</i> <b>17</b>:455&ndash;456.")),
                                      
                                      hr(),
                                      h4("Version history"),
                                      h5("22 April 2020"),
                                      p("Starting work."),
                                      h5("5 May 2020"),
                                      p("Refactored."),
                                      width=16
                                      
                                    ))
                           
                ))

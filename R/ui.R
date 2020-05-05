ui = fluidPage( theme=("css/style.css"),
                htmlOutput("masthead"),
                navbarPage("Modelling Infectious Epidemics",id="tabs",
                           
                           tabPanel("The SEIRS model",value=1,id=1,
                                    sidebarLayout(
                                      sidebarPanel(
                                        sliderInput("ip1", HTML("Infectious period, <i>ip</i> (days)"), 
                                                    value = ip_default,
                                                    min = 1, max = ip_max, step = 1),
                                        sliderInput("R01", HTML("<i>R</i><sub>0</sub>"), value = 3,
                                                    min = 1, max = R0_max, step = R0_step),
                                        sliderInput("lp1", HTML("Latent period, 1/<i>&sigma;</i> (days)"),
                                                    value = lp_default,
                                                    min = lp_min, max = lp_max, step = 1),
                                        sliderInput("id1", HTML("Immunity duration, 1/<i>&omega;</i> (years)"),
                                                    value = id_default,
                                                    min = id_step, max = id_max, step = id_step),
                                        sliderInput("le1", HTML("Life expectancy, 1/<i>&mu;</i> (years)"),
                                                    value = le_default,
                                                    min = 1, max = le_max, step = 1),
                                        sliderInput("vac", HTML("vaccination level, <i>p</i> (%)"), 0,
                                                    min = 0, max = 99, step = 1),
                                        checkboxInput("log1", HTML("log y-axis scale"), FALSE)
                                      ),
                                      mainPanel(h3("The SEIRS model of infection spread"),
                                                div(htmlOutput("text1"),class="copy"),
                                                div(
                                                div(htmlOutput("title1a"),class="title"),
                                                div(plotOutput("plot1"),class="plot"),
                                                div(htmlOutput("caption1"),class="caption"),
                                                class="plotbox")
                                      )
                                    )),
                           
                           # tabPanel(HTML("Effect of <i>R</i><sub>0</sub> mitigation"),value=2,id=2,
                           #          sidebarLayout(
                           #            sidebarPanel(
                           #              sliderInput("ip2", HTML("Infectious period, <i>ip</i> (days)"), value = ip_default,
                           #                          min = 1, max = ip_max, step = 1),
                           #              sliderInput("R0init", HTML("<i>R</i><sub>0</sub>"), 3,
                           #                          min = 1.1, max = R0_max, step = R0_step),
                           #              sliderInput("R0final", HTML("<i>R</i><sub>0</sub> with mitigation"), 2,
                           #                          min = 1, max = R0_max, step = R0_step),
                           #              sliderInput("capacity", HTML("Hospital capacity, <i>C</i> %"), 5,
                           #                          min = 1, max = 100, step = 1),
                           #              checkboxInput("show2", HTML("show intermediate trajectories"), FALSE)
                           #            ),
                           #            mainPanel(
                           #              h3(HTML("Effect of <i>R</i><sub>0</sub> mitigation on infection spread")),
                           #              div(htmlOutput("text2"),class="copy"),
                           #              div(
                           #              div(htmlOutput("title2a"),class="title"),
                           #              div(plotOutput("plot2a"),class="plot"),
                           #              div(htmlOutput("caption2a"),class="caption"),
                           #              class="plotbox"),
                           #              div(
                           #              div(htmlOutput("title2b"),class="title"),
                           #              div(plotOutput("plot2b"),class="plot"),
                           #              div(htmlOutput("caption2b"),class="caption"),
                           #              class="plotbox"),
                           #              div(
                           #              div(htmlOutput("title2c"),class="title"),
                           #              div(plotOutput("plot2c"),class="plot"),
                           #              div(htmlOutput("caption2c"),class="caption"),
                           #              class="plotbox"),
                           #              div(
                           #              div(htmlOutput("title2d"),class="title"),
                           #              div(plotOutput("plot2d"),class="plot"),
                           #              div(htmlOutput("caption2d"),class="caption"),
                           #              class="plotbox"),
                           #              div(
                           #              div(htmlOutput("title2e"),class="title"),
                           #              div(plotOutput("plot2e"),class="plot"),
                           #              div(htmlOutput("caption2e"),class="caption"),
                           #              class="plotbox"),
                           #              div(
                           #              div(htmlOutput("title2f"),class="title"),
                           #              div(plotOutput("plot2f"),class="plot"),
                           #              div(htmlOutput("caption2f"),class="caption"),
                           #              class="plotbox")
                           #            )
                           #          )),
                           # 
                           # tabPanel("Effect of vaccination",value=3,id=3,
                           #          sidebarLayout(
                           #            sidebarPanel(
                           #              sliderInput("ip3",HTML("Infectious period, <i>ip</i> (days)"), value = ip_default,
                           #                          min = 1, max = ip_max, step = 1),
                           #              sliderInput("R0v", HTML("<i>R</i><sub>0</sub>"), 2,
                           #                          min = 1, max = R0_max, step = R0_step)
                           #            ),
                           #            mainPanel(h3("Effect of vaccination on infection spread"),
                           #                      div(htmlOutput("text3"),class="copy"),
                           #                      div(
                           #                      div(htmlOutput("title3a"),class="title"),
                           #                      div(plotOutput("plot3a"),class="plot"),
                           #                      div(htmlOutput("caption3a"),class="caption"),
                           #                      class="plotbox"),
                           #                      div(
                           #                      div(htmlOutput("title3b"),class="title"),
                           #                      div(plotOutput("plot3b"),class="plot"),
                           #                      div(htmlOutput("caption3b"),class="caption"),
                           #                      class="plotbox"),
                           #                      div(
                           #                      div(htmlOutput("title3c"),class="title"),
                           #                      div(plotOutput("plot3c"),class="plot"),
                           #                      div(htmlOutput("caption3c"),class="caption"),
                           #                      class="plotbox"),
                           #                      div(
                           #                      div(htmlOutput("title3d"),class="title"),
                           #                      div(plotOutput("plot3d"),class="plot"),
                           #                      div(htmlOutput("caption3d"),class="caption"),
                           #                      class="plotbox")
                           #            )
                           #            
                           #          )),
                           
                           tabPanel("Equations",value=2,id=2,
                                    withMathJax(
                                      helpText("Susceptible $$\\frac{dS}{dt} = \\mu (N - S) - \\frac{\\beta I S}{N} + \\omega R$$"),
                                      helpText("Exposed $$\\frac{dE}{dt} = \\frac{\\beta I S}{N} - \\sigma E$$"),
                                      helpText("Infectious $$\\frac{dI}{dt} = \\sigma E - \\gamma I$$"),
                                      helpText("Recovered $$\\frac{dR}{dt} = \\gamma I - (\\omega + \\mu)R$$"),
                                      helpText("Recovery rate (can we say this?) $$\\gamma = \\frac{1}{\\text{infectious period}}$$"),
                                      helpText("Basic reproduction number $$R_0 =  \\frac{\\sigma}{\\sigma +\\mu} \\frac{\\beta}{\\gamma+\\mu}$$"),
                                      helpText("Equilibrium infected fraction $$I^* = \\frac{1-\\frac{1}{R_0}}{\\mu+\\gamma-\\frac{\\omega\\gamma}{\\omega+\\mu}}$$"),
                                      helpText("Inter-epidemic period $$T \\approx 4 \\pi \\sqrt{4 \\frac{R_0 -1}{G_I G_R} - \\left(\\frac{1}{G_R}-\\frac{1}{A}\\right)^2}$$"),
                                      helpText("Endemic mean age of infection $$A = \\frac{\\omega+\\mu+\\gamma}{(\\omega+\\mu)(\\beta-\\gamma-\\mu)}$$"),
                                      helpText("Mean infectious period $$G_I$$"),
                                      helpText("Mean duration of immunity $$G_R$$"),
                                      helpText("Age-dependent model $$\\begin{align*}

                                               \\frac{dS_1}{dt} &= \\underbrace{\\mu N}_\\textrm{birth} - \\underbrace{\\phi_1 S_1}_\\textrm{infection} - \\underbrace{\\nu_1 S_1}_\\textrm{vaccination} - \\underbrace{a_1 S_1}_\\text{aging} - \\underbrace{ \\mu S_1 }_\\text{death} 
                                               
                                               \\\\
                                               
                                               \\frac{dS_k}{dt} &= \\underbrace{a_{k-1}S_{k-1}}_\\text{aging in} - \\phi_k S_k -\\nu_k S_k - \\underbrace{a_k S_k}_\\text{aging out} - \\mu S_k
                                               
                                               \\\\ 
                                               
                                               \\frac{dI_k}{dt} &= a_{k-1}I_{k-1} + \\phi_k S_k - \\underbrace{\\gamma I_k}_\\text{recovery} - a_k I_k - \\mu I_k
                                               
                                               \\\\
                                               
                                               \\frac{dR_k}{dt} &= a_{k-1}R_{k-1} + \\gamma I_k + \\nu_k S_k - \\mu I_R
                                               
                                               \\\\
                                               
                                               \\phi_k &= \\sum_{j=1}^A \\frac{\\beta W_{ij} I_j}{N}
                                               \\end{align*}$$")
                                    ),
                                    p(paste("Infection trajectories show a numerical solution to the SEIRS equations with",formatC(sir_system_steps,format="f",big.mark=",",digits=0),"time steps and initial parameters",sep=" ")),
                                    withMathJax(
                                      helpText("$$S(0) = 0.999 - p$$"),
                                      helpText("$$E(0) = 0.001$$"),
                                      helpText("$$I(0) = 0$$"),
                                      helpText("$$R(0) = p$$"),
                                      helpText("$$S + E + I + R = N = 1$$")
                                    ),
                                    p("where",tags$i("p"),"is the vaccination fraction.")),
                           
                           tabPanel("Download & Credits",value=3,id=3,
                                    mainPanel(
                                      h3("Points of Significance: Modelling Infectious Epidemics"),
                                      p(HTML("Ottar Bjornstad<sup>1,2</sup>, Katriona Shea<sup>1</sup>, Martin Krzywinski<sup>3*</sup>, Naomi Altman<sup>4</sup>")),
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
                                      p(HTML("Bjornstad, O., Shea, K., Krzywinski, M. & Altman, N. Points of Significance: Modelling Infectious Epidemics (2020) <i>Nature Methods</i> <b>17</b> (in press).")),
                                      
                                      hr(),
                                      h4("Version history"),
                                      h5("22 April 2020"),
                                      p("Starting work."),
                                      width=16
                                      
                                    ))
                           
                ))
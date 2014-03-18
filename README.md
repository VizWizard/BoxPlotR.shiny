BoxPlotR
========

This is the repository for the Shiny application presented in "BoxPlotR: a web tool for generation of box plots" (Spitzer at al. 2014).

Installation
------------

You have two options for running shiny-boxplot:

1) Launch directly from R and GitHub:
  - Before running the app you will need to have R and RStudio installed (tested with R 3.0.2 and RStudio 0.97.449).
  - Launch the R console

- Please run these lines in R:
  - install.packages("shiny")
  - install.packages("devtools")
  - devtools::install_github("shiny-incubator","rstudio")
  - install.packages("beeswarm")
  - install.packages("vioplot")
  - install.packages("beanplot")
  - install.packages("RColorBrewer")

- Then start the app:
  - shiny::runGitHub("BoxPlotR.shiny", "VizWizard")

Your web browser will open the web app.

2) Install the shiny-server and implement shiny-boxplot as a web application and service:
  - In Ubuntu 12.04+
    - sudo apt-get install gdebi-core
    - wget http://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.0.0.42-amd64.deb (may need to change ubuntu or server version number)
	- sudo gdebi shiny-server-1.0.0.42-amd64.deb
	- edit: /opt/shiny-server/config/default.config in a text editor
	  - Change these lines to suit your environment
	  - listen **SHINY_PORT**; (change **SHINY_PORT** to match the port you want)
	  - site_dir **SHINY_APP_HOME**; (change **SHINY_APP_HOME** to the location for your shiny apps)
	- make sure **SHINY_PORT** is open on your firewall
	- Go to your **SHINY_APP_HOME**
	  - cd **SHINY_APP_HOME**
	- Get the latest shiny-boxplot code from github:
	  - wget https://github.com/jwildenhain/shiny-boxplot/archive/master.zip
	  - unzip master.zip
	  - mv shiny-boxplot-master shiny-boxplot
	- Restart shiny-server service:
	  - sudo service shiny-server restart

You should now be able to access shiny-boxplot at: http://YOURSITE:**SHINY_PORT**/shiny-boxplot

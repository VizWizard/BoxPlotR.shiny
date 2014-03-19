
shinyUI(pageWithSidebar(

	headerPanel("BoxPlotR: a web-tool for generation of box plots",
		tags$head(tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
		tags$style(type="text/css", "select { max-width: 200px; }"),
		tags$style(type="text/css", "textarea { max-width: 185px; }"),
		tags$style(type="text/css", ".jslider { max-width: 200px; }"),
		tags$style(type='text/css', ".well { max-width: 330px; }"),
		tags$style(type='text/css', ".span4 { max-width: 330px; }")) 
	),
  
	sidebarPanel(
		conditionalPanel(condition="input.tabs1=='About'",
			h4("Introduction")
		),
		conditionalPanel(condition="input.tabs1=='Data upload'",
			h4("Enter data"),
			radioButtons("dataInput", "", list("Load sample data"=1,"Upload file"=2,"Paste data"=3)),
			conditionalPanel(condition="input.dataInput=='1'",
				h5("Load sample data:"),
				radioButtons("sampleData", "Load sample data", list("Example 1 (100,76,16,76,41 data points)"=1,"Example 2 (3 columns with 100 data points)"=2))
			),
			conditionalPanel(condition="input.dataInput=='2'",
				h5("Upload delimited text file: "),
				fileInput("upload", "", multiple = FALSE),
				radioButtons("fileSepDF", "Delimiter:", list("Comma"=1,"Tab"=2,"Semicolon"=3)),#, "Space"=4))
				HTML('<p>Data in <a href="http://en.wikipedia.org/wiki/Delimiter-separated_values">delimited text files </a> can be separated by comma, tab or semicolon. 
				For example, Excel data can be exported in .csv (comma separated) or .tab (tab separated) format. </p>')
			),
			conditionalPanel(condition="input.dataInput=='3'",
				h5("Paste data below:"),
				tags$textarea(id="myData", rows=10, cols=5, ""),
				actionButton('clearText_button','Clear data'),
				radioButtons("fileSepP", "Separator:", list("Comma"=1,"Tab"=2,"Semicolon"=3))
			)
		),
		conditionalPanel(condition="input.tabs1=='Data visualization'",

			radioButtons("plotType", "", list("Boxplot"=0, "Other"=1)),
			conditionalPanel(condition="input.plotType=='1'",
				radioButtons("otherPlotType", "", list("Violin plot"=0, "Bean plot"=1)),
				textInput("myOtherPlotColours", "Colour(s):", value=c("light grey, white")),
				conditionalPanel(condition="input.otherPlotType=='0'",
					helpText("Colour of the 'violin area'"),
					textInput("violinBorder", "Border colour:", value=c("grey"))			
				),
				conditionalPanel(condition="input.otherPlotType=='1'",
					helpText("up to 4 colours can be specified: area of the beans, lines inside the bean, lines outside the bean, and average line per bean"),
					textInput("beanBorder", "Border colour:", value=c("grey")),	
					radioButtons("beanPlotMedianMean", "Display: ", list("Median"=0, "Mean"=1))
				)
			),

			h4("Plot options"),
			checkboxInput("plotDataPoints", "Minimum number of data points", FALSE),
			conditionalPanel(condition="input.plotDataPoints",			
				numericInput("nrOfDataPoints", "Data point limit: ", value=5, min=5)
			),

			conditionalPanel(condition="input.plotType=='0'",
				checkboxInput("showDataPoints", "Add data points", FALSE),
				conditionalPanel(condition="input.showDataPoints",
					radioButtons("datapointType", "", list("Default"=0, "Bee swarm"=1, "Jittered"=2)),
					textInput("pointColors", "Colour(s):", value=c("black"))					
				),
				checkboxInput("whiskerDefinition", "Definition of whisker extent", FALSE),
				conditionalPanel(condition="input.whiskerDefinition",
					radioButtons("whiskerType", "", list("Tukey"=0, "Spear"=1, "Altman"=2)),
#					conditionalPanel(condition="input.whiskerType=='0'",
#						numericInput("TukeyRange", "Define whisker extent (x IQR):", min=0, step=0.5, value=1.5)
#					),
#					conditionalPanel(condition="input.whiskerType=='1'",
#						HTML('<p>Spear - Whiskers extend to minimum and maximum values.</p>')
#					),
#					conditionalPanel(condition="input.whiskerType=='2'",
#						numericInput("AltmanRange", "Define whisker extent in percentiles (ie, '5' means that whiskers extend to 5th and 95th percentile):", min=0, step=0.5, value=5)
#					),
					HTML('<p>Tukey - whiskers extend to data points that are less than 1.5 x <a href="http://en.wikipedia.org/wiki/Interquartile_range">IQR</a> away from 1st/3rd <a href=:"http://en.wikipedia.org/wiki/Quartile">quartile</a>; 
					Spear - whiskers extend to minimum and maximum values; 
					Altman - whiskers extend to 5th and 95th percentile (use only if n>40)</p>')
				),
				checkboxInput("showNrOfPoints", "Display number of data points", FALSE),
				checkboxInput("addMeans", "Add sample means", FALSE),
				conditionalPanel(condition="input.addMeans",
					checkboxInput("addMeanCI", "Add confidence intervals of means", FALSE),
					conditionalPanel(condition="input.addMeanCI",
						radioButtons("meanCI", "Define confidence interval of means:", list("83%"=83, "90%"=90, "95%"=95))
					)				
				),
							
				checkboxInput("myVarwidth", "Variable width boxes", FALSE),
				helpText("Widths of boxes are proportional to square-roots of the number of observations."),
				checkboxInput("myNotch", "Add notches", FALSE),
				HTML('<p>+/-1.58*<a href="http://en.wikipedia.org/wiki/Interquartile_range">IQR</a>/sqrt(n) - gives roughly 95% confidence that two medians differ (Chambers et al., 1983)</p>'),
				conditionalPanel(condition="input.myNotch",
					HTML('<p>The notches are defined as +/-1.58*<a href="http://en.wikipedia.org/wiki/Interquartile_range">IQR</a>/sqrt(n) and represent the 95% <a href="http://en.wikipedia.org/wiki/Confidence_interval">confidence interval</a> for each median. 
					Non-overlapping notches give roughly 95% confidence that two medians differ, ie, in 19 out of 20 cases the population 
					medians (estimated based on the samples) are in fact different (Chambers et al., 1983).</p>')
				),
				textInput("myColours", "Colour(s):", value=c("light grey, white")),
				helpText("Colours in HEX format can be chosen on http://colorbrewer2.org/")
			),

			checkboxInput("labelsTitle", "Modify labels and title", FALSE),
			conditionalPanel(condition="input.labelsTitle",
				checkboxInput("xaxisLabelAngle", "Rotate sample names", FALSE),
				textInput("myXlab", "X-axis label:", value=c("")),
				textInput("myYlab", "Y-axis label:", value=c("")),
				textInput("myTitle", "Boxplot title:", value=c("")),
				textInput("mySubtitle", "Boxplot subtitle:", value=c(""))
			),
			checkboxInput("plotSize", "Adjust plot size", FALSE),
			conditionalPanel(condition="input.plotSize",
				numericInput("myHeight", "Plot height:", value=550),
				numericInput("myWidth", "Plot width:", value=750)
			),
			checkboxInput("fontSizes", "Change font sizes", FALSE),
			conditionalPanel(condition="input.fontSizes",
				numericInput("cexTitle", "Title font size:", value=14),
				numericInput("cexAxislabel", "Axis label size:", value=14),
				numericInput("cexAxis", "Axis font size:", value=12)
			),
			h5("Orientation of box plots:"),
			radioButtons("myOrientation", "", list("Vertical"=0, "Horizontal"=1)),
			conditionalPanel(condition="input.myOrientation=='0'",
				h5("Y-axis range (eg., '0,10'):"),
				textInput("ylimit", "", value="")
			),
			conditionalPanel(condition="input.myOrientation=='1'",
				h5("X-axis range (eg., '0,10'):"),
				textInput("xlimit", "", value="")
			),
			checkboxInput("logScale", "Change to log scale (only for data >0)", FALSE),			
			h5("Add grid: "),
			radioButtons("addGrid", "", list("None" = 0, "X & Y"= 1, "X only" = 2, "Y only" = 3))
#			numericInput("boxWidth", "Width of boxes:", value=1),
		)	
	),
  
	mainPanel(
		tabsetPanel(
			# Welcome tab
			tabPanel("About",
				HTML('<p>This application was developed with Nature Methods as described in this <a href="http://www.nature.com/nmeth/journal/v11/n2/full/nmeth.2837.html">editorial</a> and this 
				<a href="http://blogs.nature.com/methagora/2014/01/bring-on-the-box-plots-boxplotr.html">blog entry</a>. Nature methods also dedicated a 
				<a href="http://www.nature.com/nmeth/journal/v11/n2/full/nmeth.2807.html">Points of View</a> and a <a href="http://www.nature.com/nmeth/journal/v11/n2/full/nmeth.2813.html">Points of Significance</a> 
				column to box plots. We hope that you find the <a href= "http://www.nature.com/nmeth/journal/v11/n2/full/nmeth.2811.html">BoxPlotR</a> useful and we welcome suggestions 
				for additional features by our users. We would like to thank everyone who has made constructive suggestions so far. We will document the addition of new features in the News tab.</p>
				<p>This application allows users to generate customized <a href="http://en.wikipedia.org/wiki/Box_plot">box plots</a> in a number of variants based on their data. A data matrix 
				can be uploaded as a file or pasted into the application. Basic box plots are generated based on the data and can be modified to include 
				additional information. Additional features become available when checking that option.  Information about sample sizes can be represented 
				by the width of each box where the widths are proportional to the square roots of the number of observations n. Notches can be added to the 
				boxes. These are defined as +/-1.58*<a href="http://en.wikipedia.org/wiki/Interquartile_range">IQR</a>/sqrt(n) which gives roughly 95% confidence that two medians are different. It is also possible to define 
				the whiskers based on the ideas of Spear and Tukey. Additional options of data visualization (violin and bean plots) reveal more information 
				about the underlying data distribution. Plots can be labeled, customized (colors, dimensions, orientation) and exported as eps, pdf and svg files.</p>
				<p>BoxPlotR code can be run locally via <a href="https://github.com/VizWizard/BoxPlotR.shiny/blob/master/README.md">GitHub</a>. You can also download and install it as a virtual machine (see <a href="https://github.com/VizWizard/BoxPlotR.shiny/blob/master/README.md">GitHub</a> and FAQs for details). </p>'),
				h5("Software references"),
				HTML('<p>R Development Core Team. <i><a href="http://www.r-project.org/">R</a>:  A Language and Environment for Statistical Computing.</i> R Foundation for Statistical Computing, Vienna (2013) <br>
				RStudio and Inc. <i><a href="http://www.rstudio.com/shiny/">shiny</a>: Web Application Framework for R.</i> R package version 0.5.0 (2013) <br>
				Adler, D. <i><a href="http://cran.r-project.org/web/packages/vioplot/index.html">vioplot</a>: Violin plot.</i> R package version 0.2 (2005)<br>
				Eklund, A. <i><a href="http://cran.r-project.org/web/packages/beeswarm/index.html"> beeswarm</a>: The bee swarm plot, an alternative to stripchart.</i> R package version 0.1.5 (2012)<br>
				Kampstra, P. <i><a href="http://cran.r-project.org/web/packages/beanplot/index.html">Beanplot</a>: A Boxplot Alternative for Visual Comparison of Distributions.</i> Journal of Statistical Software, Code Snippets 28(1). 1-9 (2008) <br>
				Neuwirth, E. <i><a href="http://cran.r-project.org/web/packages/RColorBrewer/index.html">RColorBrewer</a>: ColorBrewer palettes.</i> R package version 1.0-5. (2011)</p>'),
				h5("Further references"),
				HTML('<p> Hadley Wickham and Lisa Stryjewski: <a href="http://vita.had.co.nz/papers/boxplots.pdf"> 40 years of boxplots </a></p>'),
				HTML('<p> Kristin Potter: <a href="http://pages.uoregon.edu/kpotter/publications/potter-2006-MPSI.pdf">Methods for Presenting Statistical Information: The Box Plot</a></p>'),
				h6("This application was created by the ", a("Tyers", href="http://tyers.iric.ca/"), " and ", a("Rappsilber", href="http://rappsilberlab.org/"), 
				" labs. Please send bugs and feature requests to Michaela Spitzer (michaela.spitzer(at)gmail.com) and Jan Wildenhain (jan.wildenhain(at)gmail.com). This application uses the ", 
				a("shiny package from RStudio", href="http://www.rstudio.com/shiny/"), ".")
			),
			# Data upload tab
			tabPanel("Data upload", tableOutput("filetable"),
				h6("This application was created by the ", a("Tyers", href="http://tyers.iric.ca/"), " and ", a("Rappsilber", href="http://rappsilberlab.org/"), 
				" labs. Please send bugs and feature requests to Michaela Spitzer (michaela.spitzer(at)gmail.com) and Jan Wildenhain (jan.wildenhain(at)gmail.com). This application uses the ", 
				a("shiny package from RStudio", href="http://www.rstudio.com/shiny/"), ".")
			),
			# Boxplot tab
			tabPanel("Data visualization", downloadButton("downloadPlotEPS", "Download eps-file"),
				downloadButton("downloadPlotPDF", "Download pdf-file"),
				downloadButton("downloadPlotSVG", "Download svg-file"),
				plotOutput("boxPlot", height='100%', width='100%'),
				h4("Box plot statistics"), tableOutput("boxplotStatsTable"),
				h6("This application was created by the ", a("Tyers", href="http://tyers.iric.ca/"), " and ", a("Rappsilber", href="http://rappsilberlab.org/"), 
				" labs. Please send bugs and feature requests to Michaela Spitzer (michaela.spitzer(at)gmail.com) and Jan Wildenhain (jan.wildenhain(at)gmail.com). This application uses the ", 
				a("shiny package from RStudio", href="http://www.rstudio.com/shiny/"), ".")
			), 
			# Figure legend 
			tabPanel("Figure legend template", h5("Box plot description for figure legend:"), textOutput("FigureLegend"),
				h5("Further information to be added to the figure legend:"), p("What do the box plots show, explain colours if used."),
				downloadButton("downloadBoxplotData", "Download box plot data as .CSV file"),
				h6("This application was created by the ", a("Tyers", href="http://tyers.iric.ca/"), " and ", a("Rappsilber", href="http://rappsilberlab.org/"), 
				" labs. Please send bugs and feature requests to Michaela Spitzer (michaela.spitzer(at)gmail.com) and Jan Wildenhain (jan.wildenhain(at)gmail.com). This application uses the ", 
				a("shiny package from RStudio", href="http://www.rstudio.com/shiny/"), ".")
			),
			# News
			tabPanel("News",
				h5("March 18, 2014"), 
				p("The user can now choose the color of the data points. There is also an additional option for data point display: data points can now be randomly jittered. 
				A small bug in label display was fixed. Log scales can now be used.")
			),			
			# FAQ 
			tabPanel("FAQ",
				h5("Q: I have trouble editing the graphic files."), 
				p("A: For EPS files make sure to 'ungroup' all objects so they can be edited independently. 
				In Adobe Illustrator you will also need to use the 'release compound path' command. For PDF 
				files you should 'release clipping mask'. SVG import appears to have problems in Adobe Illustrator 
				and Corel Draw and should be avoided. EPS, PDF and SVG import all work with Inkscape http://www.inkscape.org/."),
				h5("Q: I would like to install BoxPlotR as a virtual machine."), 
				HTML('<p>A: Please download the virtual machine from http://tyerslab.bio.ed.ac.uk/download/shiny.boxplot.7z (1.1GB). 
				Unzip the file using <a href="http://www.7-zip.org/">7zip</a> or equivalent. The virtual machine is available in the open 
				virtualization format (OVF) and you can use this file with <a href="https://www.virtualbox.org/">vbox</a> and 
				<a href="https://my.vmware.com/web/vmware/free#desktop_end_user_computing/vmware_player/6_0">vmware player</a>. 
				One easy way to use the server is to set the virtual host network environment to NAT. After importing the virtual machine you can 
				start the server and login as user shiny  with password pk635153Y6jx89r. On the command line use the command ifconfg and record the 
				IP address of the virtual server. Now going back to the virtual host network environment change the advanced settings of the NAT and 
				activate port forwarding for the guest network ip address (shiny server) using port 80 to the host ip address using for example port 8080. 
				Now you should be able to access the shiny server in a browser on port 127.0.0.1:8080 or your localhost:8080.
				If you are not familiar with the software packages there are detailed examples for 
				<a href="http://www.howtogeek.com/122641/how-to-forward-ports-to-a-virtual-machine-and-use-it-as-a-server/">vbox</a> and 
				<a href="http://blog.fardad.com/2012/06/vmware-player-and-custom-nat-port-map.html">vmware</a>.</p>')
#				h5("Q:"), 
#				p("A:")
			),			
			id="tabs1"
		)
	)
))








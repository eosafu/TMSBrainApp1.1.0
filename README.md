## TMSBrainApp
![LOGOS](https://user-images.githubusercontent.com/70357973/128624809-6b29adb1-57c9-42b8-baef-772caa73468a.jpg) 
``TMSBrainApp`` is an ``R`` Package that contains a user-friendly Shiny interface for analyzing Transcranial Magnetic Stimulation (TMS) mapping data. This interface allows users access to an application that utilizes a Bayesian spatial statistical modeling technique, making it a powerful tool for performing brain mapping in ``R`` without the need for coding expertise. With TMSBrainApp, users can easily analyze brain activity data, making it an invaluable resource for researchers and clinicians. For demostration of the interface see here https://youtu.be/HaIw7L0VEc0.

### Package Installation Instruction
Required tools

- Running the package requires a working R environment. Ensure that your machine has ``R`` (>=3.5.0) installed. Otherwise, download and install from https://cran.r-project.org/.  
- For Windows users, please ensure that "Rtools" is installed. Otherwise, download from https://cran.r-project.org/bin/windows/Rtools/rtools40.html. 

*Step 1*: Manually install ``INLA`` package (version 22.04.16 or more recent) from its repository (https://www.r-inla.org/download-install): 
```
		R > install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
```
or upgrade:
```
                R > inla.upgrade()
```
*Step 2*: On ``R`` interface, install "devtools" package from CRAN repository:  
```
		R > install.packages("devtools")  
```
*Step 3*: Intall ``TMSBrainApp`` package from Github repository:  
```
		R > devtools::install_github("eosafu/TMSBrainApp1.1.0")  
		R > library(TMSBrainApp) 
```
*Step 4*: Execute the TMSBrainApp application:  
```
		R > TMSapp.run()  
```
 
Navigate through the interface by clicking on the appropriate tabs. One can as well choose to work on a web browser rather than the R shiny interface.  To do this: on the ``R`` shiny interface, click on "Open in Browser".

A sample of TMS Brain data is available for practice.

The ``TMSBrainApp1.1.0`` package contains well documented functions used internally by the Shiny application, which are also made available for users. For example, use 

```
		R > ?QuantResidTMS()
```
to view the documentation for ``QuantResidTMS`` function.
## Functions
* For analyzing any spatial data
* Make statistical inference and predictions based on Bayesian methods
* Detect hotspots and provide the posterior distribution of the hotspot for post-modeling inference
* Prepare summary report of analysis performed

## Screenshot of its page on R
![function Summary](https://github.com/eosafu/TMSBrainApp/blob/main/Rsummary.png)
## Screenshot of the App
![Example analysis](https://raw.githubusercontent.com/eosafu/TMSBrainApp/main/TMSBrainApp1.1.0.png)

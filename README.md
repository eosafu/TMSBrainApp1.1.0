## TMSBrainApp
![LOGOS](https://user-images.githubusercontent.com/70357973/128624809-6b29adb1-57c9-42b8-baef-772caa73468a.jpg)
TMSBrainApp is an ``R`` Package for TMS brain mapping, adopting a Bayesian spatial statistical modeling. The package contains a shiny user interface, where one could perform Brain mapping in ``R`` without writing codes.  


### Package Installation Instruction
Required tools

- Ensure that your machine has ``R`` (>=3.5.0) installed. Otherwise, download and install from https://cran.r-project.org/.  
- For Windows users, please ensure that "Rtools" is installed. Otherwise, download from https://cran.r-project.org/bin/windows/Rtools/rtools40.html. 

*Step 1*: Manually install ``INLA`` package (version 21.11.22 or more recent) from its repository (https://www.r-inla.org/download-install): 
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
		R > library("devtools") 
```
*Step 3*: Intall ``TMSBrainApp`` package from Github repository:  
```
		R > install_github("eosafu/TMSBrainApp1.1.0")  
		R > library(TMSBrainApp) 
```
*Step 4*: Execute the TMSBrainApp application:  
```
		R > TMSapp.run()  
```

Navigate through the interface by clicking on the appropriate tabs. One can as well choose to work on a web browser rather than the R shiny interface.  To do this: on the ``R`` shiny interface, click on "Open in Browser".

A sample of TMS Brain data is available for practice.

The ``TMSBrainApp1.1.0`` package contains functions which are well documented. For example, use 

```
		R > ?QuantResidTMS()
```
to view the documentation for ``QuantResidTMS`` function.

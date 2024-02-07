# Global Family Planning Estimation Model (FPEMglobal)

FPEMglobal is an _R_ package for producing estimates and projections of contraceptive use and unmet need for family planning among married, unmarried, and all women. 

This package was developed by the _Family Planning Research_ (FPR) Group which is comprised of teams from the [Alkema Lab](https://leontinealkema.github.io/alkema_lab/) at the University of Massachusetts, Amherst, the [Population Division](https://www.un.org/en/development/desa/population/theme/making-family-planning-count/index.asp) of the United Nations Dept. of Economic and Social Affairs, and [Track20](http://www.track20.org/) at Avenir Health. It was supported, in part, by grants nos. OPP1110679 and OPP1183453, Making Family Planning Count, from the Bill & Melinda Gates Foundation. The package vignette has more information (`vignette("FPEMglobal_Intro")`).

The views expressed herein are those of the authors and do not necessarily reflect the views of the United Nations.


## Installation

Ensure you have installed *both* [_R_](https://cran.r-project.org/index.html) and [_JAGS_](http://mcmc-jags.sourceforge.net/). FPEMglobal has been tested with _R_ v4.0.0--v.4.3.1 (64 bit) and _JAGS_ v4.3.0--v4.3.1. 


Follow one of the following three options:


### 1. Straight from GitHub

1. Make sure you have installed the _R_ package [remotes](https://cran.r-project.org/package=remotes). 
2. In _R_, type:

    ```
    remotes::install_github(repo = "https://github.com/FPcounts/FPEMglobal", 
	                        ref = remotes::github_release(), 
                            build_manual = TRUE, build_vignettes = TRUE, dependencies = TRUE)
    ```

    `repo` is the only mandatory argument. If you encounter any problems you can try omitting any, or all, of the others. If you do not supply the `ref` argument, or if you change it, you may end up installing an old release or one that is in development and not fully tested. 



### 2. From the .zip File (Windows Only)

1. Go to the [release page](https://github.com/FPcounts/FPEMglobal/releases)
2. Download the zip file for the latest release (at the top of the list). It will have a filename of the form "FPEMglobal_x.y.z.zip" where "x.y.z" are numbers indicating the version. If the zip file is not visible, click "Assets". 
3. Install the package: launch _R_ and type:

    ```
    install.packages("[path/to/FPEMglobal_<x.y.z>.zip]", repos = NULL)
    ```

    You will need to edit the first argument to ensure you enter the correct file path to the zip file. `<x.y.z>` should be replaced by the actual version number. You will also need to install any package depencencies according to the notices that come up.



### 3. From Source

You will need to have basic knowledge of _Git_, _GitHub_, and the command line interface on your system to use this option. 

1. Clone the repository to your local drive.
2. Open a terminal or command prompt window in the directory containing the cloned repository (the directory containing the directory "FPEMglobal").
3. Issue the following commands:
    ```
	R CMD build FPEMglobal
	R CMD INSTALL FPEMglobal_<x.y.z>.tar.gz
    ```
	In the last command, `<x.y.z>` should be replaced by the actual version number. 
	
	If you get errors related to the vignette, try modifying the first command as follows: `R CMD build --no-build-vignettes FPEMglobal`. If you are on Windows and you get an error after a line of output containing "i386" , try modifying the second command as follows: `R CMD INSTALL --no-multiarch FPEMglobal_<x.y.z>.tar.gz` (replacing `<x.y.z>` accordingly). If you still have problems, follow the instructions above to install from the zip file. 
	
	
	
## Documentation

The vignette "Introduction to FPEMglobal" explains the package and how to use it. From within _R_ type:

```
vignette("FPEMglobal_Intro")
```

The vignette will not be available if you excluded it by omitting the `build_vignettes` argument in installation option (1) or adding `--no-build-vignettes` in option (3).

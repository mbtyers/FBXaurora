# FBXaurora 

### Machine Learning Aurora Alerts for Fairbanks, AK

The decision to go to sleep can seem like a gamble for an aurora enthusiast.  What if I miss a mind-blowing light show?  This package presents a slightly kludgy solution to this conundrum.

This package pulls image data from two excellent webcams, and applies a trained regression tree to features of both images.  Previous imagery has been classified according to the following scheme:

- 0: No aurora activity
- 1: Aurora activity, but nothing special
- 2: Get out the camera, if it's a weekend
- 3: Get out the camera, even if it's a weeknight
- 4: Highlight of the winter!

If the numerical rating of either image is above a given threshold, an email alert will be sent to the user.

### Commonly-used functions

* `run_all_night()` is probably the only function the user will actually call, with all others running in the background.  This function sets up R to pull the imagery from both webcams at a set time interval (defaults to every 5 minutes) and send alerts to the specified email address.

### Installation

The 'FBXaurora' package is currently available on Github, and can be installed in R with the following code:

`install.packages("devtools",dependencies=T")`

`devtools::install_github("mbtyers/FBXaurora")`

### Issues

It's probably got issues.  This was written as a standalone script during the winter, and converted to a package during the summer - hence, I haven't been able to test it.  

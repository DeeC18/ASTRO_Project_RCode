# See https://ggplot2-book.org/
# Also see https://www.rstudio.com/resources/cheatsheets/
# They recommend the following installations
install.packages(c("RColorBrewer", "dplyr", "ggforce", "gghighlight", 
  "ggnewscale", "ggplot2", "ggraph", "ggrepel", "ggtext", "ggthemes", 
  "hexbin", "Hmisc", "mapproj", "maps", "munsell", "ozmaps", 
  "paletteer", "patchwork", "rmapshaper", "scico", "seriation", "sf", 
  "stars", "tidygraph", "tidyr", "wesanderson" 
))
Yes
###########################################################
# Then WAIT FOR THEM TO FINISH INSTALLING
# to use any of these packages, 
# issue either the "library(packagename)" or
#"require(packagename)" command
library(ggplot2)
library(RColorBrewer)

#####################################################
# SET THE FOLLOWING TO YOUR WORKING DIRECTORY
setwd("~/Desktop/ASTRO 115 Project")

# Read in csv file from Gaia archive, the basic file dat
# read.csv creates a data frame with rows=observations
# and columns = variables
dat = read.csv("Gaia_Archive.csv")
str(dat)

# Convert the source_id variable to a character variable
# Variables (columns) are denoted by 
# datafilename$variablename. Here the data frame is "dat"
# and the variable is "source_id"
dat$source_id=as.character(dat$source_id)
str(dat)

# The following command, which you must modify, 
# creates a variable in the dat dataframe called
# Distancepc. You must modify the equation below
###########################################################
# INSERT A NUMBER OR EXPRESSION BETWEEN THE PARENTHESES BELOW 
dat$Distancepc = (1*10^3)/(dat$parallax)

# Since we know the distance, we know the absolute
# magnitude, no? Modify the expression below to 
# define another variable (column) in dat. Look at 
# dat to see if it looks reasonable.
##########################################################
# SUBSTITUTE FOR THE XXXs IN THE FOLLOWING & CHECK DAT
dat$Magnitude = dat$phot_g_mean_mag - 5 * log10(dat$Distancepc/10)

# An initial plot. We'll plot bp_rp, which is blue
# minus red (very red) Gaia color, vs Magnitude, which you just
# defined. The first line of ggplot sets the stage
# with x-y and axes and a background and grid, no
# data. Add a plus sign to add another layer (line) 
# to the plot. Here we plot the data as points, so geom_points.
# alpha = 0.1 gives transparency so we can see data
# that is overlaid. 
# You can play with alpha to get what kind of plot you want. 
# Completely opaque symbols can lose information.
# The plus sign at the end of the line adds the next line to
# the plot
ggplot(data = dat, aes(x=bp_rp, y=Magnitude)) +
  geom_point(alpha = .1) 

# That doesn't look like an HR diagram. What's wrong? 

# Try resetting x, y limits. To insert two
# limits, you must specify a "vector", c(xlo,xhi)
# or c(ylo, yhi). Also insert an X label and Y label
# to describe what we're plotting.
#############################################################
# INPUT VALUES FOR XXX AND AXIS LABELS
ggplot(data = dat, aes(x=bp_rp, y=Magnitude)) +
  geom_point(alpha = 0.1) +
  ylim(c(15,38)) + 
  xlim(c(-1,7)) +
  xlab("Blue - Red") +
  ylab("Magnitude")

###################################################################
# Teff insertion. This formula is from Mucciarelli & Bellazzini 2020
# for Gaia DR2. This expression is good for Gbp-Grp < 1.5; going 
# redder would require 2mass K colors. This will be good for 
# distinguishing spectral types less than K.
dat$logTeff = log10(5040.) - 
  log10(0.4988 + 0.4925*dat$bp_rp - 0.0287*(dat$bp_rp)^2)

################# SEPARATING SPECTRAL CLASSES ################
# Next, we'll label main sequence and white dwarfs to
# distinguish them, and separate main sequence classes.
# This adds another variable (column) to dat, called 
# "classif", a character variable

# The MS looks weird. Let's separate out the fainter stars,
# below ~16-17, but not WD, into a 3rd class, "LT"

# The following was a tricky piece of vectorized code! 
# Vectorized Boolean statement uses single & and not &&
# To vectorize if must use an ifelse statement.
# Do this by Teff and below bp-rp 1.5, use color. The 
# K/M cutoff is unreliable since the Gaia colors don't
# represent super red stars well. Here use V-I color of
# 2.19 for M0 and translate to Gbp-rp ~ 3 from Gaia docs
# This statement is tricky so I won't make you do it. 
# Cutoff for LT also somewhat arbitrary, guided by well-defined
# WD sequence to the blue, but to the red, overlap with M dwarfs
# Gaia filters not the best for very red stars, do not trust this
# separation. Cool stars need IR.
# Format of ifelse is: ifelse(logical test, TRUE, FALSE)
dat$classif = with(dat,
  ifelse(dat$Magnitude>9 & dat$bp_rp<.9, "WD", 
  ifelse(dat$Magnitude>15.7 & dat$bp_rp < 3, "LT", 
  ifelse(dat$Magnitude>15. & dat$bp_rp < 4, "LT", 
  ifelse(dat$Magnitude>16 & dat$bp_rp < 4.5, "LT",
  ifelse(dat$Magnitude > 17 & dat$bp_rp>=3, "LT",
  ifelse(dat$Magnitude>12.5 & dat$bp_rp < 1.8, "WD",
  ifelse(dat$logTeff>3.99, "A",
  ifelse(dat$logTeff>3.86, "F",
  ifelse(dat$logTeff>3.77, "G",
  ifelse(dat$bp_rp<3., "K","M")))))))))))

# Subset out main sequence and other classes for future use. 
mainseqlt = subset(dat, (classif !="WD"))
mainseq = subset(mainseqlt, (classif !="LT"))
ltdwarfs = subset(dat, classif == "LT")
whitedwarfs = subset(dat, classif == "WD")

# We now have five datasets: dat, which contains all 
# nearby stars,  mainseqlt, which excludes white dwarfs,
# mainseq, which excludes WD and LT, and white dwarfs.
# They should be visible in the "Global Environment".

# Since you have already looked at white dwarfs, I have 
# commented out the next plot, but you could do this
# again, especially if you used a small parallax cut.
#########################################################
# INSERT VALUES FOR X, Y LIMITS AND AXIS LABELS
#ggplot(data = whitedwarfs, aes(x=bp_rp, y=Magnitude)) +
#  geom_point(alpha = 0.2, pch=10) +
#  ylim(c(XXX,XXX)) + 
#  xlim(c(XXX,XXX))+
#  xlab("XXXr") +
#  ylab("XXX")

# Plot the main sequence
########################################################
# INSERT VALUES FOR X, Y LIMITS AND AXIS LABELS
ggplot(data = mainseq, aes(x=bp_rp, y=Magnitude)) +
  geom_point(alpha = 0.2) +
  ylim(c(15,38)) + 
  xlim(c(-1,7))+
  xlab("Blue - Red") +
  ylab("Magnitude")

# Now the megaplot
# Let's plot the entire sample, and this time
# use color to separate the classes
#######################################################3
# YOU NEED TO INSERT VALUES FOR "XXX"
# AND PICK A COLOR PALETTE FOR COLOR BREWER. G**GLE IT.

starplot = ggplot(data = dat, aes(x=bp_rp, y=Magnitude)) # basic plot
starplot = starplot +
  geom_point(aes(color=classif), alpha=.4) + 
  ylim(c(20,-10)) + 
  xlim(c(-1,7)) +
  xlab("Blue - Red") +
  ylab("Magnitude") +
  ggtitle("HR Diagram")  +
  scale_color_brewer(palette="Paired")
starplot  # displays final plot

############################################################
#                SAVE THIS PLOT#1                          #
############################################################


###########################  ANALYSIS #############################
# First, let's consider the distribution in distance, and
# the mean density of stars.

# Histograms can also be done with ggplot2 but hist() is easy to remember
# One could do a histogram as a proportion not frequency
# hist(dat$Distancepc, freq=F) But here we'll look at counts (frequency).
hist(dat$Distancepc)

#  Now a cumulative histogram:  this sums the prior bins
# to give the number of stars within the radius specifed
# by the bin. It shows how the number of stars grows with distance
disthist <- hist(dat$Distancepc, xlab="distance (pc)",
                 main="Cumulative Numbers of Stars with Distance
                 from Sol")
disthist$counts <- cumsum(disthist$counts)
plot(disthist, xlab="distance (pc)",
     main="Cumulative Numbers of Stars with Distance
                 from Sol")

# Next let's plot an analytical function that should
# represent the increase in the total (cumulative)
# number of stars with distance.
##################################################################
# HERE YOU MUST PUT IN A NUMBER FOR a) THE POWER XXXX IN THE CURVE
# EQUATION AND b) THE LARGEST DISTANCE XXXX IN YOUR SAMPLE.
# JUSTIFY THE POWER LAW VALUE IN YOUR PAPER. WHY IS THIS
# VALUE EXPECTED? (Question: where did the 903 and 14.28 come
# from? Could you provide other values and get a slightly better
# FIT?) NOTE THAT THE SINGLE "X" IS PART OF THE IMPLICIT X SEQUENCE 
# IN THE CURVE EQUATION, LEAVE IT
curve((903*(x/14.28)^3), from = 2, to = 25, col="red", add=T)

#to the 3rd power becuase of spherial volume radius goes as r^3

#################################################################
#                    SAVE THIS PLOT#2                           #
#################################################################

# Now let's look at the relative numbers of stars of
# different spectral types. Consider main sequence stars only, 
# so use the data frame main sequence. 

# First, a histogram of absolute G magnitude. We'll redo
# this later, but this is a first look.
hist(mainseqlt$Magnitude)

# Now break this up into spectral types. You've already
# categorized these in the data frame.
# With ggplot you can plot a bar chart with 
# relative numbers of stars. Divide these numbers by the total
# volume to get stellar density.

###########################################################
# FOR YOUR PLOT YOU MUST PICK A STANDARD R PLOT COLOR
# AND SPECIFY THE FILL FOR THE BOXES BY SUBSTITUTING FOR
# THE XXXX. USE G**GLE FOR "BASE" PLOT COLORS.
numStars = ggplot(dat, aes(x=classif)) +
  geom_bar(fill="darksalmon", alpha=0.4) +
  stat_count(geom="text", aes(label=..count..)) +
  labs(x="Spectral Type")
numStars

#################################################################
#                    SAVE THIS PLOT#3                           #
#################################################################

## Now let's attempt to look at distribution of spectral type 
# and masses; a present-day mass function, PDMF.

# First we need to somehow assign masses to these stars.

# We'll assign masses by using "calibrators" from the low mass star
# list of Mann+13. They determined masses for this sample of binaries. 
# We can then correlate mass and magnitude and thus predict masses for
# our Gaia sample based on Gaia G magnitudes. I've added to the table of
# magnitude & color data in the Mann data the Gaia magnitudes & colors  
# sources from the Gaia archive. The Mann reference has mass, L, Teff, 
# so we're now in a position to relate these to Gaia magnitudes & colors.

# The file that has the Mann data with my Gaia additions is called 
# "redlist.csv".  CHECK THE WORKING DIRECTORY 
redstars = read.csv("redlist.csv")
redstars = na.omit(redstars)

# First let's see if mass correlates with Gaia G magnitude.
# The binaries complicate M_G but only by <~ factor 2 in luminosity.
plot(redstars$Mtot~ redstars$M_G)


#(**** SHOULD WE FLIP AXES HERE??? IE MAGNITUDE ON Y AXIS****)
#(***************************************)



# This is a pretty good linear correlation, although not perfect at 
# the low end.  Let's do a linear regression fit using lm(). It will 
# be saved to a "list" object "massmodel"
massmodel= lm(redstars$Mtot~ redstars$M_G)
abline(lm(redstars$Mtot~ redstars$M_G))
summary(massmodel)

# The summary gives an intercept and slope, and indicates
# with stars the significance of the correlation. Three stars
# mean it is very significant. We already know it is
# significant. However, if we fit to the current data it will
# give negative masses for the faintest stars. Instead the curve
# seems to flatten out at the low mass end.

# An alternative to the linear fit is to do a local smoothing 
# using neighboring points. This might predict masses better for the 
# fainter stars, where the mass seems to flatten out.

# "loess" means "locally weighted scatterplot smoothing". The
# reason to use it rather than a 2 parameter linear fit, is
# that we have local data points that are reality, and also that 
# we don't a priori expect that a linear function is expected. 

# We'll just eyeball the loess fit here. We would test various
# smoothing kernels and do a more comprehensive statistical analysis 
# if we were writing a journal paper. But first let's see how it looks.
# Pick your own colors XXX
plot(Mtot ~M_G, data=redstars, col="blue")
with(redstars, lines(loess.smooth(M_G, Mtot),col="red"))

 #################################################################
#                    SAVE THIS PLOT#4                           #
#################################################################

# While we can't justify every wiggle (we could set some 
# parameters to have a longer smoothing distance but not
# worth the time for our purposes), the loess fit looks decent. 
# So we'll save a model to make predictions from. 
massmodel2=loess(Mtot ~ M_G,redstars,
                 control = loess.control(surface="direct"))

# Use the model to create a new column in the
# mainseqlt dataframe with predicted masses.
mainseqlt$mass = predict(massmodel2, mainseqlt$Magnitude)

# Plot a histogram of masses using hist(). y-axis is counts
# Save the histogram object, which is a list.

## SET THE NUMBER OF BINS HERE. PLAY WITH IT, ALTHOUGH
# 30 IS GOOD. ADD LABELS IF YOU WANT using previous examples
nBins = 30
magCounts = hist(mainseqlt$Magnitude, breaks = nBins)

#################################################################
#                    SAVE THIS PLOT#5                           #
#################################################################


########################### FITTING AN IMF ######################
# Now we're in a position to do a mass function, a present
# day mass function (PDMF). For lower mass stars, this is also 
# the IMF, since they have not had time to evolve off the
# main sequence. massCounts stores the fit of the histogram, a list.
massCounts = hist(mainseqlt$mass, breaks= nBins)

# IMFs or PDMFs are usually represented by a power law.
# We're going to fit this power law.

# Start by saving the "mids" and "counts" variables in either
# histogram to have a numerical table of histogram values. 
# The output of lm() is a list, which is difficult to work with 
# since it contains dissimilar items. However, "mids" 
# (= mass bin midpoints) and "counts" from the massCounts 
# histogram model have the same length and can be 
# column-bound (cbind()) into a new data frame for plotting)

# the new data frame, containing the mass bins and counts
# within them is "countsMassbins"
countsMassbins = as.data.frame(cbind(massCounts$mids, 
                                   massCounts$counts))
# For convenience rename the column headers
colnames(countsMassbins) = c("massbin", "counts")
plot(countsMassbins$counts ~ countsMassbins$massbin)

# Add two other columns to countsMassbins with 
# logcounts and logbins & plot them
countsMassbins$logcounts = log10(countsMassbins$counts)
countsMassbins$logbin = log10(countsMassbins$massbin)

plot(countsMassbins$logcounts ~ countsMassbins$logbin)

# We want to fit only the points with mass >= 0.5 Msun
######################################################
# INSERT CODE HERE TO DO A LINEAR REGRESSION FIT
# to logcounts ~ logbin, using data in countsMassbins,
# for massbin > .45
# name this model "salpeter fit" and so a summary

#***
#DATA LINE 25 IN countsMassbins = 0 -> results in infinity... must remove!
#***

# Syntax to do so is 

#dataset[-row_index,]
# .....
#countsMassbins[-25,]


salpeterfit=lm(countsMassbins[-25,]$logcounts ~ countsMassbins[-25,]$logbin, data=countsMassbins, 
               subset=(countsMassbins$massbin > 0.45))
summary(salpeterfit)

# Examine the results of summary and plug in for
# intercept and slope (alpha) of the Salpeter fit to overplot
abline(salpeterfit)

# What do you think of your fit?  Look at Bochanski
# Table 1 and compare. ##SAVE THIS PLOT IF YOU WISH (OPTIONAL)

# The Salpeter MF is good for stars with masses greater then 0.5 Msun. 
# Kroupa and Chabrier are good for lower masses, but we need to be careful, 
# since we separated out the lowest magnitude objects. Let's replot our 
# original histogram with the Salpeter power law on top. We're almost done.

##########################################################
# FILL IN THE XXX IN THIS EQUATION WITH 1) THE NUMBER OF STARS
# IN THE 0.5Msun BIN AND 2) THE ALPHA YOU FIT
# AND LABELS IF YOU WANT. This will plot the power law scaled 
# to total stellar number at 0.5 Msun
hist(mainseqlt$mass, breaks= nBins)

# Will take average of 0.45Msun and 0.55Msun bin
# Specify this in the paper

points(x=mainseqlt$mass, y=597*
         (mainseqlt$mass/0.55)^{-2.8})

# ~-2.8 matches the data in the kroupa and bochanski. literature

#-2.8 matches the slope from -> summary(salpeterfit)

#################################################################
#                    SAVE THIS PLOT#6                           #
#################################################################

# Note that this distribution is in COUNTS, or Nstars per
# bin, and not mass per bin. To see the mass in each bin, 
# rather than the counts of the number of stars in each bin, 
# one must multiply by the bin mass. This is useful data
# to have, so we'll do it for possible future reference.
binmass = massCounts$counts*massCounts$mids

# then make a new data frame with this to get the
# distribution of counts in bins. then assign column names
# to it, since it assigns the 2nd variable to "V2"
# So we do cbind again with the bins ("mids") and the mass total
# in the bins (binmass)
massinbins = as.data.frame(cbind(binmass,massCounts$mids))
colnames(massinbins) = c("mass", "bin")
totalmass = sum(massinbins$mass)

##########################################################
# This is the total mass in the binned histogram
totalmass

# Let's check that this compares with the actual mass,
# which is the sum of all the predicted masses in the sample.
# They may be different due to the mass binning.
# The following mass is the sum of the predicted
# masses, which would be exact if the masses were 
# measured, not predicted. The sample does not include Sirius, 
# alpha Cen, nor the WD sample, which were not in the 
# Gaia sample, so this estimate is off by at least
# 10-20 and probably more like 30 Msun. 

#################################################################
#    MAKE NOTE OF THE ACTUAL MASS OF THE SAMPLE, TOTALMASS2.    #
#################################################################
totalmass2 = sum(mainseqlt$mass)
totalmass2

# totalmass2 is the exact sample mass. we see that our binned mass
# function is close enough (<0.5%) to be a good mass function,
# i.e., a very good prediction of mass in each bin. So we could
# use the binned mass function to determine masses in different
# mass bin ranges.

#################################################################
#                            FIN!!!                             #
#################################################################

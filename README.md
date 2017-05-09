# cleansema

## Overview
The cleansema package is designed to perform the initial cleaning of data collected using the SEMA smartphone application. All you need to do is use the `cleansema` function in R by pointing it at the directory containing your raw data, and it will save clean, processed data as output. 

## Use
You'll first want to install R and Rstudio (see [here](https://www.researchgate.net/publication/316678011_A_Psychologist's_Guide_to_R]) for a quick walkthrough of that process, which is quite simple.)

One you have RStudio installed and open, using the cleansema package is simple. The first time you want to use it, you'll need to run the following code to install it. Paste this code into the command line of RStudio and hit run:

```
install.packages('devtools')
library(devtools)
install_github("seanchrismurphy/cleansema")
```

After that, you're ready to use the cleansema function on your data. This function requires two pieces of information - the input folder that contains your raw data, and the path to the .csv file you'd like to create with the processed data in it.


For example, running the code like this will take any files in the 'Raw Sema Data' folder, process them, and save them on the desktop as a csv file called 'clean_data.csv'. Adjust these two paramaters as needed (note that Windows users will need to use forward slashes, not the default back slashes that Windows uses)

```
require(cleansema)

cleansema(input = 'Users/Sean/Raw Sema Data/', output = 'Users/Sean/Desktop/clean_data.csv')
```

That's all there is to it!

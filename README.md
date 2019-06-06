## Introduction <img src="docs/reference/figures/logo.png" align="right" height="278" width="240" style="margin:30px 40px"/> 

The `ltaer` package (pronounced 'later') is an API client R package that enables users to call the various transportation-related APIs offered by the Land Transport Authority of Singapore. Currently, most of these APIs are geared towards mobile app developers who provide their end-users with real-time information about bus arrivals or traffic conditions. If used correctly, the data from these APIs can also serve as a rich dataset for those keen on analysing transportation trends in Singapore.

My main motivation behind creating this package is to make the data more accessible to a wider audience. While it is not difficult to call the API directly using the `httr` package on R, it requires a greater understanding of R and APIs that a beginner in R is unlikely to have. An important part of open data is accessibility, and I hope that this package will constitute a small step towards that goal for Singapore.

## Installing this package

Ensure that you have `devtools` downloaded, and use the `install_github` function to download the package directly from this Github repository. The code you need can be found below:


```r
install.packages('devtools')
devtools::install_github('shaunkhoo/ltaer')
```

## Documentation and Vignettes

Find all the information you need on this package's website [here](https://shaunkhoo.github.io/ltaer/index.html)

## Contact

For any bugs or issues, please raise an issue on Github.

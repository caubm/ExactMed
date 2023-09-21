
## Test environments  

* local Debian 10 (buster) install, R 4.3.1
* win-builder (oldrelease)
* win-builder (release)
* win-builder (devel)
* R-hub debian-gcc-devel (r-release)
* R-hub ubuntu-gcc-release (r-release)
* R-hub fedora-clang-devel (r-devel)

## R CMD check  results

There were no ERRORs or WARNINGs. There are 3 NOTEs:

On win-builder, R-hub ubuntu-gcc-release and 
R-hub fedora-clang-devel:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Miguel Caubet <miguelcaubet@gmail.com>'

New maintainer:
  Miguel Caubet <miguelcaubet@gmail.com>
Old maintainer(s):
  Miguel Caubet <caubet_fernandez.miguel@uqam.ca>

On R-hub ubuntu-gcc-release and R-hub fedora-clang-devel:

* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
Skipping checking math rendering: package 'V8' unavailable

On win-builder (oldrelease):

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Miguel Caubet Fernandez <caubet_fernandez.miguel@uqam.ca>’

Found the following (possibly) invalid DOIs:
  DOI: 10.1002/sim.9621
    From: DESCRIPTION
    Status: Forbidden
    Message: 403 


The maintainer's email address has been changed because he is no longer on the research staff at UQAM, and his university email address and account will be deactivated shortly.

The HTML version manual has been validated locally and on platforms 
other than R-hub ubuntu-gcc-release and R-hub fedora-clang-devel.

All DOIs were manually checked and are valid.

## Downstream dependencies

There are currently no downstream dependencies for this package.

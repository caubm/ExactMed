
## Test environments  

* local Debian 10 (buster) install, R 4.2.2
* win-builder (oldrelease)
* win-builder (release)
* win-builder (devel)
* R-hub windows-x86_64-devel (r-devel)
* R-hub debian-gcc-release (r-release)
* R-hub ubuntu-gcc-release (r-release)
* R-hub fedora-clang-devel (r-devel)


## R CMD check  results

There were no ERRORs or WARNINGs. There are 3 NOTEs:

Only on win-builder (oldrelease):

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Miguel Caubet <caubet_fernandez.miguel@uqam.ca>'

Possibly mis-spelled words in DESCRIPTION:
  Blais (10:33)
  Lefebvre (10:43, 11:17, 11:76)
  Samoilenko (10:21, 11:5, 11:64)

On win-builder (release) and ubuntu-gcc-release (r-release):

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Miguel Caubet Fernandez <caubet_fernandez.miguel@uqam.ca>’

Found the following (possibly) invalid DOIs:
  DOI: 10.1002/sim.9621
    From: DESCRIPTION
    Status: Forbidden
    Message: 403  

Found the following (possibly) invalid DOIs:
    DOI: 10.1093/aje/kwab055
      From: DESCRIPTION
      Status: Forbidden
      Message: 403  

Only on fedora-clang-devel (r-devel):

* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found


Misspelled words are all proper names.

All DOIs were manually checked and are valid.

The HTML version manual has been validated locally and on platforms other than fedora-clang-devel.


## Downstream dependencies

There are currently no downstream dependencies for this package.


# Statistical analiser in R 
Simple, universal statistical analiser
# Before using
* Make sure you're putting all your data and scripts into one folder!
* You need to have R language and Rscript installed (R version 3.6.* and up)
* If you're not sure about installed packages, it is suggested to install packages using ```analiser_package_install.R```.

Package script usage:
* Windows
```
"C:\Program Files\R\R-3.6.2\bin\Rscript.exe" analiser_package_install.R 
```
* Linux
```
Rscript analiser_package_install.R
```
# Usage
Arguments:
* file with data (csv required): -f name.csv
* file separator: -s "[sparator]"
* report file: -o output.txt

Example:
* Windows
```
"C:\Program Files\R\R-3.6.2\bin\Rscript.exe" analiser.R -f data.csv -o output.txt -s ; 
```
* Linux
```
Rscript analiser.R -f data.csv -o output.txt -s ; 
```

# Installer creation using RInno
* Follow instructions on https://github.com/ficonsulting/RInno to install the RInno package and the RInno Setup software.
    * If Windows version not supported (64bit), install with installr following instructions here https://github.com/ficonsulting/RInno/issues/118#issuecomment-460094226
* To create the installer, run [rinno_installer.R.](/src/rinno_installer.R)
* The create_config step may flag version differences between your installed R packages and the ones cached in the /bin directory. It's best to ignore this request (code 2) unless dependency issues cause problems.
* The code overwrites certain functions with some breaking bugs from the rinno packages (which is no longer maintained). It uses the infobefore.txt and infoafter.txt files for putting helper text in the installer wizard. The result will be an installer with the version number set in config.ini in the /Rinno_installer folder.

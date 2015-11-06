# The ACRU Parameter Initialization Menu Generator

This program will only automatically adjust 43 variables in the MENU Parameter File using an updated parameter text file. Once determined, these variables will only need to be adjusted once. This program will work with a distributed mode or point mode MENU Parameter File. This program also utilizes an implemented a logfile system to ensure accuracy when dealing with the automatic adjustment of values.

The variables that will be updated after each run are as follows:

 - ICELLN
 - IDSTRM
 - PRTOUT
 - IRAINF
 - FORMAT
 - PPTCOR
 - CORPPT
 - IOBSTQ
 - CLAREA
 - SAUEF
 - ELEV
 - WSSIZE
 - IYSTRT
 - IYREND
 - TELEV
 - WINCOR
 - RHUCOR
 - ALBEDO
 - SUNCOR
 - LCOVER
 - CAY
 - ELAIM
 - ROOTA
 - FOREST
 - COIAM
 - ISNOW
 - ISNOTP
 - IPSCOR
 - ISCREE
 - IFOR
 - SNCAPI
 - MCDMOD
 - TPCRIT
 - TRANGE
 - ADJ
 - TMAXSN
 - SLORAD
 - RADCOR
 - ICC
 - CORPS
 - TMCRIT
 - SNOMC
 - SNEREL

The original values from the following 16 variables will also be copied:

 - MAP
 - ARF
 - IOBSPK
 - IOBOVR
 - ALAT
 - IHEMI
 - LRREG
 - ICONS
 - ISWAVE
 - GLACIE
 - GLMULTI
 - GDEPTH
 - GAREA
 - SNORC
 - SNIRC
 - SNCC

This script will automatically adjust the parameters for all the catchments within the ACRU model. These variables belong to different information blocks in the Menu Parameter File:

 - Subcatchment Configuration : ICELLN, IDSTRM, PRTOUT
 - Rainfall File Organization : IRAINF
 - Rainfal Information : FORMAT, PPTCOR, MAP, ARF
 - Monthly Rainfall Adjustment Factor : CORPPT
 - Availability of Streamflow Data : IOBSTQ, IOBSPK, IOBSVR
 - Locational and Catchment Information : CALARE, SAUEF, ELEV, ALAT, IHEMI, WSSIZE
 - Period of Record Simulation : IYSTRT, IYREND
 - Temperature adjustment for altitude: TELEV
 - Monthly Correction Factors : WINCOR, RHUCOR, SUNCOR, SLORAD, RADCOR
 - Reference Potential Evaporation Unit Information : ALBEDO, ICONS, ISWAVE
 - Level of Land Cover : LCOVER, GLACIE, GLMULTI, GDEPTH, GAREA
 - Catchment Land Cover Information : CAY, ELAIM, ROOTA
 - Option Enhanced Wet Canopy Evaporation: FOREST
 - Streamflow Simulation Control Variables : COIAM
 - Snow Variables: ISNOW, ISNOTP, IPSCOR, ISCREE, IFOR, SNCAPI, MCDMOD
 - Snow Option Variables : TPCRIT, TRANGE, ADJ, TMAXSN, ICC, CORPS, TMCRIT, SNOMC, SNEREL

## Context

[Dr. Stefan Kienzle](http://people.uleth.ca/~stefan.kienzle/) created many fortran scripts to automate parameter calibration of the [ACRU - Agricultural Catchments Research Unit](http://unfccc.int/adaptation/nairobi_work_programme/knowledge_resources_and_publications/items/5299.php) model. He utilized the Fortran 77 language and compiled with Compaq Visual Fortran Version 6.6 compiler.

## Legacy

The purpose is to EFFICIENTLY and EFFECTIVELY initialize the Menu Parameter File with default values. This meant revising the scripts to Fortran 90 and compiled using the SilverFrost Plato Fortran 95 compiler. 

## License

The modules and scripts are licensed under a GNU General Public License v2.0

Copyright (C) 2015 Charmaine Michelle T. Bonifacio

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

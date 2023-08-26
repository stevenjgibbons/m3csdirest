Example "workflow" for m3csdirest  
=================================  

This file contains a script **onetime_oneconfig_run.sh** which takes 7 arguments:  

(1) RunID  (e.g. **KEV_NZ2010**)

(2) Reference time (e.g. **2010-10-11T22:51:21.900**)  

(3) Number of seconds before reference time to read (e.g. **60**)  

(4) Number of seconds after reference time to read (e.g. **120**)  

(5) Lower frequency for bandpass filtering in Hz (e.g. **1.0**)  

(6) Upper frequency for bandpass filtering in Hz (e.g. **5.0**)  

(7) Name of config file (e.g. **MJAO.config**)  

This config file contains one line for each station to be used:  

```
# configfile must have one line per station to be used.
#
# e.g.
#
#  station  z_chan        h1_chan         h1_azi    h2_chan      h2_azi
#
#  PSA00  AU_PSA00_00_BHZ AU_PSA00_00_BH1  289.8  AU_PSA00_00_BH2  19.8
#  PSAA1  AU_PSAA1_00_BHZ AU_PSAA1_00_BH1  350.8  AU_PSAA1_00_BH2  80.8
#  PSAA2  AU_PSAA2_00_BHZ AU_PSAA2_00_BH1  276.4  AU_PSAA2_00_BH2   6.4
#  PSAA3  AU_PSAA3_00_BHZ AU_PSAA3_00_BH1  174.5  AU_PSAA3_00_BH2 264.5
#  PSAB1  AU_PSAB1_00_BHZ AU_PSAB1_00_BH1  264.5  AU_PSAB1_00_BH2 354.5
```

so calls to the script will be as follows.

```
sh onetime_oneconfig_run.sh

USAGE: 
./onetime_oneconfig_run.sh  runID         refTime                   sBEF  sAFT   f1   f2   configfile 
./onetime_oneconfig_run.sh   PSAR_dprk6   2017-09-03T03:40:30.300   60.0  120.0  1.0  5.0   PSAR.config 
./onetime_oneconfig_run.sh   PSAR_dprk5   2016-09-09T00:40:30.300   60.0  120.0  1.0  5.0   PSAR.config 
./onetime_oneconfig_run.sh   PSAR_dprk4   2016-01-06T01:40:30.300   60.0  120.0  1.0  5.0   PSAR.config 
./onetime_oneconfig_run.sh   PSAR_dprk3   2013-02-12T03:08:20.800   60.0  120.0  1.0  5.0   PSAR.config 
./onetime_oneconfig_run.sh   MAJO_dprk6   2017-09-03T03:32:06.630   60.0  120.0  2.0  5.0   MAJO.config 
./onetime_oneconfig_run.sh   MAJO_dprk5   2016-09-09T00:32:06.630   60.0  120.0  2.0  5.0   MAJO.config 
./onetime_oneconfig_run.sh   MAJO_dprk4   2016-01-06T01:32:06.630   60.0  120.0  2.0  5.0   MAJO.config 
./onetime_oneconfig_run.sh   MAJO_dprk3   2013-02-12T02:59:56.630   60.0  120.0  2.0  5.0   MAJO.config 
./onetime_oneconfig_run.sh   KEV_NZ2010   2010-10-11T22:51:21.900   60.0  120.0  4.0  8.0   KEV.config
```

This directory contains all the config files given in this example.  



#!/bin/bash
# Steven J Gibbons, NGI 2023-04-03
#
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
#
scriptname=./onetime_oneconfig_run.sh
if [ $# != 7 ]
then
  echo
  echo "USAGE: "
  echo "$scriptname  runID         refTime                   sBEF  sAFT   f1   f2   configfile "
  echo "$scriptname   PSAR_dprk6   2017-09-03T03:40:30.300   60.0  120.0  1.0  5.0   PSAR.config "
  echo "$scriptname   PSAR_dprk5   2016-09-09T00:40:30.300   60.0  120.0  1.0  5.0   PSAR.config "
  echo "$scriptname   PSAR_dprk4   2016-01-06T01:40:30.300   60.0  120.0  1.0  5.0   PSAR.config "
  echo "$scriptname   PSAR_dprk3   2013-02-12T03:08:20.800   60.0  120.0  1.0  5.0   PSAR.config "
  echo "$scriptname   MAJO_dprk6   2017-09-03T03:32:06.630   60.0  120.0  2.0  5.0   MAJO.config "
  echo "$scriptname   MAJO_dprk5   2016-09-09T00:32:06.630   60.0  120.0  2.0  5.0   MAJO.config "
  echo "$scriptname   MAJO_dprk4   2016-01-06T01:32:06.630   60.0  120.0  2.0  5.0   MAJO.config "
  echo "$scriptname   MAJO_dprk3   2013-02-12T02:59:56.630   60.0  120.0  2.0  5.0   MAJO.config "
  echo "$scriptname   KEV_NZ2010   2010-10-11T22:51:21.900   60.0  120.0  4.0  8.0   KEV.config "
  echo
  exit 1
fi
#
runID=${1}
refTime=${2}
sBEF=${3}
sAFT=${4}
f1=${5}
f2=${6}
configfile=${7}
#
# Check that the config file exists
#
if test ! -r ${configfile}
then
  echo No file ${configfile} found ...
  exit 1
fi
#
# Check that the program m3csdirest exists
#
m3csdirest=`which m3csdirest`
if test ! -x ${m3csdirest}
then
  echo No program m3csdirest found ...
  exit 1
fi
#
if test -d ${runID}
then
  if test -r ${runID}.backup
  then
    rm -r ${runID}.backup
  fi
  mv ${runID} ${runID}.backup
fi
mkdir ${runID}
if test ! -r ${runID}
then
  echo "Failed to create directory ${runID}"
  exit 1
fi
cp ${configfile} ${runID}
cd ${runID}
#
counter=0
while read line
do
  firstchar=`echo $line | cut -c1-1`
  if [ ${firstchar} != "#" ]
  then
    echo $line
    statname=`echo $line | awk '{print $1}'`
    zzcode=`echo $line | awk '{print $2}'`
    h1code=`echo $line | awk '{print $3}'`
    h1azim=`echo $line | awk '{print $4}'`
    h2code=`echo $line | awk '{print $5}'`
    h2azim=`echo $line | awk '{print $6}'`
    zzfile=${zzcode}.sac
    h1file=${h1code}.sac
    h2file=${h2code}.sac
    netzz=`echo ${zzcode} | sed 's/_/ /g' | awk '{print $1}'`
    stazz=`echo ${zzcode} | sed 's/_/ /g' | awk '{print $2}'`
    loczz=`echo ${zzcode} | sed 's/_/ /g' | awk '{print $3}'`
    chazz=`echo ${zzcode} | sed 's/_/ /g' | awk '{print $4}'`
    neth1=`echo ${h1code} | sed 's/_/ /g' | awk '{print $1}'`
    stah1=`echo ${h1code} | sed 's/_/ /g' | awk '{print $2}'`
    loch1=`echo ${h1code} | sed 's/_/ /g' | awk '{print $3}'`
    chah1=`echo ${h1code} | sed 's/_/ /g' | awk '{print $4}'`
    neth2=`echo ${h2code} | sed 's/_/ /g' | awk '{print $1}'`
    stah2=`echo ${h2code} | sed 's/_/ /g' | awk '{print $2}'`
    loch2=`echo ${h2code} | sed 's/_/ /g' | awk '{print $3}'`
    chah2=`echo ${h2code} | sed 's/_/ /g' | awk '{print $4}'`
    cat << EOF > getwf.py
import obspy
from obspy.clients.fdsn import Client
client = Client("IRIS")
from obspy import UTCDateTime
t = UTCDateTime("$refTime")
[sbef, saft] = [ ${sBEF}, ${sAFT} ]
#
try:
    st = client.get_waveforms( "${netzz}", "${stazz}", "${loczz}", "${chazz}", t - sbef, t + saft )
except:
    print ("No waveform for ${zzcode}" )
    exit()
#
st.taper(max_percentage = 0.5, max_length = 1.0)
st.filter('bandpass', freqmin=${f1}, freqmax=${f2}, corners=2, zerophase=True)
st.write( "${zzfile}", format='sac' )
#
EOF
    python getwf.py
    cat << EOF > getwf.py
import obspy
from obspy.clients.fdsn import Client
client = Client("IRIS")
from obspy import UTCDateTime
t = UTCDateTime("$refTime")
[sbef, saft] = [ ${sBEF}, ${sAFT} ]
#
try:
    st = client.get_waveforms( "${neth1}", "${stah1}", "${loch1}", "${chah1}", t - sbef, t + saft )
except:
    print ("No waveform for ${h1code}" )
    exit()
#
st.taper(max_percentage = 0.5, max_length = 1.0)
st.filter('bandpass', freqmin=${f1}, freqmax=${f2}, corners=2, zerophase=True)
st.write( "${h1file}", format='sac' )
#
EOF
    python getwf.py
    cat << EOF > getwf.py
import obspy
from obspy.clients.fdsn import Client
client = Client("IRIS")
from obspy import UTCDateTime
t = UTCDateTime("$refTime")
[sbef, saft] = [ ${sBEF}, ${sAFT} ]
#
try:
    st = client.get_waveforms( "${neth2}", "${stah2}", "${loch2}", "${chah2}", t - sbef, t + saft )
except:
    print ("No waveform for ${h2code}" )
    exit()
#
st.taper(max_percentage = 0.5, max_length = 1.0)
st.filter('bandpass', freqmin=${f1}, freqmax=${f2}, corners=2, zerophase=True)
st.write( "${h2file}", format='sac' )
#
EOF
    python getwf.py
    if test -r ${zzfile}
    then
      if test -r ${h1file}
      then
        if test -r ${h2file}
        then
          counter=`expr $counter + 1`
          touch stations.txt
          echo ${statname} >> stations.txt
          touch m3csdirest.input
          cat << EOF >> m3csdirest.input
* Station $counter $line
${zzfile}   ${statname}
${h1file}   ${h1azim}
${h2file}   ${h2azim}
EOF
        fi
      fi
    fi
  fi
done < ${configfile}
#
numstations=`wc stations.txt | awk '{print $1}'`
if [ $numstations == 0 ]
then
  echo "Failed to collect any stations"
  exit 1
fi
NCCLEN=160
NSKIP=40
NAZIB2=36
ICFLAG=1
CHOUTR=1111
echo ${m3csdirest} ${numstations} ${NCCLEN} ${NSKIP} ${NAZIB2} ${ICFLAG} ${CHOUTR} \< m3csdirest.input
echo "${m3csdirest} ${numstations} ${NCCLEN} ${NSKIP} ${NAZIB2} ${ICFLAG} ${CHOUTR} < m3csdirest.input > m3csdirest.out" >> run_m3csdirest.sh
#
sh run_m3csdirest.sh
exit 0

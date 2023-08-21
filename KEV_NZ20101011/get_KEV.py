#
# This python script (last tested 2023/08/21) should collect
# 3 waveforms in SAC-format for the station KEV for the 
# earthquake on Novaya Zemlya on October 11, 2010, as described
# in the paper https://doi.org/10.1785/0120150302
#
import obspy
from obspy.clients.fdsn import Client
client = Client("IRIS")
from obspy import UTCDateTime
t = UTCDateTime("2010-10-11T22:50:21.90")
[sbef, saft] = [0,360]
#
st = client.get_waveforms( "IU", "KEV", "10", "BHZ", t - sbef, t + saft, attach_response = True )
# st[0].stats.latitude = 69.7565
# st[0].stats.longitude = 27.0035
st.taper(max_percentage = 0.5, max_length = 1.0)
st.filter('bandpass', freqmin=2.0, freqmax=8.0, corners=2, zerophase=True)
st.write( "IU_KEV_BHZ_10.sac", format='SAC' )
st.plot()
#
st = client.get_waveforms( "IU", "KEV", "10", "BH1", t - sbef, t + saft )
st.taper(max_percentage = 0.5, max_length = 1.0)
st.filter('bandpass', freqmin=2.0, freqmax=8.0, corners=2, zerophase=True)
st.write( "IU_KEV_BHN_10.sac", format='SAC' )
st.plot()
#
st = client.get_waveforms( "IU", "KEV", "10", "BH2", t - sbef, t + saft )
st.taper(max_percentage = 0.5, max_length = 1.0)
st.filter('bandpass', freqmin=2.0, freqmax=8.0, corners=2, zerophase=True)
st.write( "IU_KEV_BHE_10.sac", format='SAC' )
st.plot()
#

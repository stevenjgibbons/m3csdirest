#!/bin/sh
awk '{print $2, $3}' AK.BRLK_azi.txt | aziwsa > AK.BRLK_func.txt

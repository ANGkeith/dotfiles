#!/usr/bin/env bash

# prints all temperature
# paste <(cat /sys/class/thermal/thermal_zone*/type) <(cat /sys/class/thermal/thermal_zone*/temp) | column -s $'\t' -t | sed 's/\(.\)..$/.\1°C/'
 
# acpitz        41.0°C # motherboard
# pch_skylake   49.0°C # platform controller hub
# iwlwifi       38.0°C # 
# x86_pkg_temp  39.0°C # cpu package temperatuer

paste <(cat /sys/class/thermal/thermal_zone*/type) <(cat /sys/class/thermal/thermal_zone*/temp) | column -s $'\t' -t | sed 's/\(.\)..$/.\1°C/' | awk 'NR==2 {print $2}'



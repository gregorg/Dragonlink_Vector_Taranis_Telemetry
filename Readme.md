# Readme

This Script DLVT is a nice Telemetry screen for Taranis with OpenTX >2.17 and DragonLink V3 and Vector >1.58

Thanks to ilihack, SockEye, Richardoe, Schicksie, lichtl, ben_&Jace25,Clooney82&fnoopdogg for they Previous Work.


Changelog:

V1:
First version


## Screenshots

![Screenshot](https://raw.githubusercontent.com/gregorg/Dragonlink_Vector_Taranis_Telemetry/master/screenshot.png)


## Installing

1. Enable telemetry from Dragonlink to your Taranis
2. Enable telemetry from Vector to Dragonlink

Follow those youtube tutorials from [jmxp69 Youtube channel](https://www.youtube.com/channel/UCcJwn8V3MTsib2LjAcTnarg): 

1. [Part 1: wiring](https://www.youtube.com/watch?v=lkADQvqdozI)
2. [Part 2: software](https://www.youtube.com/watch?v=5HS3AZ5jNJ8)
3. [Part 3: radio](https://www.youtube.com/watch?v=IBhJB5RaJ74) - on this part, replace dlvector.lua by DLVT.lua



## Taranis Setup OpenTX 2.1.6 or newer

1. Make sure you have LUA-Scripting enabled in companion
2. Download the scripts folder from here and copy to the SD card root
3. Optional: Edit with a txt Editor the Downloaded Script to Change the Setup to you own Wishes
3. Start your Taranis, go into your desired Model Settings by short pressing the Menu button
4. Navigate to the last Page by long pressing the page button
5. Delete all Sensors
6. Discovery new Sensors
7. There will be a lot of sensors listed depending on your receiver (d8r, d4r, x8r etc.)
8. Recommend is to check if the sensors Name correct. 
9. Set this lua script as Telemetry screen.

### Sensor Name (case sensitive!)

* A1 -> is the Lipo Voltage
* GAlt -> is the GPS Altitude
* Curr -> Current drain
* GSpd -> GPS Speed
* Hdg -> Compass Direction
* RSSI -> Rssi Value
* Flightmodes -> Vector flight modes (needs manual configuration into your Taranis)


### Optional Setup DLVT:

open the script with an txt editor and you can modify at the beginn of the script allot of Parameters.

### Using:

Push in the Normal Taranis Screen Long the Page Button to see the DLVT Telemetry screens.
If you want to Reset DLVT because you have a new Home Position or reset you Battery Consume or what else Push long (Menu) in the DLVT Screen.

## useful links

1. http://copter.ardupilot.com/wiki/common-optional-hardware/common-telemetry-landingpage/common-frsky-telemetry/ (How to connect your Converter)

## DLVT Script Download

https://github.com/gregorg/Dragonlink_Vector_Taranis_Telemetry/archive/master.zip


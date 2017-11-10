--#########################################################################################
--#   DragonLink / Vector Lua Telemetry Script for Taranis                                #
--#                                                                                       #
--#  + with opentx 2.16 and above, tested with DragonLink V3 Slim and Vector 1.60         #
--#                                                                                       #
--#  Thanks to ilihack, Lupinixx, athertop, gulp79, SockEye, Richardoe, Schicksie,        #
--#  lichtl, _ben&Jace25(FPV-Community) and Clooney82&fnoopdogg                           #
--#                                                                                       #
--#  DLVector © 2017 skygreg                                                              #
--#########################################################################################
--Setup: (not used, work in progress)                                                     #
--                                                                                        #
local HeadingOrDist = 2       --draw Hdg=0 / Draw Distance=1 / Draw Both alternatel=2     #
local BatterymahAlarm = 0 --0=off or like 2200 for Alarming if you used more 2200mAh      #
local SaybatteryPercent=1 ---0=off or 1 if you will hear you Batterypercent in 10% Steps  #
local CellVoltAlarm=3.3 --0=off or like 3.3 to get an Alarm if you have less than 3.3V    #
--                                                                                        #
--#########################################################################################                                                  
-- Advance Configs:                                                                       #
--                                                                                        #
local MaxAvarageAmpere=0 -- 0=Off, Alarm if the avarage 5s current is over this Value     #
local battype=0   -- 0=Autodetection (1s,2s,3s,4s,6s,8s) or 7 for an 7s Battery Conf      #
                       --from this mAh Value                                             #
local GPSOKAY=1 --1=play Wav files for Gps Stat , 0= Disable wav Playing for Gps Status   # 
local SayFlightMode = 1 --0=off 1=on then play wav for Flightmodes changs                 #
--                                                                                        #
--######################################################################################### 


-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY, without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, see <http://www.gnu.org/licenses>.


local function getTelemetryId(name)
  field = getFieldInfo(name)
  if getFieldInfo(name) then return field.id end
  return -1
end


local data = {}
  --data.battsumid =    getTelemetryId("VFAS")
  data.vfasid =       getTelemetryId("A1")
  data.celsid =       getTelemetryId("Cels")
  data.gpsaltid =     getTelemetryId("GAlt") 
  data.spdid =        getTelemetryId("GSpd")
  data.gpsid =        getTelemetryId("GPS")
  data.currentid =    getTelemetryId("Curr")
  data.rssiId =       getTelemetryId("RSSI")
  data.headingid =    getTelemetryId("Hdg")
  data.fuelid =       getTelemetryId("Fuel")
  fmode,fmodeName =   getFlightMode()
  fmodeName = "Fake"
  modelname =         model.getInfo()


  --init Telemetry Variables 
  data.battsum =    0
  data.cellnum =    0
  data.alt =        0
  data.spd =        0
  data.current =    0
  data.rssi =       0
  data.heading =    0
  data.throttle =   0

--init Timer
local oldTime={0,0,0,0,0,0}
local Time={0,0,0,0,0,0}

--init var for v.speed Calc
local Vspeed = 0.0
local prevAlt = 0.0

--intit Battery and consume
local totalbatteryComsum = 0.0
local HVlipoDetected = 0 
local battpercent = 0
local CellVolt=0.0
local batttype = 0

local CurrA={}
local CellVoltA={}

local CellResistance=0.0
local ResCalcError=0
local ArrayIteam=0
local goodIteams=0
local AraySize= 200--set the Size of the Ring resistance Array 

--init other
local effizient=0.0
local currAvarg=0.0
local gps_hori_Distance=0.0
local lastsaynbattpercent=100
local rxpercent = 0
local firsttime=0
local settings = getGeneralSettings()
local DisplayTimer=0

--init compass arrow
local arrowLine = {
  {-4, 5, 0, -4},
  {-3, 5, 0, -3},
  {-2, 5, 0, -2},
  {-1, 5, 0, -1},
  {1, 5, 0, -1},
  {2, 5, 0, -2},
  {3, 5, 0, -3},
  {4, 5, 0, -4}
}
--Script Initiation end  


--------------------------------------------------------------------------------  
--------------------------------------------------------------------------------   
--------------------------------------------------------------------------------
-- functions 
-------------------------------------------------------------------------------- 
--------------------------------------------------------------------------------  
--------------------------------------------------------------------------------  


--------------------------------------------------------------------------------
-- function Reset Variables
-------------------------------------------------------------------------------
local function ResetVar() 
  --data.battsumid =    getTelemetryId("VFAS")
  data.vfasid =       getTelemetryId("A1")
  data.celsid =       getTelemetryId("Cels")
  data.gpsaltid =     getTelemetryId("GAlt") 
  data.spdid =        getTelemetryId("GSpd")
  data.gpsid =        getTelemetryId("GPS")
  data.currentid =    getTelemetryId("Curr")
  data.rssiId =       getTelemetryId("RSSI")
  data.headingid =    getTelemetryId("Hdg")
  data.fuelid =       getTelemetryId("Fuel")
  fmode,fmodeName =   getFlightMode()

  Time={0,0,0,0,0,0}
  Vspeed = 0.0
  prevAlt = 0.0
  totalbatteryComsum = 0.0
  battpercent = 0
  CellVolt=0.0
  CurrA={}
  CellVoltA={}
  CellResistance=0.0
  ArrayIteam=0 
  effizient=0.0
  currAvarg=0.0
  gps_hori_Distance=0.0
  lastsaynbattpercent=100
  battype=0
  firsttime=1
  settings = getGeneralSettings()
  data.lon=nil
  data.lat=nil
end
  
  
--------------------------------------------------------------------------------
-- function Round
-------------------------------------------------------------------------------- 
local function round(num, idp)
  local temp = 10^(idp or 0)
  if num >= 0 then return math.floor(num * temp + 0.5) / temp
  else return math.ceil(num * temp - 0.5) / temp end
end


--------------------------------------------------------------------------------
-- function Say battery percent
--------------------------------------------------------------------------------
local function SayBattPercent()  

  if (battpercent < (lastsaynbattpercent-10)) then --only say in 10 % steps

    Time[6] = Time[6] + (getTime() - oldTime[6]) 
        
    if Time[6]> 700 then --and only say if battpercent 10 % below for more than 10sec
      lastsaynbattpercent=(round(battpercent*0.1)*10)
      Time[6] = 0
      playNumber(round(lastsaynbattpercent), 13, 0)
      if lastsaynbattpercent <= 10 then 
        playFile("batcrit.wav") 
      end
    end

    oldTime[6] = getTime() 

  else    
    Time[6] = 0
    oldTime[6] = getTime() 
  end

end
 
 
--------------------------------------------------------------------------------
-- function Vertical Speed Calc
--------------------------------------------------------------------------------
local function VSpeedCalc()  
  local temp = 0.0 --Valueholder

  Time[2] = Time[2] + (getTime() - oldTime[2])
  if  data.alt~=prevAlt or Time[2]>130 then --1300 ms
    temp  = ( (data.alt-prevAlt) / (Time[2]/100) )
    Time[2] = 0 
    prevAlt=data.alt
  end
  oldTime[2] = getTime() 
  
  if Vspeed <10 then
    Vspeed=temp*0.3 + Vspeed*0.7
  else 
    Vspeed=temp*0.1 + Vspeed*0.90
  end
end
   
   
--------------------------------------------------------------------------------
-- funnction Lipo Cell Dection 
--------------------------------------------------------------------------------
local function BatteryCalcCellVoltageAndTyp()
  if (battype==0 or math.ceil(data.battsum/4.37) > batttype) then
    if math.ceil(data.battsum/4.37) > battype and data.battsum<4.37*8 then 
      battype=math.ceil(data.battsum/4.37)
      if battype==7 then battype=8 end --dont Support 5s&7s Battery, its Danger to Detect: if you have an Empty 8s its lock like an 7s...
      -- if battype==5 then battype=6 end 
    end
  end
  if data.battsum > 4.22*battype then --HVLI is detected
    HVlipoDetected=1
  else
    HVlipoDetected=0
  end
  if battype > 0 then 
    CellVolt = round(data.battsum/battype, 2)
  end
end
  
--------------------------------------------------------------------------------
-- funnction conti Lipo Resistance Calculate V0.73 ALPHA ilihack
--------------------------------------------------------------------------------
local function BatteryResistanceCalc() --Need CellVolt and current from Telemetry Sampels and Calc the Resistence with it

  local temp=0 --only an Valueholder in calcs
  local sum_x=0
  local sum_y=0
  local sum_xx=0
  local sum_xy=0

  
  if ArrayIteam==0 then --init Aray wenn its first Time
    goodIteams=0
    ResCalcError=0
    for i=1,AraySize do
      CurrA[i]=0
      CellVoltA[i]=0
    end
  end
  

  if ArrayIteam < AraySize  then ArrayIteam=ArrayIteam+1 else ArrayIteam=1  --if on the end Return to the beginn and overwrite old Values
  end 
  
  if ( CellVolt>2.5 and CellVolt<4.5 and data.current>0.1 and data.current<180 ) then --check if values are in range and Safe New Samples in Array) 
      if CellVoltA[ArrayIteam]==0 then goodIteams=goodIteams+1 end
      CellVoltA[ArrayIteam]=CellVolt
      CurrA[ArrayIteam]=data.current
  else
      if CellVoltA[ArrayIteam]~=0 then goodIteams=goodIteams-1 end
      CellVoltA[ArrayIteam]=0
      CurrA[ArrayIteam]=0
  end
  
  if goodIteams>(AraySize*0.7) then --if cache 80 % filled begin to calc
  ---Start Liniar Regression over the Volt & Current Arrays    
    for i=1,AraySize do
        local curr=CurrA[i]
        local volt=CellVoltA[i]
        sum_x=sum_x+curr
        sum_y=sum_y+volt
        sum_xx=sum_xx+curr*curr
        sum_xy=sum_xy+(curr*volt)
    end
    
    temp=(sum_x*sum_y-goodIteams*sum_xy)/(goodIteams*sum_xx-sum_x*sum_x) --calc the coeffiz m of an Liniar func and symbolise the Battery Resistance

    if (temp > 0.001 and temp < 0.20 ) then --check if in Range 1- 200mohm 
      if CellResistance==0 then --init for faster filtering
        CellResistance=temp
      else
        CellResistance=CellResistance*0.99 +temp*0.01 --Update Cellresistance             
      end
    end
     
      
  ---if Resistance okay correctet Voltage else counterror
    temp=(data.current*CellResistance) --Calc temp calc cellvolt Drift
    if ((HVlipoDetected==1 and CellVolt+temp>4.45) or (CellVolt+temp>4.3 and HVlipoDetected==0)) then --not in Range
      ResCalcError=ResCalcError+1
      if ResCalcError ==5 then 
        playFile("/SCRIPTS/WAV/errorres.wav")
        ArrayIteam=0
      end
    elseif (ResCalcError < 10 and CellVolt~=0 ) then --not much errors happend
      CellVolt=CellVolt+temp --correct Cell Voltage with calculated Cell Resistance  
    end
  end
end 


--------------------------------------------------------------------------------
-- funnction Lipo Range Calculate with Voltage
--------------------------------------------------------------------------------
local function BatteryLevelCalcVoltage()  
  local temp=0 --for cellvolt and unfildred battpercent placeholder
  
  if HVlipoDetected==1 then --for HVlipo better estimation
    temp = CellVolt-0.15 
  else
    temp = CellVolt
  end --Correction for HVlipo Batterpercent Estimation

  if temp > 4.2                      then temp = 100
  elseif temp < 3.2                      then temp = 0
  elseif temp >= 4                       then temp = 80*temp - 236
  elseif temp <= 3.67                    then temp = 29.787234 * temp - 95.319149 
  elseif temp > 3.67 and temp < 4        then temp = 212.53*temp-765.29
  end

  if battpercent==0 then 
    battpercent=round(temp) --init battpercent
  --else 
  --  battpercent=round(battpercent*0.98 + 0.02*temp)
  end
end

 
--------------------------------------------------------------------------------
-- funnction CurrentTotal Calc Consum function
-------------------------------------------------------------------------------
local function totalConsume()
  Time[1] = Time[1] + (getTime() - oldTime[1])
  if Time[1] >=20 then --200 ms
    totalbatteryComsum  = totalbatteryComsum + ( data.current * (Time[1]/360))
    Time[1] = 0
  end
  oldTime[1] = getTime() 
end

--------------------------------------------------------------------------------
-- function check if BatterymahAlarm  max reached totalbatteryComsum 
--------------------------------------------------------------------------------
local function AlarmifmaxMah()
  if BatterymahAlarm  > 0 and BatterymahAlarm < totalbatteryComsum then 
    playFile("battcns.wav")
    BatterymahAlarm=0
  end
end
 
--------------------------------------------------------------------------------
-- function check if Cell Volt min
--------------------------------------------------------------------------------
local function AlarmifVoltLow()
  if CellVolt  < CellVoltAlarm and data.battsum >0.5  then 
    Time[3] = Time[3] + (getTime() - oldTime[3])
    if Time[3] >=800 then --8s
      playFile("battcns.wav")
      Time[3] = 0
    end
    oldTime[3] = getTime()
  end
end

--------------------------------------------------------------------------------
-- function check if avarage Amp over max
-------------------------------------------------------------------------------
local function AlarmifOverAmp() 
  currAvarg=data.current*0.01+currAvarg*0.99
  if currAvarg  > MaxAvarageAmpere  then 
    Time[4] = Time[4] + (getTime() - oldTime[4])
    if Time[4] >=250 then --2,5s
      playFile("currdrw.wav")
      Time[4] = 0
    end
    oldTime[4] = getTime()
  end
end

--------------------------------------------------------------------------------
-- function DisplayTimer to draw alternately
-------------------------------------------------------------------------------
local function CalcDisplayTimer()
 Time[5] = Time[5] + (getTime() - oldTime[5])
 if Time[5] >=200 then --2s
   if DisplayTimer==1 then 
     DisplayTimer=0 
   else 
     DisplayTimer=1
   end
   Time[5] = 0
 end
 oldTime[5] = getTime()
end



--------------------------------------------------------------------------------
-- functions calc GPS Distance
-------------------------------------------------------------------------------

local function loadGpsData()
  if GPSOKAY==3 and (type(data.gps) == "table") then
    if data.gps["lat"] ~= nil and data.lat==nil then
        data.lat = data.gps["lat"]
    elseif data.gps["lon"] ~= nil and data.lon==nil then
        data.lon = data.gps["lon"]
    else
    local sin=math.sin--locale are faster
    local cos=math.cos
    local z1 = (sin(data.lon - data.gps["lon"]) * cos(data.lat) )*6358364.9098634
    local z2 = (cos(data.gps["lat"]) * sin(data.lat) - sin(data.gps["lat"]) * cos(data.lat) * cos(data.lon - data.gps["lon"]) )*6358364.9098634 
    gps_hori_Distance =  (math.sqrt( z1*z1 + z2*z2))/100

    end      
  end
end


--------------------------------------------------------------------------------
-- function Get new Telemetry Value
--------------------------------------------------------------------------------
local function GetnewTelemetryValue()
  local getValue = getValue --faster
  local cellResult = getValue(data.celsid)
  data.battsum =    getValue(data.vfasid)
  data.alt =        getValue(data.gpsaltid)
  data.spd =        getValue(data.spdid)
  data.current =    getValue(data.currentid)
  data.rssi =       getValue(data.rssiId)
  data.gps =        getValue(data.gpsid)
  data.heading =    getValue(data.headingid)
  data.fuel =       getValue(data.fuelid)
  data.throttle =   getValue('thr')
  fmode,fmodeName = getFlightMode()
end
    
    
    
-- ###############################################################
---###############################################################
---###############################################################
-- ###############################################################
-- ##                -- Main draw Loop --                       ##
-- ###############################################################  
---###############################################################
---###############################################################
---###############################################################

local function draw()   
  --lokalize optimaztion
  local drawText=lcd.drawText 
  local getLastPos=lcd.getLastPos
  local MIDSIZE=MIDSIZE
  local SMLSIZE=SMLSIZE 
    
    

  -- ###############################################################
  -- Battery level Drawing
  -- ###############################################################
    
  local myPxHeight = math.floor(battpercent * 0.37) --draw level
  local myPxY = 50 - myPxHeight

  lcd.drawPixmap(1, 3, "/SCRIPTS/BMP/battery.bmp")

  lcd.drawFilledRectangle(6, myPxY, 21, myPxHeight, FILL_WHITE )
   
  local i = 38
  while (i > 0) do 
    lcd.drawLine(6, 12 + i, 26, 12 +i, SOLID, GREY_DEFAULT)
    i= i-2
  end


  if battpercent < 10 or battpercent >=100 then
     drawText(12,0, round(battpercent).."%",INVERS + SMLSIZE + BLINK)
  else
     drawText(11,0, round(battpercent).."%" ,SMLSIZE)
  end
  
  lcd.drawNumber(0,57, data.battsum*10,PREC1+ LEFT )
  --if (HVlipoDetected == 1 and data.battsum >=10) then
  --  lcd.drawNumber(0,57, data.battsum,PREC1+ LEFT )
  --else
  --  lcd.drawNumber(0,57, data.battsum*100,PREC2 + LEFT )
  --end


  if HVlipoDetected == 1 then
    drawText(getLastPos(), 57,"H", BLINK, 0) 
  end
  drawText(getLastPos(), 57, "V ", 0)
  drawText(getLastPos(), 58, battype.."S" , SMLSIZE)
   

-- ###############################################################
-- Display RSSI data
-- ###############################################################
    
  if data.rssi > 38 then
    rxpercent = data.rssi
    if rxpercent > 100 then
        rxpercent = 100
    end
  else
    rxpercent=0
  end

  lcd.drawPixmap(164, 6, "/SCRIPTS/BMP/RSSI"..math.ceil(rxpercent/10)..".bmp") --Round rxpercent to the next higer 10 Percent number and search&draw pixmap
   
  drawText(184, 57, rxpercent, 0) 
  drawText(getLastPos(), 58, "% RX", SMLSIZE)
   
   
-- ###############################################################
-- Timer Drawing 
---- ###############################################################
  --local timer = model.getTimer(0)
  --drawText(36, 44, " Timer : ",SMLSIZE)
  --lcd.drawTimer(getLastPos(), 40, timer.value, MIDSIZE)
    
    
-- ###############################################################
-- Vertical Speed Drawing
-- ###############################################################
  drawText(36,44, "Vspeed: ",SMLSIZE)
  lcd.drawChannel(getLastPos(),44,'VSpd',MIDSIZE)  
 
 
-- ###############################################################
-- Speed Drawing
-- ###############################################################
  drawText(38,29, "Speed : ",SMLSIZE,0)
  if settings['imperial'] ~=0 then
    drawText(getLastPos(), 25, round(data.spd), MIDSIZE)
    drawText(getLastPos(), 29, "mph", SMLSIZE)
  else
    drawText(getLastPos(), 25, round(data.spd), MIDSIZE)
    drawText(getLastPos(), 29, "kmh", SMLSIZE)	
  end    
      
        
-- ###############################################################
-- Distance above rssi  Drawing
-- ###############################################################
  if HeadingOrDist == 1 or (DisplayTimer==1 and HeadingOrDist == 2)  then
  
    if settings['imperial'] ~=0 then
      drawText(163,0, "Dist:"..(round(gps_hori_Distance*3.28)).."f",SMLSIZE)
    else
     drawText(163,0, "Dist:"..(round(gps_hori_Distance)).."m",SMLSIZE)
   end
  
  
---- ###############################################################
---- Heading  above rssi Drawing
---- ###############################################################
  
  elseif HeadingOrDist==0 or (DisplayTimer==0 and HeadingOrDist == 2) then
    
    local HdgOrt=""
    
    if data.heading <0 or data.heading >360 then HdgOrt="Error"  
      elseif data.heading <  22.5  then HdgOrt="N"     
      elseif data.heading <  67.5  then HdgOrt="NE" 
      elseif data.heading <  112.5 then HdgOrt="E"  
      elseif data.heading <  157.5 then HdgOrt="SE" 
      elseif data.heading <  202.5 then HdgOrt="S"  
      elseif data.heading <  247.5 then HdgOrt="SW"    
      elseif data.heading <  292.5 then HdgOrt="W"     
      elseif data.heading <  337.5 then HdgOrt="NW"    
      elseif data.heading <= 360.0 then HdgOrt="N"    
    end
    
    drawText(175,0, HdgOrt.." "..data.heading,SMLSIZE)
    drawText(getLastPos(), -2, 'o', SMLSIZE)  
  end


-- ###############################################################
-- Display Compass arrow data
-- ###############################################################

  sinCorr = math.sin(math.rad(data.heading))
  cosCorr = math.cos(math.rad(data.heading))
  for index, point in pairs(arrowLine) do
      X1 = 150 + math.floor(point[1] * cosCorr - point[2] * sinCorr + 0.5)
      Y1 = 5 + math.floor(point[1] * sinCorr + point[2] * cosCorr + 0.5)
      X2 = 150 + math.floor(point[3] * cosCorr - point[4] * sinCorr + 0.5)
      Y2 = 5 + math.floor(point[3] * sinCorr + point[4] * cosCorr + 0.5)
      if X1 == X2 and Y1 == Y2 then
          lcd.drawPoint(X1, Y1, SOLID, FORCE)
      else
          lcd.drawLine (X1, Y1, X2, Y2, SOLID, FORCE)
      end
  end
   
   
-- ###############################################################
-- Altitude Drawing
-- ###############################################################
  drawText(114,44, "Alt: ",SMLSIZE,0)
  local temp=data.alt
  
  if temp >=10 or temp<-0.1 then
    drawText(getLastPos(), 40, round(temp), MIDSIZE)
  elseif temp<=0.0 and temp>=-0.1 then
    drawText(getLastPos(), 40, 0, MIDSIZE)
  else 
    drawText(getLastPos(), 40, round(temp,1), MIDSIZE)
  end
  
  if settings['imperial']~=0 then
    drawText(getLastPos(), 44, 'f', 0) 
  else
    drawText(getLastPos(), 44, 'm', 0)
  end
   
   
-- ###############################################################
-- CurrentTotal Draw Consum Drawing AND single cell voltage
-- ###############################################################
 
  --drawText(46, 58, "Cell: "..(CellVolt)..'V',SMLSIZE)
   
   
-- ###############################################################
-- Throttle
-- ###############################################################
 
  drawText(60, 57, ""..round((data.throttle + 1024)/20.48)..'%',SMLSIZE)
  lcd.drawGauge(46, 56, 70, 8, round((data.throttle + 1024)/20.48), 100)
   
  
-- ###############################################################
-- efficient  Calc and Drawing
-- ############################################################### 
  
  if data.spd > 10 then --draw wh per km
     
    if settings['imperial']==0 then
      effizient = effizient*0.8+(0.2*(data.current*data.battsum/data.spd))--spdint can not be 0 because the previus if
      drawText(130, 58,""..round(effizient,1)..'Wh/km', SMLSIZE)
    else
      effizient = effizient*0.8+(0.2*(data.current*data.battsum/(data.spd*0.621371)))
      drawText(98, 58,""..round(effizient,1)..'Wh/mi', SMLSIZE) 
    end
  
  else --draw wh per h
     effizient = effizient*0.8+0.2*(data.current*data.battsum)
     drawText(104, 58, " draw: "..(round(effizient,1))..'W', SMLSIZE)
  end
   

-- ###############################################################
-- Current Drawing
-- ###############################################################

  drawText(113, 29, "Cur: ",SMLSIZE)
  
  if data.current >=100 then  
    drawText(getLastPos(), 25, round(data.current),MIDSIZE)
  else 
    drawText(getLastPos(), 25, round(data.current,1),MIDSIZE)
  end
  
  drawText(getLastPos(), 29, 'A', 0)
  
  
-- ###############################################################
-- Flightmodes Drawing for copter todo for plane,Folow
-- ###############################################################

  drawText(68, 1, fmodeName, MIDSIZE)
    
  

-- ###############################################################
-- Flightmode Image
-- ###############################################################

 --     lcd.drawPixmap(50, 2, "/SCRIPTS/BMP/H.bmp")  
 --     lcd.drawPixmap(50, 2, "/SCRIPTS/BMP/stab.bmp")
      --lcd.drawPixmap(50, 2, "/SCRIPTS/BMP/gps.bmp")
  

-- ###############################################################
-- GPS Fix
-- ###############################################################
  if (type(data.gps) == "table") then
     drawText(68, 15, round(data.gps["lat"],4) .. "/" .. round(data.gps["lon"],4), SMLSIZE)
  else
     drawText(68, 15, "No GPS Data", SMLSIZE)
  end
end


--------------------------------------------------------------------------------
-- BACKGROUND loop FUNCTION
--------------------------------------------------------------------------------
local function backgroundwork()
  GetnewTelemetryValue()
  
--  data.current=getValue(MIXSRC_Rud)/7 
--  data.battsum=getValue(MIXSRC_Thr)/60
  
  BatteryCalcCellVoltageAndTyp() 
  totalConsume()
  
  if MaxAvarageAmpere > 0     then AlarmifOverAmp()       end
  if BatterymahAlarm > 0      then AlarmifmaxMah()        end--check if alarm funktion enabled and calc it.
  if CellVoltAlarm>0          then AlarmifVoltLow()       end
  
end

local function background()
  ArrayIteam=0--Delete Resistance Aray because it can be have old Values in the Background
  ResCalcError=0
  backgroundwork()
end


--------------------------------------------------------------------------------
-- RUN loop FUNCTION
--------------------------------------------------------------------------------
local function run(event)
    
  --if firsttime==0 then
  --  playFile("/SCRIPTS/WAV/welcome.wav") 
  --  firsttime=firsttime+1
  --end
  --if firsttime<20 then
  --  lcd.drawPixmap(0, 0, "/SCRIPTS/BMP/LuaPiloL.bmp")
  --  lcd.drawPixmap(106, 0, "/SCRIPTS/BMP/LuaPiloR.bmp")
  --  firsttime=firsttime+1
  --  return 0
  --end
  
  if event ==  64  then --if menu key pressed that Reset All Variables.  
    playFile("/SCRIPTS/WAV/reset.wav")
    killEvents(64) 
    ResetVar() 
  end
  
  backgroundwork()
  
  if HeadingOrDist ==1 or HeadingOrDist ==2       then loadGpsData() end
  battpercent = data.fuel
  BatteryLevelCalcVoltage()
  if SaybatteryPercent==1                         then SayBattPercent()     end
  
  CalcDisplayTimer()
  -- VSpeedCalc() 
  lcd.clear()
  draw()
end

--------------------------------------------------------------------------------
-- SCRIPT END
--------------------------------------------------------------------------------
return {run=run,  background=background}

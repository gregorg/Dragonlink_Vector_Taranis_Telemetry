--#########################################################################################
--#   LuaPilot v2.008  Lua Telemetry Script for Taranis                                   #
--#                                                                                       #
--#  + with opentx 2.16 and above, tested with D4r-II & D8R & X4R                         #
--#  + works with Arducopter Flight Controller like Pixhawk, APM, PX4 and maybe others    #
--#                                                                                       #
--#  Thanks to Lupinixx, athertop, gulp79, SockEye, Richardoe, Schicksie,lichtl                                       #    
--#  _ben&Jace25(FPV-Community) and Clooney82&fnoopdogg                                   #
--#                                                                                       #
--#  LuaPilot © 2017 ilihack                                                              #
--#########################################################################################
--Setup:                                                                                  #
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
local CalcBattResistance=0 --0=off 1=AutoCalc Lipo Resistance an correct Lipo.Level ALPHA #   
local battype=0   -- 0=Autodetection (1s,2s,3s,4s,6s,8s) or 7 for an 7s Battery Conf      #
local BattLevelmAh = 0 --if 0 BatteryLevel calc from Volt, if -1 Fuel is used else        #
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
  data.vfasid =       getTelemetryId("VFAS")
  data.celsid =       getTelemetryId("Cels")
  data.altid =        getTelemetryId("Alt")
  --data.gpsaltid =     getTelemetryId("GAlt") 
  data.spdid =        getTelemetryId("GSpd")
  data.gpsid =        getTelemetryId("GPS")
  data.currentid =    getTelemetryId("Curr")
  data.flightmodeId = getTelemetryId("Tmp1")
  data.rssiId =       getTelemetryId("RSSI")
  data.gpssatsid =    getTelemetryId("Tmp2")
  data.headingid =    getTelemetryId("Hdg")
  data.fuelid =       getTelemetryId("Fuel")


  --init Telemetry Variables 
  data.battsum =    0
  data.cellnum =    0
  data.alt =        0
  data.spd =        0
  data.current =    0
  data.flightmodeNr=0
  data.rssi =       0
  data.gpssatcount =0
  data.heading =    0

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

  local CurrA={}
  local CellVoltA={}

  local CellResistance=0.0
  local ResCalcError=0
  local ArrayIteam=0
  local goodIteams=0
  local AraySize= 200--set the Size of the Ring resistance Array 
  
  --init other
  local effizient=0.0
  local lastflightModeNumber = 0
  local currAvarg=0.0
  local gps_hori_Distance=0.0
  local lastsaynbattpercent=100
  local rxpercent = 0
  local firsttime=0
  local DisplayTimer=0
  local settings = getGeneralSettings()

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
    data.vfasid =       getTelemetryId("VFAS")
    data.celsid =       getTelemetryId("Cels")
    data.altid =        getTelemetryId("Alt")
    --data.gpsaltid =     getTelemetryId("GAlt") 
    data.spdid =        getTelemetryId("GSpd")
    data.gpsid =        getTelemetryId("GPS")
    data.currentid =    getTelemetryId("Curr")
    data.flightmodeId = getTelemetryId("Tmp1")
    data.rssiId =       getTelemetryId("RSSI")
    data.gpssatsid =    getTelemetryId("Tmp2")
    data.headingid =    getTelemetryId("Hdg")
    data.fuelid =       getTelemetryId("Fuel")
  
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
    lastflightModeNumber = 0
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
      if (battype==0) then
          if math.ceil(data.battsum/4.37) > battype and data.battsum<4.37*8 then 
             battype=math.ceil(data.battsum/4.37)
             if battype==7 then battype=8 end --dont Support 5s&7s Battery, its Danger to Detect: if you have an Empty 8s its lock like an 7s...
             if battype==5 then battype=6 end 
           end
      end
      if data.battsum > 4.22*battype then --HVLI is detected
         HVlipoDetected=1
      else
         HVlipoDetected=0
      end
      if battype > 0 then 
         CellVolt = data.battsum/battype 
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
  -- funnction Lipo Range Calculate with COMSUME
--------------------------------------------------------------------------------
  local function BatteryLevelCalcmah()  --calc Battery Percent with mah comsume
    if BattLevelmAh>0 then
      battpercent= round((100/BattLevelmAh)*(BattLevelmAh-totalbatteryComsum))  
    end
    if battpercent<0 then battpercent=0 end
    if battpercent>100 then battpercent=100 end
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
    else 
      battpercent=round(battpercent*0.98 + 0.02*temp)
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

--if Cels is available from FrSky FLVSS then use it, else use VFAS
local cellResult = getValue(data.celsid)
   if (type(cellResult) == "table") then
      data.battsum = 0
      if (battype == 0) then
         for i, v in ipairs(cellResult) do
            data.battsum = data.battsum + v
            battype = battype +1
         end
      else
         for i, v in ipairs(cellResult) do
            data.battsum = data.battsum + v
         end
      end
   else
      data.battsum =    getValue(data.vfasid)
   end

    --data.battsum =    getValue(data.battsumid)
    data.alt =        getValue(data.altid)
    data.spd =        getValue(data.spdid) --knotes per h 
    data.current =    getValue(data.currentid)
    data.flightmodeNr=getValue(data.flightmodeId)
    data.rssi =       getValue(data.rssiId)
    data.gpssatcount =getValue(data.gpssatsid)
    data.gps =        getValue(data.gpsid)
    data.heading =    getValue(data.headingid)
    data.fuel =       getValue(data.fuelid)
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
   
-- local myPxHeight = math.floor(battpercent * 0.37) --draw level
--    local myPxY = 13 + 37 - myPxHeight

--    lcd.drawPixmap(1, 3, "/SCRIPTS/BMP/battery.bmp")

--   lcd.drawFilledRectangle(5, myPxY, 24, myPxHeight, FILL_WHITE )
   
    local i = 38
    while (i > 0) do 
    lcd.drawLine(6, 12 + i, 26, 12 +i, SOLID, GREY_DEFAULT)
    i= i-2
  end


--  if ( CalcBattResistance==1 and  goodIteams<25) or ResCalcError > 10  then--check if we have good samples from Battery Resistance Compensattion
--    drawText(3,1, "~", SMLSIZE)
--  end
  
     if battpercent < 10 or battpercent >=100 then
        drawText(12,0, round(battpercent).."%",INVERS + SMLSIZE + BLINK)
     else
        drawText(11,0, round(battpercent).."%" ,SMLSIZE)
     end
  
   if (HVlipoDetected == 1 and data.battsum >=10) then
     lcd.drawNumber(0,57, data.battsum*10,PREC1+ LEFT )
   else
     lcd.drawNumber(0,57, data.battsum*100,PREC2 + LEFT )
   end


    if HVlipoDetected == 1 then
      drawText(getLastPos(), 57,"H", BLINK, 0) 
    end
    drawText(getLastPos(), 57, "V ", 0)
    drawText(getLastPos(), 58, battype.."S" , SMLSIZE)
   

-- ###############################################################
-- Display RSSI data
-- ###############################################################
    
    if data.rssi > 38 then
      rxpercent =round(rxpercent*0.5+0.5*(((math.log(data.rssi-28, 10)-1)/(math.log(72, 10)-1))*100))
      if rxpercent > 100 then
          rxpercent = 100
      end
    else
      rxpercent=0
    end
  
    lcd.drawPixmap(164, 6, "/SCRIPTS/BMP/RSSI"..math.ceil(rxpercent*0.1)..".bmp") --Round rxpercent to the next higer 10 Percent number and search&draw pixmap
   
    drawText(184, 57, rxpercent, 0) 
    drawText(getLastPos(), 58, "% RX", SMLSIZE)
   
   
-- ###############################################################
-- Timer Drawing 
---- ###############################################################
--        local timer = model.getTimer(0)
--        drawText(36, 44, " Timer : ",SMLSIZE)
--        lcd.drawTimer(getLastPos(), 40, timer.value, MIDSIZE)
    
    
-- ###############################################################
-- Vertical Speed Drawing
-- ###############################################################
 

      drawText(36,44, "Vspeed: ",SMLSIZE)
      
      if settings['imperial'] ~=0 then
        drawText(getLastPos(), 40, round(Vspeed*3.28,1) , MIDSIZE) 
        drawText(getLastPos(), 44, "fs", 0)
      else
        drawText(getLastPos(), 40, round(Vspeed,1) , MIDSIZE) 
        drawText(getLastPos(), 44, "ms", 0)
      end
   
 
 
-- ###############################################################
-- Speed Drawing
-- ###############################################################
      
      drawText(38,29, "Speed : ",SMLSIZE,0)
      
      if settings['imperial'] ~=0 then
        drawText(getLastPos(), 25, round(data.spd*1.149), MIDSIZE)
        drawText(getLastPos(), 29, "mph", SMLSIZE)
      else
	drawText(getLastPos(), 25, round(data.spd*1.851*2), MIDSIZE)
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
   local temp
   if settings['imperial']~=0 then
    temp=data.alt*3.28 
    else
    temp=data.alt
  end
  
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
 
   if DisplayTimer==1 then
      drawText(46, 58, "Used: "..(round(totalbatteryComsum))..'mAh',SMLSIZE)
      elseif DisplayTimer==0 then
         drawText(46, 58, "Cell: "..(CellVolt)..'V',SMLSIZE)
   end
   
  
-- ###############################################################
-- efficient  Calc and Drawing
-- ############################################################### 
  
  if data.spd > 10 then --draw wh per km
     
    if settings['imperial']==0 then
      effizient = effizient*0.8+(0.2*(data.current*data.battsum/data.spd))--spdint can not be 0 because the previus if
      drawText(98, 58,"  effiz: "..round(effizient,1)..'Wh/km', SMLSIZE)
    else
      effizient = effizient*0.8+(0.2*(data.current*data.battsum/(data.spd*0.621371)))
      drawText(98, 58,"  effiz: "..round(effizient,1)..'Wh/mi', SMLSIZE) 
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

local FlightModeName = {}

  -- APM Flight Modes
  FlightModeName[0]="Stabilize"
  FlightModeName[1]="Acro"
  FlightModeName[2]="Alt Hold"
  FlightModeName[3]="Auto"
  FlightModeName[4]="Guided"
  FlightModeName[5]="Loiter"
  FlightModeName[6]="RTL"
  FlightModeName[7]="Circle"
  FlightModeName[8]="Invalid Mode"
  FlightModeName[9]="Landing"
  FlightModeName[10]="Optic Loiter"
  FlightModeName[11]="Drift"
  FlightModeName[12]="Invalid Mode"
  FlightModeName[13]="Sport"
  FlightModeName[14]="Flip"
  FlightModeName[15]="Auto Tune"
  FlightModeName[16]="Pos Hold"
  FlightModeName[17]="Brake"

  -- PX4 Flight Modes
  FlightModeName[18]="Manual"
  FlightModeName[19]="Acro"
  FlightModeName[20]="Stabilized"
  FlightModeName[21]="RAttitude"
  FlightModeName[22]="Position"
  FlightModeName[23]="Altitude"
  FlightModeName[24]="Offboard"
  FlightModeName[25]="Takeoff"
  FlightModeName[26]="Pause"
  FlightModeName[27]="Mission"
  FlightModeName[28]="RTL"
  FlightModeName[29]="Landing"
  FlightModeName[30]="Follow"

  FlightModeName[31]="No Telemetry"

  if data.flightmodeNr < 0 or data.flightmodeNr > 31 then
      data.flightmodeNr=12    
  
    elseif data.flightmodeId ==-1 or ( rxpercent==0 and data.flightmodeNr==0 )then
      data.flightmodeNr=31
  end
    
    
    drawText(68, 1, FlightModeName[data.flightmodeNr], MIDSIZE)
    
    if data.flightmodeNr~=lastflightModeNumber and SayFlightMode == 1 then
      playFile("/SCRIPTS/WAV/AVFM"..data.flightmodeNr.."A.wav")
    lastflightModeNumber=data.flightmodeNr
  end
  

-- ###############################################################
-- Flightmode Image
-- ###############################################################

    if data.flightmodeNr == 6 or data.flightmodeNr == 9 or data.flightmodeNr == 28 or data.flightmodeNr == 29 then
      lcd.drawPixmap(50, 2, "/SCRIPTS/BMP/H.bmp")  
    elseif (data.flightmodeNr >= 0 and data.flightmodeNr <= 2) or (data.flightmodeNr >= 18 and data.flightmodeNr <= 23) then
      lcd.drawPixmap(50, 2, "/SCRIPTS/BMP/stab.bmp")
    elseif data.flightmodeNr~=-1 or data.flightmodeNr~=12 then
      lcd.drawPixmap(50, 2, "/SCRIPTS/BMP/gps.bmp")
    end
  

-- ###############################################################
-- GPS Fix
-- ###############################################################

    local gpsFix =  (data.gpssatcount%10)
    local satCount =   (data.gpssatcount -  (data.gpssatcount%10))*0.1
    
    if data.gpssatsid==-1 then 
      drawText(68, 15, "Check Telemetry Tem2", SMLSIZE)
        
    
    elseif gpsFix >= 4 then
        drawText(70,15, "3D D.GPS, "..satCount..' Sats', SMLSIZE)
        if GPSOKAY==1 and satCount>6 then
          GPSOKAY=3
          playFile("gps.wav") 
          playFile("good.wav") 
        end

    elseif gpsFix == 3 then
        drawText(70,15, "3D FIX, "..satCount..' Sats', SMLSIZE)
        if GPSOKAY==1 and satCount>6 then
          GPSOKAY=3
          playFile("gps.wav") 
          playFile("good.wav") 
        end
        
    elseif gpsFix == 2 then
        drawText(70,15, "2D FIX, "..satCount..' Sats', BLINK+SMLSIZE)
        
    
    else 
        drawText(70,15, "NO FIX, "..satCount.." Sats", BLINK+SMLSIZE) 
        if GPSOKAY==3 then
          GPSOKAY=1
          playFile("gps.wav") 
          playFile("bad.wav")
        end
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
    
  if firsttime==0 then
    playFile("/SCRIPTS/WAV/welcome.wav") 
  end
  if firsttime<60 then
    lcd.drawPixmap(0, 0, "/SCRIPTS/BMP/LuaPiloL.bmp")
    lcd.drawPixmap(106, 0, "/SCRIPTS/BMP/LuaPiloR.bmp")
    firsttime=firsttime+1
    return 0
  end
  
  if event ==  64  then --if menu key pressed that Reset All Variables.  
    playFile("/SCRIPTS/WAV/reset.wav")
    killEvents(64) 
    ResetVar() 
  end
  
  backgroundwork()
  
  if HeadingOrDist ==1 or HeadingOrDist ==2       then loadGpsData() end
  if CalcBattResistance==1                        then BatteryResistanceCalc() end
  if BattLevelmAh > 0 then
      BatteryLevelCalcmah()
  elseif BattLevelmAh < 0 then
      battpercent = data.fuel
  else
      BatteryLevelCalcVoltage()
  end
  if SaybatteryPercent==1                         then SayBattPercent()     end
  
  CalcDisplayTimer()
  VSpeedCalc() 
  lcd.clear()
  draw()
end

--------------------------------------------------------------------------------
-- SCRIPT END
--------------------------------------------------------------------------------
return {run=run,  background=background}

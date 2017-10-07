
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


--#########################################################################################################
--#   LuaPilot v2.207  Lua Telemetry Script for Taranis                                                   #
--#                                                                                                       #
--#  + with opentx 2.17 and above, tested with D4r-II & D8R & X4R & X8R                                   #
--#  + works with Ardu-Copterp&Plane, DJJ, Naze, Naze32, Cleanflight, APM, Pixhawk & more                 #
--#                                                                                                       #
--#  Thanks to SockEye, Richardoe, Schicksie,lichtl			                                                  #    
--#  _ben&Jace25(FPV-Community) ,Clooney82&fnoopdogg and nedimh90(Beta tester)                            #
--#         									                                                                            #
--#  LuaPilot © 2016 ilihack 							                                                                #
--#########################################################################################################
--SETUP:                                                                                                  #
--                                                                                                        #
local FC= 1 --FlightController: 0=Auto,1=Arducopter,2=Arduplane,3=PX4,4=DJI,5=Cleanflight,6=Taulabs,9=Other 
local BatterymahAlarm = 0 --0=off or like 2200 for Alarming if you used more 2200mAh                      #
local CellVoltAlarm = 3.3 --0=off or like 3.3 to get an Alarm if you have less than 3.3Vcell              #                         
--                                                                                                        #
--#########################################################################################################                
-- Advance Setup:                                                                                       #
--                                                                                                        #
local SaybatteryPercent=1 ---0=off or 1 if you will hear you Battery percent in 10% Steps                 #
local HeadingOrDist = 2       --draw Hdg=0 / Draw Distance=1 / Draw Both alternate=2                      #           
local MaxAvarageAmpere=0 -- 0=Off, Alarm if the average 15s current is over this Value                    #
local CalcBattResistance=0 --0=off 1=AutoCalc Lipo Resistance and correct Lipo.Level ALPHA                #   
local battype=0   -- 0=Auto detection (1s,2s,3s,4s,6s,8s) or like 7 for an 7s Battery Conf                #
local GPSOKAY=1 --1=play Wav files for GPS Status, 0= Disable                                             # 
local SayFlightMode = 1 --0=off or 1=on then play wav for Flight modes changes                            #
local BattLevelmAh = 1 -- 0 = Calc Battery % ONLY from Battery Volt                                       #
                       -- 1 = if available get Batt % from Flight controller else calk from Volt          #
                       -- a Value bigger l like 2200 to Calk the Battery % instead from THIS mAh Value    #
--                                                                                                        #
--#########################################################################################################
  
  
  


 dataR = {} --array for Raw Telemetry Values
 dataC = {} --array for Calculated Values
  
    --init RAW Telemetry Variables 
    dataR.VFAS =       0
    dataR.Alt =        0
  --dataR.GAlt =       0
    dataR.GSpd =       0
    dataR.Curr =       0
    dataR.Cnsp =       0
    dataR.Tmp1=        0
    dataR.RSSI =       0
    dataR.Tmp2=        0
    dataR.Hdg =        0

    dataR.Vspd =       0
    dataR.Fuel =       0 
    dataR.Dist =       0
    dataR.AccX =       0
    dataR.AccY =       0
    
    dataR.AccZ =       0
    dataR.RPM =        0 
    dataR.ASpd =       0


--init Calculated Variables who needed in Drawing
  --init var for v. speed Calk
    dataC.Vspd = 0.0
    dataC.PrevAlt = 0.0

  --init Battery and consume
  --dataC.Cnsp = 0.0
    dataC.HVlipo= 0 
    dataC.FuelP = 0
    dataC.CellVolt=0.0

  --dataC.CellResi=0.0
    dataC.effizient=0.0
    dataC.Prevfmode = 0
    dataC.currAvarg=-0.3
    dataC.Dist=0.0
    dataC.RSSIperc = 0
    dataC.gpsFix = 0
    dataC.satCount = 0
    dataC.fmode=0
    dataC.Armed = false
    dataC.PrevArmed= false
    
  --dataC.GpsHome = false
    dataC.GSpd = 0
    dataC.fmodetext = "Invalid Mode"
    dataC.fmodepic=0
    dataC.HdgOrt=""
    dataC.FCName ="Unknown FC"
  --Script Initiation end  
    


    local oldTime={0,0,0,0,0,0}
    local Time={0,0,0,0,0,0}

  --init var for Battery Resistance Calc.
    local CurrA={}
    local CellVoltA={}
    local ResCalcError=0
    local ArrayIteam=0
    local goodIteams=0
    local AraySize= 200--set the Size of the Ring Resistance Array
    
    local firsttime=0
    local DisplayTimer=0
    local settings = getGeneralSettings()  
    local lastsayP=100


  local function getTelemetryId(name)
   field = getFieldInfo(name)
   if getFieldInfo(name) then return field.id end
    return -1
  end
  
  
  local function initteleids()
    
  --APM Telemetry and backbone for the other Flighcontrollers
    dataR.VFASid =       getTelemetryId("VFAS")
    dataR.Altid =        getTelemetryId("Alt")
  --dataR.GAltid =       getTelemetryId("GAlt") --Unused
    dataR.GSpdid =       getTelemetryId("GSpd")
    dataR.GPSid =        getTelemetryId("GPS")
    dataR.Currid =       getTelemetryId("Curr")
    dataR.Cnspid =       getTelemetryId("Cnsp")
    dataR.Tmp1id =       getTelemetryId("Tmp1") --apm: Flight mode Info’s
    dataR.Tmp2id =       getTelemetryId("Tmp2") --apm: gpsat Info’s
    dataR.RSSIid =       getTelemetryId("RSSI")
    dataR.Hdgid =        getTelemetryId("Hdg")
    dataR.Fuelid =       getTelemetryId("Fuel") 
    
    
  --for DJJ Telemetry with Any sense this come extra
    dataR.VSpdid =       getTelemetryId("VSpd") --coming future to apm
    dataR.Distid =       getTelemetryId("Dist") --maybe calculate
    dataR.AccXid =       getTelemetryId("AccX") --coming future to apm
    dataR.AccYid =       getTelemetryId("AccY") --coming future to apm
    
  --and for Naze32 Telemetry the above plus this extra:
    dataR.AccZid =       getTelemetryId("AccZ") --coming future to apm
    dataR.RPMid =        getTelemetryId("RPM") --indicator for Naze32
    dataR.ASpdid =       getTelemetryId("ASpd") --coming future to apm
   
  end
  
  initteleids() --call the above function






--------------------------------------------------------------------------------  
--------------------------------------------------------------------------------   
--------------------------------------------------------------------------------
-- functions 
-------------------------------------------------------------------------------- 
--------------------------------------------------------------------------------  
--------------------------------------------------------------------------------  
  
  
  
--------------------------------------------------------------------------------
-- function Get New Telemetry Value 
--------------------------------------------------------------------------------
  local function GetnewTelemetryValue()
    
    local getValue = getValue --localize because this is faster
    
  
      --APM Telemetry and backbone for the other Flighcontrollers
    dataR.VFAS =       getValue(dataR.VFASid)
    dataR.Alt =        getValue(dataR.Altid)
  --dataR.GAlt =       getValue(dataR.GAltid)
    dataR.GSpd =       getValue(dataR.GSpdid)
    dataR.GPS =        getValue(dataR.GPSid)
    dataR.Curr =       getValue(dataR.Currid)
    dataR.Cnsp =       getValue(dataR.Cnspid)
    dataR.Tmp1 =       getValue(dataR.Tmp1id) --apm: Flight mode Info’s
    dataR.Tmp2 =       getValue(dataR.Tmp2id) --apm: gpsat Info’s
    dataR.RSSI =       getValue(dataR.RSSIid)
    dataR.Hdg =        getValue(dataR.Hdgid)
    dataR.Fuel =       getValue(dataR.Fuelid) 
    
  --for DJJ Telemetry Withy Any sense this come extra
    dataR.Vspd =       getValue(dataR.VSpdid)
    dataR.Dist =       getValue(dataR.Distid)
    dataR.AccX =       getValue(dataR.AccXid)
  --dataR.AccY =       getValue(dataR.AccYid)
    
  --and for Naze32 Telemetry the above plus this extra:
  --dataR.AccZ =       getValue(dataR.AccZid)
    dataR.RPM =        getValue(dataR.RPMid)
    dataR.ASpd =       getValue(dataR.ASpdid) --Airspeed
    
  end
  
  
  
  
  
--------------------------------------------------------------------------------
-- AutoDetect Flight controller (try...)
-------------------------------------------------------------------------------- 

function Autodetect()
  --Let’s try to Autodetect
  if FC==0 then
      if dataR.AccXid == -1 and dataR.AccYid == -1 then --APM
        FC=1
      elseif dataR.RPMid ~= -1 and dataR.AccZid ~= -1 then --Taulabs
        FC=6
      elseif  dataR.AccZid ==-1 and dataR.RPMid == -1  then --DJJ Detect maybe a better detection is if true dataR.Tmp2==dataR.fuel%100
        FC=4
      elseif dataR.RPMid == -1 and dataR.AccZid ~=-1  then --Cleanflight
        FC=5
      else --No above flight controller Detect, take Unknown
        FC=9
      end
  end 


  if     FC==1 then
      dataC.FCName ="ArduCopter" --todo try how it’s possible to check if it Arducopter or arduplane or Px4
  elseif FC==2 then
      dataC.FCName ="ArduPlane"
  elseif FC==6 then
      dataC.FCName ="Taulabs"
  elseif FC==4 then
      dataC.FCName ="DJJ"
  elseif FC==5 then
      dataC.FCName ="Cleanflight"
  else   
      dataC.FCName ="Unknown FC."
  end

dataR.AccZid,dataR.AccZ,dataR.AccY,dataR.AccYid=nil,nil,nil,nil

end 


--------------------------------------------------------------------------------
-- function   Calculate from Hdg the Actual Direction
-------------------------------------------------------------------------------
function calcHdgOrt()

    if dataR.Hdg <0 or dataR.Hdg >360 then dataC.HdgOrt="Error"  
      elseif dataR.Hdg <  22.5  then dataC.HdgOrt="N"     
      elseif dataR.Hdg <  67.5  then dataC.HdgOrt="NO" 
      elseif dataR.Hdg <  112.5 then dataC.HdgOrt="O"  
      elseif dataR.Hdg <  157.5 then dataC.HdgOrt="OS" 
      elseif dataR.Hdg <  202.5 then dataC.HdgOrt="S"  
      elseif dataR.Hdg < 247.5  then dataC.HdgOrt="SW"    
      elseif dataR.Hdg < 292.5  then dataC.HdgOrt="W"     
      elseif dataR.Hdg < 337.5  then dataC.HdgOrt="WN"    
      elseif dataR.Hdg <= 360.0 then dataC.HdgOrt="N"    
    end

end 


------------------------------------------------------------------------------
-- function Reset Variables (after pressing long Menu)
-------------------------------------------------------------------------------
local function ResetVar() 
  
    initteleids() --reinet Telemetrie ID.S.
  

    Time={0,0,0,0,0,0}
    dataC.Vspd = 0.0
    dataC.PrevAlt = 0.0
  --dataC.Cnsp = 0.0
    dataC.FuelP = 0
    dataC.CellVolt=0.0
    CurrA={}
    CellVoltA={}
    dataC.CellResi=0.0
    ArrayIteam=0 
    dataC.effizient=0.0
    dataC.Prevfmode = 0
    dataC.currAvarg=0.0
    dataC.Dist=0.0
    lastsayP=100
    battype=0
    firsttime=1
    settings = getGeneralSettings()
    dataC.lon=nil
    dataC.lat=nil
    
	end
  
  
--------------------------------------------------------------------------------
-- function Round
-------------------------------------------------------------------------------- 
function round(num, idp)
    local temp = 10^(idp or 0)
    if num >= 0 then return math.floor(num * temp + 0.5) / temp
  else return math.ceil(num * temp - 0.5) / temp end
  temp=nil
end


--------------------------------------------------------------------------------
-- function check and if change Say the GPS Stat
--------------------------------------------------------------------------------
local function checkASayGPS()
  
    if dataC.gpsFix >= 4 then
        if GPSOKAY==1 and dataC.satCount>6 then
          GPSOKAY=3
          playFile("/SCRIPTS/WAV/GoodGPS.wav") 
        end

    elseif dataC.gpsFix == 3 then
        if GPSOKAY==1 and dataC.satCount>6 then
          GPSOKAY=3
          playFile("/SCRIPTS/WAV/GoodGPS.wav")
        end
        
    elseif dataC.gpsFix == 1 then
        if GPSOKAY==3 then
          GPSOKAY=1
          playFile("/SCRIPTS/WAV/BadGPS.wav")
        end
    end
    
end



--------------------------------------------------------------------------------
-- function Say battery percent
--------------------------------------------------------------------------------
local function SayBattPercent()  

  if (dataC.FuelP < (lastsayP-10)) then --only say in 10 % steps
       
    Time[6] = Time[6] + (getTime() - oldTime[6]) 
        
    if Time[6]> 700 then --and only say if dataC.FuelP 10 % below for more than 10sec
      lastsayP=(round(dataC.FuelP*0.1)*10)
      Time[6] = 0
      playFile("/SCRIPTS/WAV/"..round(lastsayP).."P.wav")
      --playNumber(round(dataC.FuelP), 8, 0)
      if lastsayP <= 10 then 
        playFile("/SCRIPTS/WAV/BadLipoC.wav")
      end
    end
    
    oldTime[6] = getTime() 
  
  else    
    Time[6] = 0
    oldTime[6] = getTime() 
  end
  
end
 
--------------------------------------------------------------------------------
-- function check and say new flight mode
--------------------------------------------------------------------------------
local function  checkASayFlightMode()

  if dataC.fmode~=dataC.Prevfmode  then
      playFile("/SCRIPTS/WAV/FC"..FC.."M"..dataC.fmode..".wav")
      dataC.Prevfmode=dataC.fmode
    end
  

  if dataC.Armed ~= dataC.PrevArmed then
      if dataC.Armed then 
      playFile("/SCRIPTS/WAV/Armed.wav")
      else
      playFile("/SCRIPTS/WAV/Disarmed.wav")
      end
      dataC.PrevArmed = dataC.Armed
    end
  
  
end
 

--------------------------------------------------------------------------------
-- function Vertical Speed Calk
--------------------------------------------------------------------------------
local function Vspeed()  --todo maybe take it from VSpd in opentx
  
local temp = 0.0 --Value holder

      Time[2] = Time[2] + (getTime() - oldTime[2])
      if  dataR.Alt~=dataC.PrevAlt or Time[2]>130 then --1300 ms
        temp  = ( (dataR.Alt-dataC.PrevAlt) / (Time[2]/100) )
        Time[2] = 0 
        dataC.PrevAlt=dataR.Alt
      end
      oldTime[2] = getTime() 
      
      if dataC.Vspd <10 then
        dataC.Vspd=temp*0.3 + dataC.Vspd*0.7
      else 
        dataC.Vspd=temp*0.1 + dataC.Vspd*0.90
      end
  end
   
   
--------------------------------------------------------------------------------
-- function Lipo Cell Dection 
--------------------------------------------------------------------------------
   local function BatteryCalcCellVoltageAndTyp()  
       
       if math.ceil(dataR.VFAS/4.37) > battype and dataR.VFAS<4.37*8 then 
          battype=math.ceil(dataR.VFAS/4.37)
         
          if battype==7 then battype=8 end--don’t Support 5s&7s Battery detection, its Danger to Detect exampel:if you have an Empty 8s its lock like an 7s...
          if battype==5 then battype=6 end 
         
          if dataR.VFAS > 4.22*battype then --HVLI is detected
            dataC.HVlipo=1
          else
            dataC.HVlipo=0
          end
        end
      
      if battype > 0 then 
      dataC.CellVolt= dataR.VFAS/battype 
     end
  end
  
----------------------------------------------------------------------------------
---- function cont. Lipo Resistance Calculate V0.73 ALPHA ilihack
----------------------------------------------------------------------------------
local function BatteryResistanceCalc() --Need dataC.CellVoltand current from Telemetry Sample’s and Calk the Resistance with it
  local temp=0 --only an Value holder in calks
  local sum_x=0
  local sum_y=0
  local sum_xx=0
  local sum_xy=0

  
  if ArrayIteam==0 then --init Array when its first Time
    goodIteams=0
    ResCalcError=0
    for i=1,AraySize do
      CurrA[i]=0
      CellVoltA[i]=0
    end
  end
  

  if ArrayIteam < AraySize  then ArrayIteam=ArrayIteam+1 else ArrayIteam=1  --if on the end Return to the begin and overwrite old Values
  end 
  
  if ( dataC.CellVolt>2.5 and dataC.CellVolt<4.5 and dataR.Curr>0.1 and dataR.Curr<180 ) then --check if values are in range and save New Samples in Array) 
      if CellVoltA[ArrayIteam]==0 then goodIteams=goodIteams+1 end
      CellVoltA[ArrayIteam]=dataC.CellVolt
      CurrA[ArrayIteam]=dataR.Curr
  else
      if CellVoltA[ArrayIteam]~=0 then goodIteams=goodIteams-1 end
      CellVoltA[ArrayIteam]=0
      CurrA[ArrayIteam]=0
  end
  
  if goodIteams>(AraySize*0.7) then --if cache 80 % filled begin to calk
  ---Start Linear Regression over the Volt & Current Arrays    
    for i=1,AraySize do
        local curr=CurrA[i]
        local volt=CellVoltA[i]
        sum_x=sum_x+curr
        sum_y=sum_y+volt
        sum_xx=sum_xx+curr*curr
        sum_xy=sum_xy+(curr*volt)
    end
    
    temp=(sum_x*sum_y-goodIteams*sum_xy)/(goodIteams*sum_xx-sum_x*sum_x) --calk the coeffiz m of the Linear func because they symbolize the Battery Resistance

  sum_x,sum_y,sum_xx,sum_xy=nil,nil,nil,nil


    if (temp > 0.001 and temp < 0.20 ) then --check if in Range 1- 200mohm per cell
      if dataC.CellResi==0 then --init for faster filtering
        dataC.CellResi=temp
      else
        dataC.CellResi=dataC.CellResi*0.99 +temp*0.01 --Update Cell resistance             
      end
    end
     
      
  ---if Resistance okay corrected Voltage else count error
    temp=(dataR.Curr*dataC.CellResi) --Calk temp calk cellvolt Drift
    if ((dataC.HVlipo==1 and dataC.CellVolt+temp>4.45) or (dataC.CellVolt+temp>4.3 and dataC.HVlipo==0)) then --not in Range
      ResCalcError=ResCalcError+1
      if ResCalcError ==5 then 
        playFile("/SCRIPTS/WAV/errorres.wav")
        ArrayIteam=0
      end
    elseif (ResCalcError < 10 and dataC.CellVolt~=0 ) then --not much errors happened
      dataC.CellVolt=dataC.CellVolt+temp --correct Cell Voltage with calculated Cell Resistance  
    end
    
  end
end 


--------------------------------------------------------------------------------
  -- function Lipo Range Calculate with COMSUME
--------------------------------------------------------------------------------
  local function BatteryLevelCalcmah()  --calk Battery Percent with mah consume
    dataC.FuelP= round((100/BattLevelmAh)*(BattLevelmAh-dataR.Cnsp))  
    
    if dataC.FuelP<-5 then BattLevelmAh=0 end --fallback to calk battery % from Voltage
  end 
  

--------------------------------------------------------------------------------
-- function Lipo Range Calculate with Voltage
--------------------------------------------------------------------------------
 local function BatteryLevelCalcVoltage()  
    
    local temp=0 --for cellvolt and unfiltered dataC.FuelP placeholder
    
    --Correction for HVlipo Batter Percent Estimation
    if dataC.HVlipo==1 then 
      temp = dataC.CellVolt-0.15 --for HVlipo give us a better estimation
    else
      temp = dataC.CellVolt
    end 

--Voltage to Percente Translation
    if temp > 4.2                      then temp = 100
elseif temp < 3.2                      then temp = 0
elseif temp >= 4                       then temp = 80*temp - 236
elseif temp <= 3.67                    then temp = 29.787234 * temp - 95.319149 
elseif temp > 3.67 and temp < 4        then temp = 212.53*temp-765.29
end

    if dataC.FuelP==0 then 
      dataC.FuelP=round(temp) --init dataC.FuelP
    else 
      dataC.FuelP=round(dataC.FuelP*0.98 + 0.02*temp) --smoothing
    end
  temp=nil
end

 
--------------------------------------------------------------------------------
-- function Current Total Calk Consume function --todo maybe from opentx cnps without calculation
-------------------------------------------------------------------------------
   local function totalConsume()
      Time[1] = Time[1] + (getTime() - oldTime[1])
      if Time[1] >=20 then --200 ms
        dataC.Cnsp  = dataC.Cnsp + ( dataR.Curr * (Time[1]/360))
        Time[1] = 0
      end
      oldTime[1] = getTime() 

    end

--------------------------------------------------------------------------------
-- function check if BatterymahAlarm max reached dataC.Cnsp 
--------------------------------------------------------------------------------
  local function AlarmifmaxMah()
    if BatterymahAlarm  > 0 and BatterymahAlarm < dataR.Cnsp then 
      playFile("/SCRIPTS/WAV/BadLipoC.wav")
      BatterymahAlarm=0
    end
	end
 
 --------------------------------------------------------------------------------
-- function check if Cell Volt lower then min Volt Setting
--------------------------------------------------------------------------------
 local function AlarmifVoltLow()
 
    if dataC.CellVolt < CellVoltAlarm and dataR.VFAS >0.5  then 
        Time[3] = Time[3] + (getTime() - oldTime[3])
        if Time[3] >=800 then --8s
          playFile("/SCRIPTS/WAV/BadLipoV.wav")
          Time[3] = 0
        end
      oldTime[3] = getTime()
    end
	end

--------------------------------------------------------------------------------
-- function check if average Amp over max Setting
-------------------------------------------------------------------------------
 local function AlarmifOverAmp() 
 
    dataC.currAvarg=dataR.Curr*0.01+dataC.currAvarg*0.99
    if dataC.currAvarg  > MaxAvarageAmpere  then 
        Time[4] = Time[4] + (getTime() - oldTime[4])
        if Time[4] >=750 then --2,5s
          playFile("/SCRIPTS/WAV/BadLipoA.wav")
          Time[4] = 0
        end
      oldTime[4] = getTime()
    end
 
end


--------------------------------------------------------------------------------
-- function Display Timer to draw alternately
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
-- functions calk GPS Distance
-------------------------------------------------------------------------------

local function loadGpsData()   

  if GPSOKAY==3 and (type(dataR.GPS) == "table") then
    
    if dataR.GPS["lat"] ~= nil and dataC.lat==nil then
	    dataC.lat = dataR.GPS["lat"]
	  elseif dataR.GPS["lon"] ~= nil and dataC.lon==nil then
	    dataC.lon = dataR.GPS["lon"]
	  else
      local sin=math.sin--locale are faster
      local cos=math.cos
      local z1 = (sin(dataC.lon - dataR.GPS["lon"]) * cos(dataC.lat) )*6358364.9098634
	    local z2 = (cos(dataR.GPS["lat"]) * sin(dataC.lat) - sin(dataR.GPS["lat"]) * cos(dataC.lat) * cos(dataC.lon - dataR.GPS["lon"]) )*6358364.9098634 
      dataC.Dist =  (math.sqrt( z1*z1 + z2*z2))/100
      
      sin,cos,z1,z2=nil,nil,nil,nil
      
    end      
            
  end
end



-------------------------------------------------------------------------------- 
--------------------------------------------------------------------------------
-- Special Telemetry encodings, conversions and Flight Mode Names for each Flight Controller
-------------------------------------------------------------------------------- 
-------------------------------------------------------------------------------- 
local function FlightController()

  
----Taulabs ####################### #######################  #######################  
  if FC==6 then 
    
--    if math.floor(dataR.Tmp1 *0.01) == 5 then 
--      dataC.gpsFix=4
--      --dataC.GpsHome = true 
--    else
--      --dataC.GpsHome = false
--      dataC.gpsFix=(dataR.Tmp1 *0.01)
--    end
    
--    dataC.satCount = math.floor(dataR.Tmp1 % 100)
    
    
--    if math.floor(dataR.RPM * 0.01) == 1 then 
--     dataC.Armed = false
--    else
--     dataC.Armed = true
--    end  
    
--    dataC.fmode = math.floor(dataR.RPM % 100)
    
    
--    --vdop = bit32.rshift(dataR.Tmp2, 8) / 100
--    --hdop = bit32.band(dataR.Tmp2, 0xff) / 100
  
  
--    if BattLevelmAh == 1 then -- If 1 than calk Battery Percent from Flighcontrollers battery settings
--              dataC.FuelP = dataR.Fuel
--              if dataC.FuelP< -5 then BattLevelmAh=0 end --fallback to calk battery % from Voltage
--              if dataC.FuelP>100 then BattLevelmAh=0 end --fallback to calk battery % from Voltage
--    end 
   
--   dataC.GSpd = dataR.GSpd
   

--   --Vspeed() 
--   dataC.Vspd= dataR.Vspd --todo take Vspeed from opentx instead to calc... must be tested
--     collectgarbage("collect") 
--        if dataC.fmode == 0 then  dataC.fmodetext = "Manual"           ; dataC.fmodepic=1
--    elseif dataC.fmode == 1 then  dataC.fmodetext = "Acro Mode"        ; dataC.fmodepic=1
--    elseif dataC.fmode == 2 then  dataC.fmodetext = "Leveling"         ; dataC.fmodepic=1
--    elseif dataC.fmode == 3 then  dataC.fmodetext = "MWRate"           ; dataC.fmodepic=1 
--    elseif dataC.fmode == 4 then  dataC.fmodetext = "Horizon"          ; dataC.fmodepic=1
--    elseif dataC.fmode == 5 then  dataC.fmodetext = "Axis Lock"        ; dataC.fmodepic=1
--    elseif dataC.fmode == 6 then  dataC.fmodetext = "Virtual Bar"      ; dataC.fmodepic=1
--    elseif dataC.fmode == 7 then  dataC.fmodetext = "Stab Mode 1"      ; dataC.fmodepic=1
--    elseif dataC.fmode == 8 then  dataC.fmodetext = "Stab Mode 2"      ; dataC.fmodepic=1
--    elseif dataC.fmode == 9 then  dataC.fmodetext = "Stab Mode 3"      ; dataC.fmodepic=1
--    elseif dataC.fmode == 10 then dataC.fmodetext = "Auto tune"        ; dataC.fmodepic=1
--    elseif dataC.fmode == 11 then dataC.fmodetext = "AltHold"          ; dataC.fmodepic=1
--    elseif dataC.fmode == 12 then dataC.fmodetext = "PosHold"          ; dataC.fmodepic=2
--    elseif dataC.fmode == 13 then dataC.fmodetext = "RTL Mode"         ; dataC.fmodepic=3
--    elseif dataC.fmode == 14 then dataC.fmodetext = "Path Plan"        ; dataC.fmodepic=2
--    elseif dataC.fmode == 15 then dataC.fmodetext = "Tablet"           ; dataC.fmodepic=2
--    else                          dataC.fmodetext = "Invalid Mode"     ; dataC.fmodepic=0
--    end



--Cleanflight####################### #######################  #######################
elseif FC == 5 then 

-- local temp
--    temp= dataR.Tmp1 % 10
--    if     math.floor(temp) == 1 then dataC.Armed = false --ready to arm
--    elseif math.floor(temp) == 2 then dataC.Armed = false --; FCerror = true --not ready to arm
--    elseif math.floor(temp) == 4 then dataC.Armed = true
--    end
  
--    temp = dataR.Tmp1 % 10000 + 10000  --change the first number to 10000 
    
--    if     math.floor(temp *0.1) == 1001 then dataC.fmode =1 ;    dataC.fmodetext="Angle Mode"     ; dataC.fmodepic=1
--    elseif math.floor(temp *0.1) == 1002 then dataC.fmode =2 ;    dataC.fmodetext="Horizon Mode"   ; dataC.fmodepic=1
--    elseif math.floor(temp *0.1) == 1004 then dataC.fmode =3 ;    dataC.fmodetext="Passthru Mode"  ; dataC.fmodepic=1
--    elseif math.floor(temp *0.01) == 101 then dataC.fmode =6 ;    dataC.fmodetext="Compass Mode"   ; dataC.fmodepic=1
--    elseif math.floor(temp *0.01) == 102 then dataC.fmode =7 ;    dataC.fmodetext="Baro Mode"      ; dataC.fmodepic=1
--    elseif math.floor(temp *0.01) == 104 then dataC.fmode =8 ;    dataC.fmodetext="Sonar Mode"     ; dataC.fmodepic=1
--    elseif math.floor(temp *0.001) == 11 then dataC.fmode =12 ;   dataC.fmodetext="GPS Hold Mode"  ; dataC.fmodepic=2
--    elseif math.floor(temp *0.001) == 12 then dataC.fmode =13 ;   dataC.fmodetext="GPS Home Mode"  ; dataC.fmodepic=3
--    elseif math.floor(temp *0.001) == 14 then dataC.fmode =14 ;   dataC.fmodetext="Headfree Mode"  ; dataC.fmodepic=2
--    end
    
--    collectgarbage("collect")
--    dataC.satCount = math.floor(dataR.Tmp1 % 100)
    
    
--    if math.floor(dataR.Tmp2 * 0.001) == 1 then
--      dataC.gpsFix = 3
--      --dataC.GpsHome=false
--    elseif math.floor(dataR.Tmp2 * 0.001) == 2 then
--      dataC.gpsFix = 1
--      --dataC.GpsHome=true
--    else
--      dataC.gpsFix = 3
--      --dataC.GpsHome=true
--    end
    
    
--   dataC.GSpd=dataR.GSpd/1.851 
--  --Vspeed() 
--   dataC.Vspd= dataR.Vspd --todo take vpsed from opentx instead to calc... must be tested


----DJJ ####################### #######################  #######################  
    
--    elseif FC==4 then 
--      collectgarbage("collect")
--      --Encode Any sense multiple Telemetry Values who stored in fuel  
--        local temp = dataR.Fuel

--        dataC.satCount = temp % 100
--        temp = math.floor((temp - dataC.satCount ) *0.01)

--        dataC.gpsFix = temp % 10
--        temp = math.floor((temp - dataC.gpsFix) *0.1)

--        dataC.fmode = temp % 10 
--        temp = math.floor((temp - dataC.fmode) *0.1)

--        if bit32.band(temp, 1) == 1 then dataC.Armed = true
--        else dataC.Armed = false
--        end   
        
--        if bit32.band(temp, 2) == 2 then 
--          --dataC.GpsHome = true
--      else 
--        --dataC.GpsHome = false
--      end
     
--        temp=nil
--        collectgarbage("collect")
        
--        dataC.GSpd=dataR.GSpd/1.851 --bring the kmh back to knotts per second
--      --Vspeed() 
--        dataC.Vspd= dataR.Vspd --todo take Vspeed from opentx instead to calc... must be tested
        
--            if     dataC.fmode==0  then  dataC.fmodetext="Manual Mode"         ; dataC.fmodepic=1
--            elseif dataC.fmode==1  then  dataC.fmodetext="GPS Mode"            ; dataC.fmodepic=2
--            elseif dataC.fmode==2  then  dataC.fmodetext="Failsafe"            ; dataC.fmodepic=3
--            elseif dataC.fmode==3  then  dataC.fmodetext="ATTI Mode"           ; dataC.fmodepic=1
--            else                         dataC.fmodetext="Invalid mode"        ; dataC.fmodepic=0
--            end
   
        
        
----APM Arducopter  ####################### #######################  #######################
      elseif FC==1 then

          --encode from Tmp2 the dataC.gpsFix and satcound
            dataC.gpsFix =  dataR.Tmp2 % 10
            dataC.satCount =   (dataR.Tmp2 -  (dataR.Tmp2 % 10)) * 0.1
            
            dataC.fmode = dataR.Tmp1
            
            if BattLevelmAh == 1 then -- If 1 than calk dataC.FuelP from Flighcontrollers battery settings
              dataC.FuelP = dataR.Fuel
              if dataC.FuelP< -5 then BattLevelmAh=0 end --fallback to calk battery % from Voltage
              if dataC.FuelP>100 then BattLevelmAh=0 end --fallback to calk battery % from Voltage
            end 
              
            dataC.GSpd=dataR.GSpd
            
             if dataR.Tmp1id == -1 or ( dataC.RSSIperc == 0 and dataC.fmode==0 ) then  
              dataC.fmode=42
             end
     
                    --Arducopter Flight modes --todo add follow me mode
                if     dataC.fmode==0 then  dataC.fmodetext="Stabilize"     ; dataC.fmodepic=1
                elseif dataC.fmode==1 then  dataC.fmodetext="Acro Mode"     ; dataC.fmodepic=1
                elseif dataC.fmode==2 then  dataC.fmodetext="Alt Hold"      ; dataC.fmodepic=1
                elseif dataC.fmode==3 then  dataC.fmodetext="Auto Mode"     ; dataC.fmodepic=2
                elseif dataC.fmode==4 then  dataC.fmodetext="Guided Mode"   ; dataC.fmodepic=2
                elseif dataC.fmode==5 then  dataC.fmodetext="Loiter Mode"   ; dataC.fmodepic=2
                elseif dataC.fmode==6 then  dataC.fmodetext="RTL Mode"      ; dataC.fmodepic=3  
                elseif dataC.fmode==7 then  dataC.fmodetext="Circle Mode"   ; dataC.fmodepic=2
                elseif dataC.fmode==9 then  dataC.fmodetext="Landing Mode"  ; dataC.fmodepic=3
                elseif dataC.fmode==10 then dataC.fmodetext="Optic Loiter"  ; dataC.fmodepic=2
                elseif dataC.fmode==11 then dataC.fmodetext="Drift Mode"    ; dataC.fmodepic=2
                elseif dataC.fmode==13 then dataC.fmodetext="Sport Mode"    ; dataC.fmodepic=1
                elseif dataC.fmode==14 then dataC.fmodetext="Flip Mode"     ; dataC.fmodepic=1
                elseif dataC.fmode==15 then dataC.fmodetext="Auto Tune"     ; dataC.fmodepic=1
                elseif dataC.fmode==16 then dataC.fmodetext="Pos Hold"      ; dataC.fmodepic=2
                elseif dataC.fmode==17 then dataC.fmodetext="Brake Mode"    ; dataC.fmodepic=1
                elseif dataC.fmode==42 then dataC.fmodetext="No Telemetry"  ; dataC.fmodepic=0
                else   dataC.fmodetext="Invalid Mode"                       ; dataC.fmodepic=0
                end
           
          
       Vspeed() 
    -- dataR.Vspd= dataR.Vspd --todo take Vspeed from opentx instead to calc... must be tested
    
    
    
----APM Arduplane  ####################### #######################  #######################
      elseif FC==2 then

--          --encode from Tmp2 the dataC.gpsFix and satcound
--            dataC.gpsFix =  dataR.Tmp2 % 10
--            dataC.satCount =   (dataR.Tmp2 -  (dataR.Tmp2 % 10)) * 0.1
            
--            dataC.fmode = dataR.Tmp1
            
--            if BattLevelmAh == 1 then -- If 1 than calk dataC.FuelP from Flighcontrollers battery settings
--              dataC.FuelP = dataR.Fuel
--              if dataC.FuelP< -5 then BattLevelmAh=0 end --fallback to calk battery % from Voltage
--              if dataC.FuelP>100 then BattLevelmAh=0 end --fallback to calk battery % from Voltage
--            end 
              
--            dataC.GSpd=dataR.GSpd
            
--             if dataR.Tmp1id == -1 or ( dataC.RSSIperc == 0 and dataC.fmode==0 ) then  
--              dataC.fmode=42
--             end
         

--                  --ArduPlane Flight modes 
--                if     dataC.fmode==0 then  dataC.fmodetext="Manuel Mode"      ; dataC.fmodepic=1
--                elseif dataC.fmode==1 then  dataC.fmodetext="Circle Mode"      ; dataC.fmodepic=1
--                elseif dataC.fmode==2 then  dataC.fmodetext="Stabilize"        ; dataC.fmodepic=1
--                elseif dataC.fmode==3 then  dataC.fmodetext="Training"         ; dataC.fmodepic=1
--                elseif dataC.fmode==4 then  dataC.fmodetext="Acro Mode"        ; dataC.fmodepic=1
--                elseif dataC.fmode==5 then  dataC.fmodetext="FBW A Mode"       ; dataC.fmodepic=1
--                elseif dataC.fmode==6 then  dataC.fmodetext="FBW B MODE"       ; dataC.fmodepic=1  
--                elseif dataC.fmode==7 then  dataC.fmodetext="Cruise Mode"      ; dataC.fmodepic=2
--                elseif dataC.fmode==8 then  dataC.fmodetext="AutoTune"         ; dataC.fmodepic=0
--                elseif dataC.fmode==10 then dataC.fmodetext="Auto Mode"        ; dataC.fmodepic=2
--                elseif dataC.fmode==11 then dataC.fmodetext="RTL Mode"         ; dataC.fmodepic=3
--                elseif dataC.fmode==12 then dataC.fmodetext="Loiter Mode"      ; dataC.fmodepic=2
--                elseif dataC.fmode==15 then dataC.fmodetext="Guided Mode"      ; dataC.fmodepic=2
--              --elseif dataC.fmode==16 then dataC.fmodetext="Initalising"      ; dataC.fmodepic=0
--                elseif dataC.fmode==17 then dataC.fmodetext="Q Stabilize"      ; dataC.fmodepic=1
--                elseif dataC.fmode==18 then dataC.fmodetext="Q Hover"          ; dataC.fmodepic=1  
--                elseif dataC.fmode==19 then dataC.fmodetext="Q Loiter"         ; dataC.fmodepic=2
--                elseif dataC.fmode==42 then dataC.fmodetext="No Telemetry"     ; dataC.fmodepic=0
--                else   dataC.fmodetext="Invalid Mode"                          ; dataC.fmodepic=0
--                end
          
--       Vspeed() 
--    -- dataR.Vspd= dataR.Vspd --todo take Vspeed from opentx instead to calc... must be tested
      
      ----Pix4 Flightstage Ardupilot  ####################### #######################  #######################
      elseif FC==3 then

--          --encode from Tmp2 the dataC.gpsFix and satcound
--            dataC.gpsFix =  dataR.Tmp2 % 10
--            dataC.satCount =   (dataR.Tmp2 -  (dataR.Tmp2 % 10)) * 0.1
            
--            dataC.fmode = dataR.Tmp1
            
--            if BattLevelmAh == 1 then -- If 1 than calk dataC.FuelP from Flighcontrollers battery settings
--              dataC.FuelP = dataR.Fuel
--              if dataC.FuelP< -5 then BattLevelmAh=0 end --fallback to calk battery % from Voltage
--              if dataC.FuelP>100 then BattLevelmAh=0 end --fallback to calk battery % from Voltage
--            end 
              
--            dataC.GSpd=dataR.GSpd
            
--             if dataR.Tmp1id == -1 or ( dataC.RSSIperc == 0 and dataC.fmode==0 ) then  
--              dataC.fmode=42
--             end
         

--                  --ArduPlane Flight modes 
--                if     dataC.fmode==0 then  dataC.fmodetext="Manuel Mode"       ; dataC.fmodepic=1
--                elseif dataC.fmode==1 then  dataC.fmodetext="Altidue Control"   ; dataC.fmodepic=1
--                elseif dataC.fmode==2 then  dataC.fmodetext="Position Control"  ; dataC.fmodepic=2
--                elseif dataC.fmode==3 then  dataC.fmodetext="Mission"           ; dataC.fmodepic=2
--                elseif dataC.fmode==4 then  dataC.fmodetext="Loiter"            ; dataC.fmodepic=2
--                elseif dataC.fmode==5 then  dataC.fmodetext="RTL"               ; dataC.fmodepic=3
--                elseif dataC.fmode==6 then  dataC.fmodetext="RC Recover"        ; dataC.fmodepic=3
--                elseif dataC.fmode==7 then  dataC.fmodetext="RTGS-Link Loss"    ; dataC.fmodepic=3
--                elseif dataC.fmode==8 then  dataC.fmodetext="Land Engine Fail"  ; dataC.fmodepic=3
--                elseif dataC.fmode==9 then  dataC.fmodetext="Land GPS Fail"     ; dataC.fmodepic=3
--                elseif dataC.fmode==10 then dataC.fmodetext="Acro Mode"         ; dataC.fmodepic=1
--                elseif dataC.fmode==11 then dataC.fmodetext="Unused"            ; dataC.fmodepic=0
--                elseif dataC.fmode==12 then dataC.fmodetext="Descend"           ; dataC.fmodepic=2
--                elseif dataC.fmode==13 then dataC.fmodetext="Termination"       ; dataC.fmodepic=1
--                elseif dataC.fmode==14 then dataC.fmodetext="Offboard"          ; dataC.fmodepic=2  
--                elseif dataC.fmode==15 then dataC.fmodetext="Stabilized"        ; dataC.fmodepic=1
--                elseif dataC.fmode==16 then dataC.fmodetext="Rattitude"         ; dataC.fmodepic=1   
--                elseif dataC.fmode==17 then dataC.fmodetext="Takeoff"           ; dataC.fmodepic=3
--                elseif dataC.fmode==18 then dataC.fmodetext="Land"              ; dataC.fmodepic=3
--                elseif dataC.fmode==19 then dataC.fmodetext="Auto Follow"       ; dataC.fmodepic=2
--                elseif dataC.fmode==20 then dataC.fmodetext="Max"               ; dataC.fmodepic=0
--                else   dataC.fmodetext="Invalid Mode"                           ; dataC.fmodepic=0
--                end
          
--       Vspeed() 
--    -- dataR.Vspd= dataR.Vspd --todo take Vspeed from opentx instead to calc... must be tested
      
      
else --Unknown Flight Controller ####################### #######################  #######################
--        dataC.gpsFix=-2
--        SayFlightMode = 0 
--        dataC.fmode=0
--        dataC.satCount=0
--        Vspeed() 
--        dataC.GSpd=dataR.GSpd/1.851 
--        dataC.fmodepic=1
--        dataC.fmodetext = "LuaPilot"
end
      
end --and function
-------------------------------------------------------------------------------- 
-------------------------------------------------------------------------------- 
  
  
  



  

  
  local function draw()

--  f= loadfile("/SCRIPTS/Telemetry/draw1.lua")
--  f=string.dump(f)
--  f()
--  collectgarbage("collect")

-- dofile("/SCRIPTS/Telemetry/draw1.lua")

--  local f = assert(loadfile("/SCRIPTS/Telemetry/draw1.lua"))
--  local b = string.dump(f)
--  f = assert(loadstring(b))
--  f()


--Here is draw1   you can change everything what you want
-- ###############################################################
---###############################################################
---###############################################################
-- ###############################################################
-- ##                -- Main draw Loop --                       ##
-- ###############################################################  
---###############################################################
---###############################################################
---###############################################################


    
--localize optimization because Locale Are Faster
  local drawText=lcd.drawText 
  local getLastPos=lcd.getLastPos
  local MIDSIZE=MIDSIZE
  local SMLSIZE=SMLSIZE 
    
    

  -- ###############################################################
  -- Battery level Drawing
  -- ###############################################################
    
    local myPxHeight = math.floor(dataC.FuelP * 0.37) --draw level
    local myPxY = 50 - myPxHeight

    lcd.drawPixmap(1, 3, "/SCRIPTS/BMP/battery.bmp")

    lcd.drawFilledRectangle(6, myPxY, 21, myPxHeight, FILL_WHITE )
   
    local i = 38
    while (i > 0) do 
      lcd.drawLine(6, 12 + i, 26, 12 +i, SOLID, GREY_DEFAULT)
      i= i-2
    end
    i=nil
  
     if dataC.FuelP < 10 or dataC.FuelP >=100 then
        drawText(12,0, round(dataC.FuelP).."%",INVERS + SMLSIZE + BLINK)
     else
        drawText(11,0, round(dataC.FuelP).."%" ,SMLSIZE)
     end
  
     if (dataC.HVlipo== 1 and dataR.VFAS >=10) then
       lcd.drawNumber(0,57, dataR.VFAS*10,PREC1+ LEFT ) --draw only with one decimal precision because we have only small place.
     else
       lcd.drawNumber(0,57, dataR.VFAS*100,PREC2 + LEFT ) --draw with two decimal precision.
     end


    if dataC.HVlipo== 1 then
      drawText(getLastPos(), 57,"H", BLINK, 0) 
    end
    drawText(getLastPos(), 57, "V ", 0)
    drawText(getLastPos(), 58, battype.."s" , SMLSIZE)
   

-- ###############################################################
-- Display RSSI data
-- ###############################################################
  
    lcd.drawPixmap(164, 6, "/SCRIPTS/BMP/RSSI"..math.ceil(dataC.RSSIperc*0.1)..".bmp") --Round dataC.RSSIperc to the next higher 10 Percent number and search draw pixmap
   
    drawText(184, 57, dataC.RSSIperc, 0) 
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
        drawText(getLastPos(), 40, round(dataC.Vspd*3.28,1) , MIDSIZE) 
        drawText(getLastPos(), 44, "fs", 0)
      else
        drawText(getLastPos(), 40, round(dataC.Vspd,1) , MIDSIZE) 
        drawText(getLastPos(), 44, "ms", 0)
      end
   
 
 
-- ###############################################################
-- Speed Drawing
-- ###############################################################
      
      drawText(38,29, "Speed : ",SMLSIZE,0)
      
      if settings['imperial'] ~=0 then
        drawText(getLastPos(), 25, round(dataC.GSpd*1.149), MIDSIZE)
        drawText(getLastPos(), 29, "mph", SMLSIZE)
      else     
        drawText(getLastPos(), 25, round(dataC.GSpd*1.851), MIDSIZE)
        drawText(getLastPos(), 29, "kmh", SMLSIZE)
      end    
      
        
-- ###############################################################
-- Distance above rssi  Drawing
-- ###############################################################
     if HeadingOrDist == 1 or (DisplayTimer==1 and HeadingOrDist == 2)  then
     
       if settings['imperial'] ~=0 then
         drawText(163,0, "Dist:"..(round(dataC.Dist*3.28)).."f",SMLSIZE)
       else
        drawText(163,0, "Dist:"..(round(dataC.Dist)).."m",SMLSIZE)
      end
    
     
---- ###############################################################
---- Heading above Rssi Drawing
---- ###############################################################
  
  elseif HeadingOrDist==0 or (DisplayTimer==0 and HeadingOrDist == 2) then
    
    drawText(175,0, dataC.HdgOrt.." "..dataR.Hdg,SMLSIZE)
    drawText(getLastPos(), -2, 'o', SMLSIZE)  
  end
   
   
-- ###############################################################
-- Altitude Drawing
-- ###############################################################
    
   drawText(114,44, "Alt: ",SMLSIZE,0)
   local temp
   if settings['imperial']~=0 then
    temp=dataR.Alt*3.28 
    else
    temp=dataR.Alt
  end
  
   if temp >=10 or temp<-0.1 then
      drawText(getLastPos(), 40, round(temp), MIDSIZE)
   elseif temp<=0.0 and temp>=-0.1 then
      drawText(getLastPos(), 40, 0, MIDSIZE)
   else 
      drawText(getLastPos(), 40, round(temp,1), MIDSIZE)
   end
   
   temp=nil
   
   if settings['imperial']~=0 then
     drawText(getLastPos(), 44, 'f', 0) 
   else
    drawText(getLastPos(), 44, 'm', 0)
   end
   
   
-- ###############################################################
-- Current Total Draw Consume Drawing
-- ###############################################################
 
    drawText(46, 58, "Used: "..(round(dataR.Cnsp))..'mAh',SMLSIZE)
   
  
-- ###############################################################
-- efficient Calk and Drawing
-- ############################################################### 
  
  if dataR.GSpd > 10 then --draw wh per km
     
    if settings['imperial']==0 then
      dataC.effizient = dataC.effizient*0.8+(0.2*(dataR.Curr*dataR.VFAS/dataR.GSpd))--spdint cannot be 0 because the previous if
      drawText(98, 58,"  effiz: "..round(dataC.effizient,1)..'Wh/km', SMLSIZE)
    else
      dataC.effizient = dataC.effizient*0.8+(0.2*(dataR.Curr*dataR.VFAS/(dataR.GSpd*0.621371)))
      drawText(98, 58,"  effiz: "..round(dataC.effizient,1)..'Wh/mi', SMLSIZE) 
    end
  
  else --draw wh per h
     dataC.effizient = dataC.effizient*0.8+0.2*(dataR.Curr*dataR.VFAS)
     drawText(104, 58, " draw: "..(round(dataC.effizient,1))..'W', SMLSIZE)
   end
   

-- ###############################################################
-- Current Drawing
-- ###############################################################

    drawText(113, 29, "Cur: ",SMLSIZE)
    
    if dataR.Curr >=100 then  
      drawText(getLastPos(), 25, round(dataR.Curr),MIDSIZE)
    else 
      drawText(getLastPos(), 25, round(dataR.Curr,1),MIDSIZE)
    end
    
    drawText(getLastPos(), 29, 'A', 0)
    
    
-- ###############################################################
-- Flight Modes Drawing 
-- ###############################################################

    
    if dataC.Armed  then
      drawText(68, 1,'ARM ' ..dataC.fmodetext , MIDSIZE)  --Include Drawing of dataC.Armed 
    else 
      drawText(68, 1, dataC.fmodetext , MIDSIZE)
    end
  
  

-- ###############################################################
-- Flight Mode Image todo
-- ###############################################################

    if dataC.fmodepic== 3  then 
      lcd.drawPixmap(50, 2, "/SCRIPTS/BMP/H.bmp")  
    elseif dataC.fmodepic==1    then
      lcd.drawPixmap(50, 2, "/SCRIPTS/BMP/stab.bmp")
    elseif dataC.fmodepic==2  then 
      lcd.drawPixmap(50, 2, "/SCRIPTS/BMP/gps.bmp")
    else
      
    end

-- ###############################################################
-- draw GPS Fix
-- ###############################################################
    
    if FC==1 or FC==2 and dataR.Tmp2id==-1 then --if APM help if Telemetry wrong
        drawText(68, 15, "Check Telemetry Tem2", SMLSIZE)
       
    elseif dataC.gpsFix >= 4 then
        drawText(70,15, "3D D.GPS, "..dataC.satCount..' Sats', SMLSIZE)

    elseif dataC.gpsFix == 3 then
        drawText(70,15, "3D FIX, "..dataC.satCount..' Sats', SMLSIZE)
        
    elseif dataC.gpsFix == 2 then
        drawText(70,15, "2D FIX, "..dataC.satCount..' Sats', BLINK+SMLSIZE)
        
    elseif dataC.gpsFix == 1 then
        drawText(70,15, "NO FIX, "..dataC.satCount..' Sats', BLINK+SMLSIZE)
        
    elseif dataC.gpsFix == 0 then
        drawText(70,15, "NO GPS", SMLSIZE) 
        
    elseif dataC.gpsFix == -1 then --no Information about the GPSFIX
        drawText(70,15, dataC.satCount.." GPS Sats", SMLSIZE)
        
    end



    drawText,getLastPos,MIDSIZE,SMLSIZE=nil,nil,nil,nil

-- ###############################################################  
-- ###############################################################
-- ##                -- Main Draw1 End                          ##
-- ###############################################################  
-- ###############################################################





    
    
    
    
    
    
    
    
    
    
    
    
    


   end 

--------------------------------------------------------------------------------
-- BACKGROUND loop FUNCTION
--------------------------------------------------------------------------------
local function backgroundwork() --for Background work and in the main Loop
  GetnewTelemetryValue()
  FlightController()
  
  if dataR.RSSI > 38 then dataC.RSSIperc = round(dataC.RSSIperc*0.5+0.5*(((math.log(dataR.RSSI-28, 10)-1)/(math.log(72, 10)-1))*100))
  else dataC.RSSIperc=0
  end

  BatteryCalcCellVoltageAndTyp() 
  totalConsume() --todo maybe take it from cnsp
  
  if MaxAvarageAmpere > 0     then AlarmifOverAmp()       end
  if BatterymahAlarm > 0      then AlarmifmaxMah()        end--check if alarm function enabled and calk it.
  if CellVoltAlarm>0          then AlarmifVoltLow()       end
  
end

local function background() --only only background work

  ArrayIteam=0--Delete Resistance Array because it can be having old Values in the Background
  ResCalcError=0
  backgroundwork()
  
end


--------------------------------------------------------------------------------
-- RUN loop FUNCTION
--------------------------------------------------------------------------------
local function run(event)

  if firsttime==0 then --Init Screen
    playFile("/SCRIPTS/WAV/welcome.wav") 
    firsttime=1
  elseif firsttime<60 then 
    GetnewTelemetryValue()
    Autodetect()
    lcd.clear()
    lcd.drawPixmap(0, 0, "/SCRIPTS/BMP/LuaPiloL.bmp")
    lcd.drawPixmap(106, 0, "/SCRIPTS/BMP/LuaPiloR.bmp")
    lcd.drawText(0, 57, dataC.FCName, 0)
    firsttime=firsttime+1
    return 0
  end

  if event ==  64  then --if menu key pressed that Reset All Variables.  
    playFile("/SCRIPTS/WAV/reset.wav")
    killEvents(64) 
    ResetVar() 
  end
 
  backgroundwork()
  
  if HeadingOrDist ==1 or HeadingOrDist ==2       then loadGpsData()            end --todo maybe take Dist from opentx
  if CalcBattResistance==1                        then BatteryResistanceCalc()  end --todo draw Resistance if Enabled
  if BattLevelmAh>1                               then BatteryLevelCalcmah()    else BatteryLevelCalcVoltage() end
  if SaybatteryPercent==1                         then SayBattPercent()         end
  if SayFlightMode == 1                           then checkASayFlightMode()    end
  if GPSOKAY~= 0                                  then checkASayGPS()           end
  if HeadingOrDist==0 or HeadingOrDist == 2       then calcHdgOrt()             end
  

  
  CalcDisplayTimer() --for alternating drawing
  lcd.clear()
  
  
  draw()
  
                 
--lcd.drawText(0, 20, round(dataC.CellResi,3) , SMLSIZE) --For debuging print of Battery Resistance
-- lcd.drawNumber(0, 30, collectgarbage("count")*1024, LEFT+INVERS) --For debuging print memory Consume
  
  
  end









--------------------------------------------------------------------------------
-- SCRIPT END
--------------------------------------------------------------------------------
return {run=run,  background=background}

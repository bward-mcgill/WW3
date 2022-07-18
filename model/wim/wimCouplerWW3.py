from datetime import datetime, timedelta
import pandas as pd
import numpy as np
import xarray as xr

def createTimeWW3(year, month, day, sec):
   day=datetime(year, month, day)
   time=day + timedelta(seconds=sec)
   print(str(time.year).zfill(4)+str(time.month).zfill(2)+str(time.day).zfill(2)+'-'+str(time.hour).zfill(2)+str(time.minute).zfill(2)+str(time.second).zfill(2))

#def createStartWW3(y_start, m_start, d_start, s_start):
#   start_day=datetime(y_start, m_start, d_start)
#   start=start_day + timedelta(seconds=s_start)
#   print(str(start.year).zfill(4)+str(start.month).zfill(2)+str(start.day).zfill(2)+'-'+str(start.hour).zfill(2)+str(start.minute).zfill(2)+str(start.second).zfill(2))

#def createEndWW3(y_start, m_start, d_start, s_start, ts):
#   start_day=datetime(y_start, m_start, d_start)
#   end=start_day + timedelta(seconds=s_start)+timedelta(seconds=ts)
#   print(str(end.year).zfill(4)+str(end.month).zfill(2)+str(end.day).zfill(2)+'-'+str(end.hour).zfill(2)+str(end.minute).zfill(2)+str(end.second).zfill(2))

def exchangeVarWW3Cice(ts, pathWW3, pathCICE, date):
   file_ww3=pathWW3+"/ww3."+date+"-"+ts+".nc"
   file_cice=pathCICE+"/restart/iced."+date+"-"+ts+".nc"
   
   ds_ww3=xr.open_dataset(file_ww3)
   ds_cice=xr.open_dataset(file_cice)

   list_var=[]

   if len(list_var)==0:
      print("Nothing hapenned")
      return
   else:
      for var in list_var:
         varww3=ds_ww3[[var]]
         varww3=np.nan_to_num(varww3[var].values)
         if (var == "ic1"):
            id_var="vicen"
         elif (var == "ic5"):
            id_var="fsd001"
         elif (var == "ice"):
            id_var="aicen"
         else:
            print("Unknown WW3 variable")

         ds_cice[id_var]=(['ncat', 'nj', 'ni'], varww3)

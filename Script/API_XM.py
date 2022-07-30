# -*- coding: utf-8 -*-

!pip install pydataxm #Se debe hacer por única vez para instalar las librerías de XM, luego se comenta

from pydataxm import *                           #Se realiza la importación de las librerias necesarias para ejecutar
import datetime as dt                            
from pydataxm.pydataxm import ReadDB as apiXM  

objetoAPI = pydataxm.ReadDB() 

df_sistema2 = objetoAPI.request_data(
                                'ListadoRecursos',
                                'Sistema', 
                                dt.date(2013, 5, 5), 
                                dt.date(2013, 5, 6))

df_sistema2.to_csv (r'E:\MAESTRIA UNIANDES\BIG DATA\Proyecto-final-MEcA-4107\Datos\Listado_agentes.csv', index = False, header=True)
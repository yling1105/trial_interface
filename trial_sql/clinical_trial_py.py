import sys
import json
import pandas as pd
import scrapy
import psycopg2
import psycopg2.extras
import numpy as np
import pandas as pd
from sqlalchemy import create_engine

from selenium import webdriver
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import os
import shutil
import time
from webdriver_manager.chrome import ChromeDriverManager   # eruqired in R


solid_entities = {'Condition':'Condition by Diagnosis Code',
                  'Measurement':'Lab',
                  'Demographic':'Demographic' ,
                  'Drug':'Prescription',
                  'Observation':'Event',
                  'Procedure':'Order'
                 }
                 
engine = create_engine('postgresql+psycopg2://stroke:YI`?vhG7d9@129.106.31.45:15432/covid19')


def getConnection():
    return psycopg2.connect("dbname='covid19' user='stroke' password='YI`?vhG7d9' host='129.106.31.45' port='15432' ")  
    raise NotImplementedError

def load_json_py(j_file):
 
	with open( j_file, "r") as read_file:
	    j_data = json.load(read_file)
		
	j_str = json.dumps(j_data)
	
	return j_str

def get_nct_jstr(nct_id):

    options = webdriver.ChromeOptions()
    options.add_argument('--headless')
    options.add_argument('window-size=2000x2000')
    options.add_argument('--no-sandbox')
    options.add_argument('--disable-dev-shm-usage')

    #driver = webdriver.Chrome(chrome_options=options)               # required in jupyter 
    driver = webdriver.Chrome(ChromeDriverManager().install(),options=options)  # required in R

    # driver.get("http://www.ohdsi.org/web/criteria2query/")
    driver.get("http://172.18.0.3:8080/criteria2query")   # container tomcat_luyao on .45
    
    #driver.save_screenshot("Website.png")

    #Load clinical trial
    nctid = driver.find_element_by_id('nctid')
    fetchct = driver.find_element_by_id('fetchct')
    nctid.send_keys(nct_id)
    ActionChains(driver).click(fetchct).perform()

    #Parse the criteria
    wait = WebDriverWait(driver, 30)
    
    parse = wait.until(EC.element_to_be_clickable((By.ID, 'start')))
    time.sleep(5)
    #driver.save_screenshot("Website1.png")
    ActionChains(driver).click(parse).perform()

    time.sleep(30)
    #driver.save_screenshot("Website2.png")
        
    #Extract JSON text
    download = wait.until(EC.element_to_be_clickable((By.ID, 'downloadfile')))
    ActionChains(driver).click(download).perform()

    #driver.save_screenshot("Website3.png")
    
    time.sleep(30)
    driver.quit()
    
    #Rename the file in the JSON folder
    if os.path.exists('Criteria2Query.json'):
        src_file = os.path.realpath('Criteria2Query.json')
        j_str =  load_json_py(src_file)
    else:
        j_str = ''    
    
    return j_str


	
 
def get_temporal(display_text, content ):
    response = scrapy.Selector(text=display_text)
    contect_found = 0
    found_temporal = ''
    for x in response.xpath('//mark'):

        entity_text = x.xpath('.//text()').get()
        entity_type = x.xpath('@data-entity').get()
        print( entity_type,entity_text )
        if x.xpath('.//text()').get() == content:
            contect_found = 1
        else:
            if contect_found == 1 and entity_type.lower() in ('condition','measurement','observation','drug','demographic','procedure'):
                break
            if contect_found == 1 and entity_type.lower() == 'temporal':
                found_temporal = entity_text
                break
    return found_temporal


def get_value (display_text, content ):
    response = scrapy.Selector(text=display_text)
    contect_found = 0
    found_value = 'test'
    for x in response.xpath('//mark'):

        entity_text = x.xpath('.//text()').get()
        entity_type = x.xpath('@data-entity').get()
        print( entity_type,entity_text )
        if x.xpath('.//text()').get() == content:
            contect_found = 1
        else:
            if contect_found == 1 and entity_type.lower() in ('condition','measurement','observation','drug','demographic','procedure'):
                break
            if contect_found == 1 and entity_type.lower() == 'value':
                found_value = entity_text
                break

    if '<=' in found_value:
        return 'Value from ( not include )', '', 'Value to ( include )', found_value.replace('<=','').strip() 

    if '<' in found_value:
        return 'Value from ( not include )', '', 'Value to ( not include )', found_value.replace('<','').strip() 
    
    if '>=' in found_value:
        return 'Value from ( include )', found_value.replace('>=','').strip() , 'Value to ( not include )', ''

    if '>' in found_value:
        return 'Value from ( not include )', found_value.replace('>','').strip() , 'Value to ( not include )', ''
            
    return 'Value from ( not include )', found_value, 'Value to ( not include )', '' 

 
 
def map_clinical_trial_dump(j_str):

    data =  json.loads(j_str)
    
    trial = pd.DataFrame.from_dict(data)    

    extraction = []
    j_out={}

    criteria_sets = {}



    #trial[0,0] is the exclusion criteria
    #trial[1,0] is the inclusion criteria

    criteria_sets['exclusion'] =  trial.iat[0,0] 
    criteria_sets['inclusion'] =  trial.iat[1,0] 

    for d_key,d_criterias in criteria_sets.items():
        print(d_key)
        m_criteria = []    
        for e in d_criterias:
            #print(e)
            if len(e.get('sents')) > 0:
                print ('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
                for term in range(0,len(e.get('sents'))):
                    phrase = e.get('sents')[term].get('text')
                    display_text = e.get('sents')[term].get('display')
                    print(phrase)
                    print(display_text)
                    q = []
                    mapped_templates=[]
                    for t in e.get('sents')[term].get('terms'):
                        entity = t.get('text')
                        domain = t.get('categorey')
                        q.append([entity, domain])
                        #print(entity, domain)
    #                print(q)

                    for i in range(0,len (q)):
                        print (q[i][1],q[i][0])
                        mapped_template={}
                        template = solid_entities.get(q[i][1])
                        content = q[i][0]
                        if template != None:
                            print('##', template)
                            temporal=''

                            if template in ('Condition by Diagnosis Code', 'Lab', 'Order', 'Event', 'Prescription') :
                                temporal = get_temporal ( display_text, content )
                            value_operation_1=''
                            value_1=''
                            value_operation_2=''
                            value_2=''          

                            if template in (  'Event' , 'Lab' ) :
                                print('******', get_value (display_text, content ))
                                value_operation_1,value_1,value_operation_2,value_2 = get_value (display_text, content )  

                            if template == 'Condition by Diagnosis Code':
                                mapped_template['template'] = template
                                mapped_template['Diagnosis Description contains'] = content
                                mapped_template['Time Period within'] = temporal
                                mapped_template['Search by diagnosis group'] = '0'

                            if template == 'Prescription':
                                mapped_template['template'] = template
                                mapped_template['Drug description contains'] = content
                                mapped_template['Time Period within'] = temporal                            

                            if template == 'Order':
                                mapped_template['template'] = template
                                mapped_template['Procedure Name contains'] = content
                                mapped_template['Time Period within'] = temporal  

                            if template == 'Demographic':
                                mapped_template['template'] = template
                                if content.lower() in ('age'):
                                    age_value_str = ''
                                    for loop in range(0,len (q)):
                                        print('@@@age', loop, q[loop][0], q[loop][1])
                                        if q[loop][1] == 'Value':
                                            age_value_str = q[loop][0]
                                            print('@@@age',age_value_str)
                                            break
                                    mapped_template['Age from ( include )'] = age_value_str

                                if 'male' in content.lower():
                                    mapped_template['Gender is'] = content

                                if any(substring in  content.lower() for substring in ['asian','white','america']):
                                    mapped_template['Race is'] = content

                                if 'hispanic' in content.lower():
                                    mapped_template['Ethnic_Group is'] = content   
                                mapped_template['Encounter based'] = '0'                
                            else:
                                 mapped_template['Encounter based'] = '1'

                            if template == 'Lab':
                                mapped_template['template'] = template
                                mapped_template['Lab Name contains'] = content
                                mapped_template['Time Period within'] = temporal
                                if value_operation_1 == 'Value from ( include )': 
                                    mapped_template['Value from ( include )'] = value_1
                                if value_operation_1 == 'Value from ( not include )':                                 
                                    mapped_template['Value from ( not include )'] = value_1  
                                if value_operation_2 == 'Value to ( include )': 
                                    mapped_template['Value to ( include )'] = value_2
                                if value_operation_2 == 'Value to ( not include )':                                 
                                    mapped_template['Value to ( not include )'] = value_2

                            if template == 'Event':
                                mapped_template['template'] = template
                                mapped_template['Event Name contains'] = content
                                mapped_template['Time Period within'] = temporal
                                if value_operation_1 == 'Value from ( include )': 
                                    mapped_template['Value from ( include )'] = value_1
                                if value_operation_1 == 'Value from ( not include )':                                 
                                    mapped_template['Value from ( not include )'] = value_1   
                                if value_operation_2 == 'Value to ( include )': 
                                    mapped_template['Value to ( include )'] = value_2
                                if value_operation_2 == 'Value to ( not include )':                                 
                                    mapped_template['Value to ( not include )'] = value_2

                            print(mapped_template)
                            if (len (mapped_template) > 0):
                                mapped_templates.append(mapped_template)  
                    if d_key == 'inclusion' :
                        internal_logic = 'AND'
                    else:
                        internal_logic = 'OR'
                    m_criteria.append({'text': phrase,
                                       'data-entities':q,
                                       'display_text':display_text,
                                       'mapped_templates':mapped_templates,
                                       'internal_logic' : internal_logic,
                                       'criteria_patients': 0,
                                       'accumulated_patients': 0})
        j_out[d_key] = m_criteria
    j_out_str = json.dumps(j_out)    
    print(type(j_out_str))
    return j_out_str
 
 
def get_data_from_templdate ( mapped_template ):
    print("$$$$$$$$$$$$",flush=True)
    print(mapped_template,flush=True)
    template = mapped_template.get('template')
    return_SQL = ''
    try: 
        if template == 'Condition by Diagnosis Code':
            if  mapped_template.get('Diagnosis Code is','') == '' \
                and mapped_template.get('Diagnosis Code starts with','') == '' \
                and mapped_template.get('Diagnosis Description contains','') == '':
                return_data =pd.DataFrame()
            else:
                if mapped_template.get('Encounters','') != '':
                    return_SQL = """
                    select * from stroke.get_patients_by_dx_encntrs  (
                        '{}',
                        '{}',
                        '{}',
                        '{}',
                        '{}',
                        '{}',
                        ''
                        )
                        """.format(
                        mapped_template.get('Diagnosis Code is',''),
                        mapped_template.get('Diagnosis Code starts with',''),
                        mapped_template.get('Diagnosis Description contains',''),
                        mapped_template.get('Time Period within',''),
                        mapped_template.get('Search by diagnosis group',''),
                        mapped_template.get('Encounters','')
                        )                    
                else:
                    return_SQL = """
                    select * from stroke.get_patients_by_dx  (
                        '{}',
                        '{}',
                        '{}',
                        '{}',
                        '{}',                        
                        '{}',
                        '{}',
                        ''
                        )
                        """.format(
                        mapped_template.get('Diagnosis Code is',''),
                        mapped_template.get('Diagnosis Code starts with',''),
                        mapped_template.get('Diagnosis Description contains',''),
                        mapped_template.get('Diagnosis Type',''),                        
                        mapped_template.get('Time Period within',''),
                        mapped_template.get('Search by diagnosis group',''),
                        mapped_template.get('Encounter based','')
                        )
                print(return_SQL, flush=True)
#                sql_conn = getConnection()    
#                return_data =pd.read_sql(return_SQL,sql_conn)
#                sql_conn.close()
                return_data = pd.read_sql_query(return_SQL,con=engine)  
                
            print(return_data.shape, flush=True)
 
        if template == 'Demographic':
            if  mapped_template.get('Age from ( include )','') == '' \
                and mapped_template.get('Age to ( include )','')== '' \
                and mapped_template.get('Race is','')== '' \
                and mapped_template.get('Gender is','')== '' \
                and mapped_template.get('Ethnic_Group is','') == '' :
                return_data =pd.DataFrame()
            else:    
                return_SQL = """
                    select person_id,null as encntr_id from stroke.get_patients_by_demo (
                    '{}',
                    '{}',
                    '{}',
                    '{}',
                    '{}',
                    ''
                    )
                    """.format(
                    mapped_template.get('Age from ( include )',''),
                    mapped_template.get('Age to ( include )',''),
                    mapped_template.get('Race is',''),
                    mapped_template.get('Gender is',''),
                    mapped_template.get('Ethnic_Group is','') 
                    )
                #print(return_SQL)
#                sql_conn = getConnection()    
#                return_data =pd.read_sql(return_SQL,sql_conn)
#                sql_conn.close()
                return_data = pd.read_sql_query(return_SQL,con=engine)
            print(return_data.shape)

        if template == 'Prescription':
            if mapped_template.get('Drug Description contains','') == '':
                return_data =pd.DataFrame()
            else:    
                return_SQL = """
                select *  from stroke.get_patients_by_rx (
                '{}',
                '{}',
                '{}',
                ''
                )
                """.format(
                mapped_template.get('Drug Description contains',''),
                mapped_template.get('Time Period within',''),
                mapped_template.get('Encounter based','')
                )
                print(return_SQL)
#                sql_conn = getConnection()    
                return_data = pd.read_sql_query(return_SQL,con=engine)
#                return_data =pd.read_sql(return_SQL,sql_conn)
#                sql_conn.close()

            print(return_data.shape)
 
        if template == 'Event':
            if mapped_template.get('Event Name contains','') == '':
                return_data =pd.DataFrame()
            else: 
                event_names = mapped_template.get('Event Name contains','')
                if isinstance(event_names, list):
                  event_names = '|'.join(event_names)              
                return_SQL = """
                 select *  from stroke.get_patients_by_events (
                '{}',
                '{}',
                '{}',
                '{}',
                '{}',
                '{}',
                '{}',
                ''
                )
                """.format(
                event_names,
                mapped_template.get('Value from ( include )',''),
                mapped_template.get('Value from ( not include )',''),
                mapped_template.get('Value to ( include )',''),
                mapped_template.get('Value to ( not include )',''),
                mapped_template.get('Time Period within',''),              
                mapped_template.get('Encounter based','')
                )
                print(return_SQL,flush=True)
#                sql_conn = getConnection()    
                return_data = pd.read_sql_query(return_SQL,con=engine)
#                return_data =pd.read_sql(return_SQL,sql_conn)
#                sql_conn.close()
            print(return_data.shape, flush=True)
            
        if template == 'Lab':
            if mapped_template.get('Lab Name contains','') == '':
                return_data =pd.DataFrame()
            else: 
                lab_names = mapped_template.get('Lab Name contains','')
                if isinstance(lab_names, list):
                  lab_names = '|'.join(lab_names)
                return_SQL = """
                 select *  from stroke.get_patients_by_lab (
                '{}',
                '{}',
                '{}',
                '{}',
                '{}',
                '{}',
                '{}',
                '{}',
                ''
                )
                """.format(
                lab_names,
                mapped_template.get('LOINC is',''),                
                mapped_template.get('Value from ( include )',''),
                mapped_template.get('Value from ( not include )',''),
                mapped_template.get('Value to ( include )',''),
                mapped_template.get('Value to ( not include )',''),
                mapped_template.get('Time Period within',''),              
                mapped_template.get('Encounter based','')
                )
                # print(return_SQL)
#                sql_conn = getConnection()    
#                return_data =pd.read_sql(return_SQL,sql_conn)
#                sql_conn.close()
                return_data = pd.read_sql_query(return_SQL,con=engine)
                
            print(return_data.shape)
            
        if template == 'Order':
            if mapped_template.get('Procedure Name contains','') == '':
                return_data =pd.DataFrame()
            else:             
                return_SQL = """
                select *  from stroke.get_patients_by_order (
                '{}',
                '{}',
                '{}',
                ''
                )
                """.format(
                mapped_template.get('Procedure Name contains',''),
                mapped_template.get('Time Period within',''),
                mapped_template.get('Encounter based','')
                )
                # print(return_SQL)
#                sql_conn = getConnection()    
#                return_data =pd.read_sql(return_SQL,sql_conn)
#                sql_conn.close()
                return_data = pd.read_sql_query(return_SQL,con=engine)

            print(return_data.shape)

        if template == 'Encounter':
            if mapped_template.get('Encounter Type','') == '' \
            and mapped_template.get('Discharge Disposition','') \
            and mapped_template.get('Admit Type','') :
                return_data =pd.DataFrame()
            else:             
                return_SQL = """
                select *  from stroke.get_patients_by_encounters (
                '{}',
                '{}',
                '{}',
                ''
                )""".format(
                mapped_template.get('Encounter Type',''),
                mapped_template.get('Admit Type',''),
                mapped_template.get('Discharge Disposition','')
                )
                print(return_SQL)
#                sql_conn = getConnection()    
#                return_data =pd.read_sql(return_SQL,sql_conn)
#                sql_conn.close()
                return_data = pd.read_sql_query(return_SQL,con=engine)

            print(return_data.shape)        
            
    except Exception as e:
        print("SQL Execution Error")
        print(e)
        return_SQL = """
        select person_id,encntr_id   from stroke.stroke_events limit 0
        """    
#        return_data =pd.read_sql(return_SQL,sql_conn)
        return_data = pd.read_sql_query(return_SQL,con=engine)

#        sql_conn.close()
        print(return_data.shape)
        
    return return_data

def merge_df_or(df1,df2):
    if df1['encntr_id'].isnull().sum()!=len(df1) and df2['encntr_id'].isnull().sum()!=len(df2):  # df1 & df2 is  encounter based 
        merged = pd.concat([df1,df2]).drop_duplicates().reset_index(drop=True)
    else:
        merged = pd.concat([df1[['person_id']],df2[['person_id']]]).drop_duplicates().reset_index(drop=True)
        merged['encntr_id']= None
    print('or merged:',merged.shape)    
    return merged

def merge_df_and(df1,df2):
    if len(df1) == 0 :
        return (df1)
    if len(df2) == 0 :
        return (df2)
    if df1['encntr_id'].isnull().sum()!=len(df1):  # df1 is  encounter based 
        if df2['encntr_id'].isnull().sum()==len(df2):  # df2 is not encounter based 
            merged = pd.merge(df1, df2, how='inner', on=['person_id']).drop_duplicates().reset_index(drop=True).iloc[:,:2 ]
            merged.columns = df1.columns
        else:
            merged = pd.merge(df1, df2, how='inner', on=['person_id','encntr_id']).drop_duplicates().reset_index(drop=True)
    else: # df1 is not encounter based 
        if df2['encntr_id'].isnull().sum()!=len(df2):  # df2 is encounter based 
            merged = pd.merge(df2, df1, how='inner', on=['person_id']).drop_duplicates().reset_index(drop=True).iloc[:,:2 ]
            merged.columns = df1.columns
        else:
            merged = pd.merge(df1, df2, how='inner', on=['person_id','encntr_id']).drop_duplicates().reset_index(drop=True)        
    print('and merged:',merged.shape) 
    return merged

def merge_df_remove(df1,df2):
    if len(df1) == 0 :
        return (df1)
    if len(df2) == 0 :
        return (df1)
    if df1['encntr_id'].isnull().sum()!=len(df1):  # df1 is  encounter based 
        if df2['encntr_id'].isnull().sum()==len(df2):  # df2 is not encounter based 
            merged = pd.merge(df1, df2, how='outer', on=['person_id'], indicator=True)
            merged = merged[merged._merge == 'left_only'].iloc[:,:2] 
            merged.columns = df1.columns
        else:
            merged = pd.merge(df1, df2, how='outer', on=['person_id','encntr_id'], indicator=True)
            merged = merged[merged._merge == 'left_only'].iloc[:,:2]         
    else: # df1 is not encounter based 
        merged = pd.merge(df1, df2, how='outer', on=['person_id'], indicator=True)
        merged = merged.loc[merged._merge == 'left_only', ['person_id']].drop_duplicates().reset_index(drop=True)
        merged['encntr_id']= None        
    print('remove merged:',merged.shape) 
    return merged

def get_pat_data(person_ids_str):
#    sql_conn = getConnection()

    sql_text = """ 
        select person_id,date_part('year',AGE(current_date,dob)) as age,gender,race,zipcode,ethnic_grp   
        from  stroke.stroke_patients  where person_id in ({})
          """.format(person_ids_str)  
    final_pat = pd.read_sql_query(sql_text,con=engine)         
          
#    final_pat = pd.read_sql(sql_text,sql_conn)
#    sql_conn.commit()
#    sql_conn.close()
    print(final_pat.shape)
    return  final_pat.to_dict('index')

def merge_template_by_timing(templdate1,templdate2,sequence,interval):
    #if  sequence =='Template 2 WITHIN Template 1': 
    interval1 = pd.to_timedelta(int(interval.split('|')[0].strip().split(' ')[0]), unit=interval.split('|')[0].strip().split(' ')[1])
    interval2 = pd.to_timedelta(int(interval.split('|')[1].strip().split(' ')[0]), unit=interval.split('|')[1].strip().split(' ')[1])
    #else:
    #    interval2 = pd.to_timedelta(int(interval.split('|')[0].strip().split(' ')[0]), unit=interval.split('|')[0].strip().split(' ')[1])
    #    interval1 = pd.to_timedelta(int(interval.split('|')[1].strip().split(' ')[0]), unit=interval.split('|')[1].strip().split(' ')[1])   
    
    print(sequence)
    print(interval1,interval2)
        
    if  sequence =='Template 1 WITHIN Template 2': 
        templdate_temp = templdate1
        templdate1 = templdate2
        templdate2 = templdate_temp
    print(templdate1.head(1))
    print(templdate2.head(1))
    
    if templdate1['encntr_id'].isna().all() or templdate2['encntr_id'].isna().all() :
        merged = templdate1.merge(templdate2, how='inner', on= ['person_id'], suffixes=('_1', '_2'))  
        if templdate1['encntr_id'].isna().all():
            merged['encntr_id'] = merged['encntr_id_2'] 
        if templdate2['encntr_id'].isna().all():
            merged['encntr_id'] = merged['encntr_id_1']             
    else:    
        merged = templdate1.merge(templdate2, how='inner', on= ['person_id','encntr_id'], suffixes=('_1', '_2'))  
    merged = merged[(merged.event_dt_tm_2 <= merged.event_dt_tm_1 +  interval2) & 
                    (merged.event_dt_tm_2 > merged.event_dt_tm_1 - interval1) ]
    merged = merged[['person_id','encntr_id']].drop_duplicates().reset_index(drop=True) 
    print(merged.shape)
    return merged
    
 

def refresh_patient_numbers(j_str):
 
    j_data = json.loads(j_str)

    final_df = pd.DataFrame()
    
    xc = 0    # first Criteria
    for x in j_data['inclusion']:
        yc = 0 # this is the first template in a Criteria 
        y_df = pd.DataFrame()
        for y in x.get('mapped_templates'):
            if 'WITHIN' in x.get('internal_logic'):
                data_enctr = y.get('Encounter based','')
                y['Encounter based'] = '1'  
            print(y)
            if yc == 0:  # this is the first template in a Criteria 
                y_df = get_data_from_templdate (y)
                if len(y_df.columns) > 0 :
                    if x.get('internal_logic') in ['AND','OR']:
                        y_df = y_df[['person_id','encntr_id']].drop_duplicates()
                    else:
                        if data_enctr != '1':
                            y_df['encntr_id'] = None
            else:
                next_templdate_data = get_data_from_templdate (y)
                if x.get('internal_logic') in ['AND','OR']:
                    next_templdate_data = next_templdate_data[['person_id','encntr_id']].drop_duplicates()  
                  
                if len(next_templdate_data.columns) > 0: # valid query , otherwise skip the merge
                    if x.get('internal_logic') == 'AND':
                        y_df =  merge_df_and(y_df,next_templdate_data)
                    if x.get('internal_logic') == 'OR':
                        y_df =  merge_df_or(y_df,next_templdate_data)
                    if 'WITHIN' in x.get('internal_logic'):
                        if data_enctr != '1':
                            next_templdate_data['encntr_id'] = None                      
                        y_df = merge_template_by_timing(y_df,next_templdate_data,x.get('internal_logic'), x.get('interval'))                           
            yc += 1
        if len(final_df.columns) == 0:    # first Criteria or no valid criteria so far 
            final_df = y_df
        else:
            if len(y_df.columns) > 0 :
                final_df = merge_df_and(final_df,y_df)
                
        if len(final_df.columns) > 0:        
            accumulated_patients = len(final_df.person_id.value_counts())
        else: 
            accumulated_patients = 'NA'
        if len(y_df.columns) > 0:          
            criteria_patients = len(y_df.person_id.value_counts())
        else: 
            criteria_patients = 'NA'
            
        j_data['inclusion'][xc]['accumulated_patients'] = accumulated_patients
        j_data['inclusion'][xc]['criteria_patients'] = criteria_patients
        print ('&&&&&&&&& inclusion..{} level..{} accumulated..{}'.format(xc,criteria_patients,accumulated_patients ))
        xc += 1
    
    xc = 0    # first Criteria
    for x in j_data['exclusion']:
        yc = 0 # this is the first template in a Criteria 
        y_df = pd.DataFrame()
        for y in x.get('mapped_templates'):
            if 'WITHIN' in x.get('internal_logic'):
                data_enctr = y.get('Encounter based','')
                y['Encounter based'] = '1'
            print(y)
            if yc == 0:  # this is the first template in a Criteria 
                y_df = get_data_from_templdate (y)
                if len(y_df.columns) > 0 :                
                    if x.get('internal_logic') in ['AND','OR']:
                        y_df = y_df[['person_id','encntr_id']].drop_duplicates()  
                    else:
                        if data_enctr != '1':
                            y_df['encntr_id'] = None                        
            else:
                next_templdate_data = get_data_from_templdate (y)
                if x.get('internal_logic') in ['AND','OR']:
                    next_templdate_data = next_templdate_data[['person_id','encntr_id']].drop_duplicates()                
                if len(next_templdate_data.columns) > 0:  # valid query , otherwise skip the merge
                    if x.get('internal_logic') == 'AND':
                        y_df =  merge_df_and(y_df,next_templdate_data)
                    if x.get('internal_logic') == 'OR':
                        y_df =  merge_df_or(y_df,next_templdate_data)
                    if 'WITHIN' in x.get('internal_logic'):
                        if data_enctr != '1':
                            next_templdate_data['encntr_id'] = None                          
                        y_df = merge_template_by_timing(y_df,next_templdate_data,x.get('internal_logic'), x.get('interval'))               
            yc += 1
    
        if len(y_df.columns) > 0 :
            final_df = merge_df_remove(final_df,y_df)
                
        accumulated_patients = len(final_df.person_id.value_counts())
        if len(y_df.columns) > 0:          
            criteria_patients = len(y_df.person_id.value_counts())
        else: 
            criteria_patients = 'NA'
            
        j_data['exclusion'][xc]['accumulated_patients'] = accumulated_patients
        j_data['exclusion'][xc]['criteria_patients'] = criteria_patients
        print ('&&&&&&&&& exclusion..{} level..{} accumulated..{}'.format(xc,criteria_patients,accumulated_patients ))
        xc += 1
        
    if accumulated_patients > 0 :
        person_ids_str = ','.join([str(x) for x in final_df.person_id.unique()])
        j_data['result_set'] = get_pat_data(person_ids_str)
        
    return json.dumps(j_data) 

  

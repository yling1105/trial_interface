import streamlit as st
import json
import os
import numpy as np
import pandas as pd

def main():
	# Read json files into the memory
	path = 'trial_json/'
	files_lst = os.listdir('trial_json')
	if ('.DS_Store' in files_lst):
		files_lst.remove('.DS_Store')
	#print(files_lst)
	name_lst = [x.split('.')[0] for x in files_lst] # Used for Choice
	json_lst = [] # Used to store all the json files

	for file in files_lst:
		with open(path+file,'rb') as js_file:
			js = json.load(js_file)
			json_lst.append(json_lst)
	
	st.title('Trial Eligibility Form')

	st.sidebar.title('Available trials')
	trial = st.sidebar.multiselect('Select the target trial(s):', np.array(name_lst))
	intialize = st.sidebar.





if __name__ == "__main__":
    main()






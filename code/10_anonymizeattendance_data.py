
## Imports
import pandas as pd
import numpy as np
import os
from pathlib import Path
import glob
import yaml

import hashlib
from unidecode import unidecode

## load credentials to use for hash
with open("../creds.yaml", 'r') as stream:
    creds_loaded = yaml.safe_load(stream)
key = creds_loaded['hash_creds'].encode('utf-8')

## Functions
### just creates hash for one id
def hash_onerow(row):
    
    """Function that takes in a single ID column 
    and hashes it using hashlib's sha256 method
    ID should be string so that it can be encoded to utf8""" 
    
    sha = hashlib.sha256(key)
    row_encode = row.encode('utf8')
    sha.update(row_encode)
    return sha.hexdigest() 

## Constants

### Path names
BASE_DIR = Path('..')
GEN_DATA_DIR = BASE_DIR / "data/analysis_data/"
ATTEND_DATA_DIR = GEN_DATA_DIR / "tt_rawattendance/"
INTERMEDIATE_DIR = BASE_DIR / "intermediate_outputs"

### Input files
rastatus_dem_fname = GEN_DATA_DIR / "analyticsamp_wosse.csv"
attendance_fnames = [file for file in os.listdir(ATTEND_DATA_DIR) 
                   if "Fall Term Attendance" in file]



### dev or run; put testing if running in notebook; real if running in full sample
code_version = "real_first" 
nrows_read = 100 # num rows of attend file to read; can change to lower to run faster



# # 1. Read in input files
# 
# - Main left file: rastatus_dem (randomization status; osse dems for those we could match; eventually for period-level analyses need to add focal teacher)
# 
# - Right files: attendance files; need to rowbind


## read in RA status file 
rastatus_dem = pd.read_csv(rastatus_dem_fname)
rastatus_dem.school_merge.value_counts()


ana_path = [ATTEND_DATA_DIR / file for file in attendance_fnames if "Anacostia" in file]
chec_path = [ATTEND_DATA_DIR / file for file in attendance_fnames if "CHEC" in file]
dunbar_path = [ATTEND_DATA_DIR / file for file in attendance_fnames if "Dunbar" in file]
print("constructed paths")


if code_version == "testing":
    ana_attend_raw = pd.read_excel(ana_path[0], nrows = nrows_read)
    ana_attend_raw['school_merge'] = "Anacostia"
    chec_attend_raw = pd.read_excel(chec_path[0], nrows = nrows_read)
    chec_attend_raw['school_merge'] = 'CHEC'
    dunbar_attend_raw = pd.read_excel(dunbar_path[0], nrows = nrows_read)
    dunbar_attend_raw['school_merge'] = 'Dunbar'
    all_attend_raw = pd.concat([ana_attend_raw, chec_attend_raw, dunbar_attend_raw])
    print("read in data and saved pickle")
    all_attend_raw.to_pickle(INTERMEDIATE_DIR / "all_attend_raw_test.pkl")
elif code_version == "real_first":
    ana_attend_raw = pd.read_excel(ana_path[0])
    ana_attend_raw['school_merge'] = "Anacostia"
    chec_attend_raw = pd.read_excel(chec_path[0])
    chec_attend_raw['school_merge'] = 'CHEC'
    dunbar_attend_raw = pd.read_excel(dunbar_path[0])
    dunbar_attend_raw['school_merge'] = 'Dunbar'
    all_attend_raw = pd.concat([ana_attend_raw, chec_attend_raw, dunbar_attend_raw])
    print("read in data and saved pickle")
    all_attend_raw.to_pickle(INTERMEDIATE_DIR / "all_attend_raw.pkl")
else:
    all_attend_raw = pd.read_pickle(INTERMEDIATE_DIR / "all_attend_raw.pkl")


# # 2. Remove unneeded columns and merge before hash
# 

# ## 2.1 Removing identifiers we do not need


## even though need demographics for the analysis,
## for now, not including since not clear how to deidentify
## can add lists together
## if we include at later point
dem_notyet = ['race',
            'gender',
            'ell', 
            'farms',
            'at-risk',
            'highest_swd_level',
            'ward',
            'dem_source']

cols_keepRA = ['StudentID', 'tx_summary',
              'control_summary', 'any_tx',
              'any_control', 'student_grade',
              'final_status',
              'school_merge',
              'usi', 
              'lea_student_id',
              'enrollment_date',
              'withdrawal_date',
              'ra_date',
              'appears_multiple',
              'missing_osse',
              'atleast_one_msgsent',
              'atleast_one_msgreceived',
              'atleast_two_msgreceived']



cols_removeattend = ['LastName',
                    'FirstName']
cols_keepattend = [col for col in all_attend_raw.columns
                  if col not in cols_removeattend]


rastatus_keep = rastatus_dem[cols_keepRA]
attend_keep = all_attend_raw[cols_keepattend]


# ## 2.2 merging


## left join attendance data and add indicator
## doing merge just on studentid to allow
## attendance records for if they switch schools
rastatus_wattend = pd.merge(rastatus_keep.rename(columns = {'school_merge': 'school_initrosters'}),
                           attend_keep.rename(columns = {'school_merge': 'school_attendancedf'}),
                           on = 'StudentID',
                           how = "left",
                           indicator = "attendance_merge_status")

## for better hashing, creating string version of studentID
rastatus_wattend['StudentID_str'] = rastatus_wattend.StudentID.astype(str)


# # 3. Add hashed id
# 
# Just hash studentID since that's unique; same hash should follow them if they attend different school (difficult to check in test sample with low n; can check in larger sample)
# 


rastatus_wattend.insert(0, "hashed_id",
                        value = rastatus_wattend.apply(lambda row: hash_onerow(row['StudentID_str']), 
                        axis = 1))


print("finished hashing ids")



assert len(rastatus_wattend.StudentID_str.unique()) == len(rastatus_wattend.hashed_id.unique())
print("passed first check; do number of hashed ids equal number of original ids?")


### for students with multiple schools, id is still only one hash
stud_nschools = pd.DataFrame(rastatus_wattend.groupby('StudentID_str')['school_attendancedf'].nunique()).reset_index()
stud_mult = stud_nschools.StudentID_str[stud_nschools.school_attendancedf > 1].copy() # empty in sample



if len(stud_mult) > 0:
    stud_multschools_df = rastatus_wattend[rastatus_wattend.StudentID_str.isin(stud_mult)].copy()
    stud_nhash = pd.DataFrame(stud_multschools_df.groupby('StudentID_str')['hashed_id'].nunique()).reset_index()
    assert stud_nhash.hashed_id.eq(1).all()
    print("passed second check; for students with multiple schools, does hash follow them?")


## then, drop student id cols and write to pkl
final_keep = [col for col in rastatus_wattend.columns if "StudentID" not in col]


#rastatus_wattend[final_keep].to_hdf(INTERMEDIATE_DIR / "anonymizedattend_3dcps.hdf", 'anonymizedattend_3dcps', mode  = 'w')
rastatus_wattend[final_keep].to_csv(INTERMEDIATE_DIR / "anonymizedattend_3dcps.csv", index = False)
print("finished writing anonymous files")

## write crosswalk
rastatus_crosswalk = rastatus_wattend[['hashed_id', 'StudentID_str']].drop_duplicates()
rastatus_crosswalk.to_pickle(INTERMEDIATE_DIR/ "dcps_hash_studID_cw.pkl")
print("finished writing cw")

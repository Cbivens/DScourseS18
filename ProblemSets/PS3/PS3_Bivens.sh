#!/bin/sh

wget https://spatialkeydocs.s3.amazonaws.com/FL_insurance_sample.csv.zip
# downloading florida data

ls
# looking at data

unzip FL_insurance_sample.csv.zip
# unzipping file

rm -rf __MACOSX rm -f FL_insurance_sample.csv.zip
# deleting the folder it auto made and the zipped version of the data

ls -al -block-size=MB FL_insurance_sample.csv
# looking at file size

head -5 FL_insurance_sample.csv
# looking at first 5 lines

wc -l FL_insurance_sample.csv
# looking at total line count

dos2unix -c mac FL_insaurance_sample.csv
# head and wc commands didnt work, converting file to linux

head -5 FL_insurance_sample.csv
# trying head again after conversion

wc -l FL_insurance_sample.csv
# trying line count again after conversion

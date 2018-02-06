#!/bin/sh

sqlite3

-- SQL script to import florida insurance data for DScourseS18

.print 'Importing data'

-- creating table to store insurance csv file
CREATE TABLE "FloridaInsurance" (
constructpolicyID INTEGER,
statecode INTEGER,
county TEXT,
eq_site_limit INTEGER,
hu_site_limit INTEGER,
fl_site_limit INTEGER,
fr_site_limit INTEGER,
tiv_2011 INTEGER,
tiv_2012 INTEGER,
eq_site_deductible INTEGER,
hu_site_deductible INTEGER,
fl_site_deductible INTEGER,
fr_site_deductible INTEGER,
point_latitude INTEGER,
point_longitude INTEGER,
line INTEGER,
construction TEXT,
point_granularity INTEGER
);

-- telling sql that it will be a csv file
.mode csv

-- importing csv file
.import	FL_insurance_sample.csv FloridaInsurance

-- dropping the headr row
DELETE FROM FloridaInsurance WHERE constructpolicyID = 'constructpolicyID';

-- View first 10 observations
.print 'Viewing first 10 observations'
SELECT * FROM FloridaInsurance LIMIT 10;

-- Listing the counties in the florida insurance csv 
.print 'Unique values'
-- # of unique counties in the data
SELECT DISTINCT county FROM FloridaInsurance;

-- Finding average property appreciation from 2011 to 2012
.print 'Average property appreciation'
-- making new column for appreciation difference, then finding the mean
SELECT AVG(tiv_2011) FROM FloridaInsurance;
SELECT AVG(tiv_2012) FROM FloridaInsurance;
SELECT AVG(tiv_2012 - tiv_2011) FROM FloridaInsurance;

-- Making a frequency table of the construction variable to see 
--    what fraction of buildings are made of wood, or whatever
.print 'Frequency Table'
SELECT construction, COUNT(*) FROM FloridaInsurance GROUP BY construction
 
-- to save as a text file
.output FloridaInsurance.sqlite3
.dump



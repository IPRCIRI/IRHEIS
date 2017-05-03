#!/bin/bash
#
#    m-extract-HEIS : extracting "Iranian Household Expenditures and Income Surveys" 
#    data tables from "Statistics Centre of Iran" MDB (Access) files, and 
#    tidying them up.
#
#    depends: mdbtools >= 0.5.99 (not tested against earlier versions)
#
#    Copyright (c) 2010-2015 Majid Einian <einian85@gmail.com>
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
#    See the GNU General Public License for more details.
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

wd=`pwd` # This is current working directory

starttime=`date` # Just to show how much time it took to run the program

# Check if mdb-tools is installed.
type -P mdb-tables &>/dev/null || { echo "m-extract-HEIS requires mdb-tools but it's not installed.  Aborting. ";echo "Try: \"sudo apt-get install mdb-tools\" in Debian/Ubuntu or appropriate program in your distro. " >&2; exit 1; }

if [ -z "$1" ]; then echo Error: usage: $0 year.mdb; exit 1; fi # Check the user input.
if [ ! -w . ]; then echo Error: Current directory access denied.; exit 2; fi # Check the current directory access.

echo "m-extract-HEIS : extracting \"Iranian Household Expenditures and Income Surveys\" "
echo "data tables from \"Statistics Centre of Iran\" MDB (Access) files, and "
echo "tidying them up."
echo
echo "depends: mdbtools >= 0.5.99 (not tested againts earlier versions)"
echo
echo "Copyright (C) 2010-2015 Majid Einian <einian85@gmail.com>"
echo
echo


# For each of files in list provided by user.
while [ ! -z "$1" ]
do
  cd $wd
	if [ ! -r $1 ]; then pwd; echo Error: File $1 does not exist.; shift; continue; fi # Check if file exists.

	year=${1%.mdb} # Drop the .mdb extension (if exists)
# echo m-debug 01 $year
	year=${year%.MDB} # Drop the .MDB extension (if exists)
# echo m-debug 02 $year

	echo Processing MDB Jet Database year $year.
	# Building directory tree system.
	if [ ! -r $year/ ] ; then mkdir $year; fi
	if [ ! -r $year/U ] ; then mkdir $year/U; fi
	if [ ! -r $year/R ] ; then mkdir $year/R; fi

	tableslist=`mdb-tables $1` # Get the list of tables in MDB file.
#echo m-debug 03 $tableslist

	echo Exporting Tables: ...
	for table in $tableslist; do
		printf "%16s" $table
		`mdb-export -Q $1 $table > $year/$table.csv`
	done
	echo

	cd $year
	echo Processing Tables \(Cleaning Up, ...\) : ...

	urbanlist=`ls U*.csv`
	echo Urban Tables
	for tablefile in $urbanlist; do
		tablename=`echo ${tablefile#U$year} | tr  '[a-z]' '[A-Z]'`
		if [ "$tablename" == ".CSV" ]; then echo OK; tablename="MONTH.CSV"; fi
		printf "%20s" $tablename
		`cat $tablefile | sed -e 's/@//' | sed -e 's/\r//' | sed -e 's/ي/ی/' | sed -e 's/ك/ک/' > U/$tablename`
		`cat $tablefile | sed -e 's/@//' | sed -e 's/\r//' | sed -e 's/ي/ی/' | sed -e 's/ك/ک/' > U/$tablename`
	done
	echo

	rurallist=`ls R*.csv`
	echo Rural Tables
	for tablefile in $rurallist; do
		tablename=`echo ${tablefile#R$year} | tr  '[a-z]' '[A-Z]'`
		if [ "$tablename" == ".CSV" ]; then echo OK; tablename="MONTH.CSV"; fi
		printf "%20s" $tablename
		`cat $tablefile | sed -e 's/@//' | sed -e 's/\r//' | sed -e 's/ي/ی/' | sed -e 's/ك/ک/' > R/$tablename`
	done
	echo
	rm U*.csv R*.csv
	shift
done
echo Program started at $starttime
echo Program ended.. at `date`
#!/bin/bash

test_dir="./test/"
count=1
for testfile in "$test_dir"*.tony
do
  res=$(awk -v line=3 -v field=3 'NR==line{print $field}' "$testfile")
  #echo $res
  output=$(./tony < $testfile 2>/dev/null)
  if [ "$res" = "Pass" ]; then
    if [ "$output" = "Success." ]; then
      echo "Test case ${count}: ${testfile} - Success"
    else
      echo "Test case ${count}: ${testfile} - Error"
    fi
  elif [ "$res" = "Fail" ]; then
    if [ "$output" = "Success." ]; then
      echo "Test case ${count}: ${testfile} - Error"
    else
      echo "Test case ${count}: ${testfile} - Success"
    fi
  else
    echo "Test case ${count}: ${testfile} - Unknown"
  fi
  let count+=1
done

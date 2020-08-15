#!/bin/bash

echo "Running test cases for semantic analysis..."
echo -e "----------------------------------------------\n"
test_dir="./test/"
count=1
correct=0
for testfile in "$test_dir"*.tony
do
  res=$(awk -v line=3 -v field=3 'NR==line{print $field}' "$testfile")
  name=$(awk -v line=2 -v field=4 'NR==line{print $field}' "$testfile")
  output=$(./tony < $testfile 2>/dev/null)
  outputerr=$(./tony < $testfile 2>&1 > /dev/null)
  #echo "$outputerr"

  if [ $res = "Pass" ]; then
    if [[ -z $outputerr ]]; then
      let correct+=1
      echo "Test case ${count}: ${name} - Success"
    else
      echo "Test case ${count}: ${name} - Error"
    fi
  elif [ $res = "Fail" ]; then
    if [[ -z $outputerr ]]; then
      echo "Test case ${count}: ${name} - Error"
    else
      echo "Test case ${count}: ${name} - Success"
      let correct+=1
    fi
  else
    echo "Test case ${count}: ${name} - Unknown | *Please follow the file format"
  fi
  let count+=1
done

let count-=1
echo -e "----------------------------------------------\n"
echo "Correct: ${correct}/${count}"

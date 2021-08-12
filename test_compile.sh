#!/bin/bash

echo "Running test cases for codegen analysis..."
echo -e "----------------------------------------------\n"
test_dir="./test/"
count=1
correct=0
correct_run=0
wrong=()
wrong_run=()

for testfile in "$test_dir"*.tony
do
  # get expected result and name
  res=$(awk -v line=3 -v field=3 'NR==line{print $field}' "$testfile")
  name=$(awk -v line=2 -v field=4 'NR==line{print $field}' "$testfile")
  expectsInput=$(awk -v line=4 -v field=2 'NR==line{print $field}' "$testfile")

  if [[ -n $expectsInput ]]; then
    continue
  fi

  if [ $res = "Pass" ]; then
    output=$(./tonyc $testfile 2>/dev/null)
    outputerr=$(./tonyc $testfile 2>&1 > /dev/null)
    if [[ -z $outputerr ]]; then
      # program has beeen compiled succesfully
      let correct+=1
      # try to run it
      name_noext=${name%.tony}
      output_run=$(./test/${name_noext}.out 2>/dev/null)
      outputerr_run=$(./test/${name_noext}.out 2>&1 > /dev/null)
      if [[ -n $outputerr_run ]]; then
        # if running it produces error then append to wrong2
        echo $name
        wrong_run+=(${name_noext})
      else
        let correct_run+=1
      fi
      echo "Test case ${count}: ${name} - Success"
    else
      wrong+=(${name})
      echo "Test case ${count}: ${name} - Error"
    fi
    let count+=1
    echo "*********************************"
  fi
done

let count-=1
echo -e "----------------------------------------------\n"
echo "Correct: ${correct}/${count}"
echo -e "----------------------------------------------\n"
echo "Wrong:"
for value in "${wrong[@]}"
do
   echo "$value"
done
echo -e "----------------------------------------------\n"
echo "Runtime errors: ${runtime_erros/${count}}"
for value in "${wrong_run[@]}"
do
   echo "$value"
done

#!/bin/bash

main_folder="~/Documents/my-projects/sneaky-cmup-10/cmup-c/test-folder/"

if [ -d "$main_folder" ]; then
  echo "indeed directory"
fi

for folder in "$main_folder/*"; do
  echo "$folder"
    if [ -d "$folder" ]; then
      touch "$folder/new_file.txt"
      echo "i'm working btw"
    fi
done

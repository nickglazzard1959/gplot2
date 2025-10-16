#!/bin/bash

# Define the directory to loop through
directory="."

# Loop through each item in the specified directory
for file in "$directory"/*.svg; do
  # Check if the current item is a regular file (not a directory)
  if [ -f "$file" ]; then
    echo "Processing file: $file"
    # Perform operations on the file here
    svgview "$file"
  fi
done

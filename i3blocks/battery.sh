#!/usr/bin/env bash
set -euo pipefail

out=$(acpi)
is_full=$(echo $out | grep -o '[^,]*$')

if [[ $is_full == " 100%" ]]
then
  echo "100%"
else
  LEVEL=$(echo $out | grep -o '[0-9][0-9]')

  echo $out | grep -o '[0-9][0-9]%'
fi

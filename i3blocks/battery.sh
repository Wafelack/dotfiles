#!/usr/bin/env bash
set -euo pipefail

out=$(acpi)
is_full=$(echo $out | cut -d, -f2)

echo $is_full

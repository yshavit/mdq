#!/bin/bash

mdq_name=mdq
suffix=1
while [[ -e "$mdq_name" ]]; do
  mdq_name="mdq-$suffix"
  suffix=$(( $suffix + 1 ))
done

cat "$0" | awk 'f;/^## BASE64 START/{f=1}' | sed 's/^# *//' | base64 -d > "$mdq_name"
chmod +x "$mdq_name"

## BASE64 START

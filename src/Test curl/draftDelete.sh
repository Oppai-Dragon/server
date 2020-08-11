#!/bin/bash
essence="draft"
action="delete"
id=$(jq ."$essence"1.id $essence.json)
access_key=$(jq .person1.access_key person.json)
generate_post_data()
{
cat <<EOF
{ "id": "$id"
, "access_key": $access_key
}
EOF
}
curl \
-H "Content-Type: application/json" \
-X POST \
--data "$(generate_post_data)" \
"http://localhost:8000/$essence/$action" > $action.txt
cat $action.txt
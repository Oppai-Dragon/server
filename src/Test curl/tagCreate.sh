#!/bin/bash
name="testTag"
access_key=$(jq .person1.access_key person.json)
generate_post_data()
{
cat <<EOF
{ "name": "$name"
, "access_key": "$access_key"
}
EOF
}
essence="tag"
action="create"
curl \
-H "Content-Type: application/json" \
-X POST \
--data "$(generate_post_data)" \
"http://localhost:8000/$essence/$action" | jq '.' > $essence.json
cat $essence.json
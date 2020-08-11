#!/bin/bash
essence="author"
action="get"
access_key=$(jq .person1.access_key person.json)
generate_post_data()
{
cat <<EOF
{ "access_key": "$access_key"
}
EOF
curl \
-H "Content-Type: application/json" \
-X POST \
--data "$(generate_post_data)" \
"http://localhost:8000/$essence/$action" | jq '.'
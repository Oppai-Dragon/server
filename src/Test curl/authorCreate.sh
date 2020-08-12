#!/bin/bash
person_id=$(jq .person1.id person.json)
access_key=$(jq .person1.access_key person.json)
generate_post_data()
{
cat <<EOF
{ "person_id": $person_id
, "access_key": $access_key
}
EOF
}
essence="author"
action="create"
curl \
-H "Content-Type: application/json" \
-X POST \
--data "$(generate_post_data)" \
"http://localhost:8000/$essence/$action" | jq '.' > $essence.json
cat $essence.json
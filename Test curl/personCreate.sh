#!/bin/bash
first_name="testFirstName"
last_name="testLastName"
generate_post_data()
{
cat <<EOF
{ "first_name": "$first_name"
, "last_name": "$last_name"
}
EOF
}
essence="person"
action="create"
curl \
-H "Content-Type: application/json" \
-X POST \
--data "$(generate_post_data)" \
"http://localhost:8000/$essence/$action" | jq '.' > $essence.json
cat person.json
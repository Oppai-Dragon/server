#!/bin/bash
first_name="First"
last_name="Admin"
is_admin=true
generate_post_data()
{
cat <<EOF
{ "first_name": "$first_name"
, "last_name": "$last_name"
, "is_admin": $is_admin
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
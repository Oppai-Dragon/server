#!/bin/bash
essence="draft"
action="get"
access_key=$(jq .access_key admin.json)
generate_post_data()
{
cat <<EOF
{ "access_key": $access_key
}
EOF
}
curl \
-H "Content-Type: application/json" \
-X GET \
--data "$(generate_post_data)" \
"http://localhost:8000/$essence/$action" | jq '.'
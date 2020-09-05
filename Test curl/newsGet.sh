#!/bin/bash
essence="news"
action="get"
generate_post_data()
{
cat <<EOF
{ "page": 1
}
EOF
}
curl \
-H "Content-Type: application/json" \
-X GET \
--data "$(generate_post_data)" \
"http://localhost:8000/$essence/$action" | jq '.'
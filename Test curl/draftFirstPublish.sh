#!/bin/bash
id=$(jq .draft1.id draft.json)
access_key=$(jq .access_key admin.json)
generate_post_data()
{
cat <<EOF
{ "id": $id
, "access_key": $access_key
}
EOF
}
essence="draft"
action="publish"
curl \
-H "Content-Type: application/json" \
-X POST \
--data "$(generate_post_data)" \
"http://localhost:8000/$essence/$action" | jq '.' > news.json
cat news.json
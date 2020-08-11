#!/bin/bash
essence="news"
action="get"
generate_post_data()
{
cat <<EOF
{
}
EOF
curl \
-H "Content-Type: application/json" \
-X POST \
--data "$(generate_post_data)" \
"http://localhost:8000/$essence/$action" | jq '.'
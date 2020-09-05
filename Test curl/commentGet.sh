#!/bin/bash
essence="comment"
action="get"
news_id=$(jq .news1.id news.json)
generate_post_data()
{
cat <<EOF
{ "news_id": $news_id
}
EOF
}
curl \
-H "Content-Type: application/json" \
-X GET \
--data "$(generate_post_data)" \
"http://localhost:8000/$essence/$action" | jq '.'
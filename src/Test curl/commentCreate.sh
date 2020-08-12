#!/bin/bash
content="testContent"
news_id=$(jq .news1.id news.json)
access_key=$(jq .person1.access_key person.json)
generate_post_data()
{
cat <<EOF
{ "content": "$content"
, "news_id": $news_id
, "access_key": $access_key
}
EOF
}
essence="comment"
action="create"
curl \
-H "Content-Type: application/json" \
-X POST \
--data "$(generate_post_data)" \
"http://localhost:8000/$essence/$action" | jq '.' > $essence.json
cat $essence.json
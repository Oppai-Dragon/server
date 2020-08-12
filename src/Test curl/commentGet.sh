#!/bin/bash
essence="comment"
action="get"
news_id=$(jq .news1.id news.json)
access_key=$(jq .person1.access_key person.json)
generate_post_data()
{
cat <<EOF
{ "news_id": $news_id
, "access_key": $access_key
}
EOF
}
curl \
-H "Content-Type: application/json" \
-X GET \
--data "$(generate_post_data)" \
"http://localhost:8000/$essence/$action" | jq '.'
#!/bin/bash
name="testDraft"
content="testContent"
category_id=$(jq .category1.id category.json)
tag_id=$(jq .tag1.id tag.json)
access_key=$(jq .person1.access_key person.json)
generate_post_data()
{
cat <<EOF
{ "name": "$name"
, "content": "$content"
, "category_id": $category_id
, "tag_ids": [$tag_id]
, "access_key": $access_key
}
EOF
}
essence="draft"
action="create"
curl \
-H "Content-Type: application/json" \
-X POST \
--data "$(generate_post_data)" \
"http://localhost:8000/$essence/$action" | jq '.' > $essence.json
cat $essence.json
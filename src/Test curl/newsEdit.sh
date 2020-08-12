#!/bin/bash
id=$(jq .draft1.id draft.json)
access_key=$(jq .person1.access_key person.json)
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
"http://localhost:8000/$essence/$action" | jq '.' > $action.txt
cat $action.txt
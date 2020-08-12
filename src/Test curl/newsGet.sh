#!/bin/bash
essence="news"
action="get"
filter=$1
filterValue=$2
search=$3
searchValue=$4
sort=$5
sortValue=$6
generate_post_data()
{
cat <<EOF
{ "page": 1
, "filter_$filter": "$filterValue"
, "search_$search": "$searchValue"
, "sort_$sort": $sortValue
}
EOF
}
curl \
-H "Content-Type: application/json" \
-X GET \
--data "$(generate_post_data)" \
"http://localhost:8000/$essence/$action" | jq '.'
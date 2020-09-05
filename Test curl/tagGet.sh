#!/bin/bash
essence="tag"
action="get"
curl "http://localhost:8000/$essence/$action" | jq '.'
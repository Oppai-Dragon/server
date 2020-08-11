#!/bin/bash
essence="category"
action="get"
curl "http://localhost:8000/$essence/$action" | jq '.'
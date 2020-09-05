#!/bin/bash
essence="person"
action="get"
curl "http://localhost:8000/$essence/$action" | jq '.'
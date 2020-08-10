#!/bin/bash
curl -d '{"first_name":"TestFirstName", "last_name":"TestLastName"}' -H "Content-Type: application/json" -X POST http://localhost:8000/person/create
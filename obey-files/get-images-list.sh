#!/bin/bash
grep "CL " obaltst | awk '{print $2"001"}' | tr '[:upper:]' '[:lower:]' > fetch-list

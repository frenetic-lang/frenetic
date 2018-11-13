#!/bin/bash
ps aux | grep -i "RPC_PARALLEL_WORKER" | awk '{print $2}' | xargs sudo kill -9

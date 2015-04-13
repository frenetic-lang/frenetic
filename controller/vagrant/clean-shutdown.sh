#!/bin/env bash
# To be run before creating a VM image to share (to reduce space usage)

sudo cat /dev/zero > zero.fill; sudo sync; sleep 1; sudo sync; sudo rm -f zero.fill
sudo shutdown -h now

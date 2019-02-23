#!/usr/bin/env bash
set -ex

# Initialize the .opam and .bashrc PATH
echo "allowed_users=anybody" | sudo tee /etc/X11/Xwrapper.config
sudo apt-get install -y xfce4 virtualbox-guest-dkms virtualbox-guest-utils virtualbox-guest-x11
sudo VBoxClient --display
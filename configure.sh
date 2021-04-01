#!/bin/bash


if [ $(id -u) != "0" ]; then
  echo "You must be the superuser to run this script" >&2
  exit 1
fi

apt-get update

# Install gcc
echo "Installing g++..."
apt-get -y -qq install g++
echo "Installed g++"

# Install clang-10
echo "Installing clang-10..."
apt-get -y -qq install clang-10
echo "Installed clang-10"

# Install llvm-10
echo "Installing llvm-10..."
apt-get -y -qq install llvm-10
echo "Installed llvm-10"

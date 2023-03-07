#!/bin/bash
cd $(dirname "$0")
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
exit 0
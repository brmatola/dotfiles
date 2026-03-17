#!/bin/bash
# Dock layout configuration
# Requires: brew install dockutil
#
# Design philosophy:
#   Left  → Daily drivers (browser, comms, productivity)
#   ···   → Spacer
#   Mid   → Development tools
#   ···   → Spacer
#   Right → Media & gaming
#
#   Folders section: Downloads (grid) and Applications (list)

set -e

if ! command -v dockutil &>/dev/null; then
    echo "Error: dockutil not installed. Run: brew install dockutil"
    exit 1
fi

add_to_dock() {
    if [ -e "$1" ]; then
        dockutil --add "$1" --no-restart
    else
        echo "  Skipping (not installed): $1"
    fi
}

echo "Configuring dock layout..."

# Clear everything
dockutil --remove all --no-restart 2>/dev/null || true

###############################################################################
# Daily drivers                                                               #
###############################################################################

add_to_dock /Applications/Google\ Chrome.app
add_to_dock /Applications/Slack.app
add_to_dock /System/Applications/Messages.app
add_to_dock /Applications/Notion.app
add_to_dock /Applications/Obsidian.app
add_to_dock "/Applications/Things 3.app"

# --- spacer ---
dockutil --add '' --type small-spacer --section apps --no-restart

###############################################################################
# Development                                                                 #
###############################################################################

add_to_dock /System/Applications/Utilities/Terminal.app
add_to_dock /Applications/Visual\ Studio\ Code.app
add_to_dock /Applications/Claude.app
add_to_dock /Applications/LM\ Studio.app

# --- spacer ---
dockutil --add '' --type small-spacer --section apps --no-restart

###############################################################################
# Gaming & media                                                              #
###############################################################################

add_to_dock /Applications/Discord.app
add_to_dock /Applications/Steam.app
add_to_dock /Applications/CrossOver.app

###############################################################################
# Folders (right side, past the divider)                                      #
###############################################################################

dockutil --add ~/Downloads --view grid --display stack --sort dateadded --no-restart
dockutil --add /Applications --view list --display folder --sort name --no-restart

# Apply
killall Dock

echo "Dock configured."

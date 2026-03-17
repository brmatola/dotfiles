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

echo "Configuring dock layout..."

# Clear everything
dockutil --remove all --no-restart

###############################################################################
# Daily drivers                                                               #
###############################################################################

dockutil --add /Applications/Google\ Chrome.app --no-restart
dockutil --add /Applications/Slack.app --no-restart
dockutil --add /System/Applications/Messages.app --no-restart
dockutil --add /Applications/Notion.app --no-restart
dockutil --add /Applications/Obsidian.app --no-restart
dockutil --add /Applications/Things3.app --no-restart

# --- spacer ---
dockutil --add '' --type small-spacer --section apps --no-restart

###############################################################################
# Development                                                                 #
###############################################################################

dockutil --add /System/Applications/Utilities/Terminal.app --no-restart
dockutil --add /Applications/Visual\ Studio\ Code.app --no-restart
dockutil --add /Applications/Claude.app --no-restart
dockutil --add /Applications/LM\ Studio.app --no-restart

# --- spacer ---
dockutil --add '' --type small-spacer --section apps --no-restart

###############################################################################
# Gaming & media                                                              #
###############################################################################

dockutil --add /Applications/Discord.app --no-restart
dockutil --add /Applications/Steam.app --no-restart
dockutil --add /Applications/CrossOver.app --no-restart

###############################################################################
# Folders (right side, past the divider)                                      #
###############################################################################

dockutil --add ~/Downloads --view grid --display stack --sort dateadded --no-restart
dockutil --add /Applications --view list --display folder --sort name --no-restart

# Apply
killall Dock

echo "Dock configured."

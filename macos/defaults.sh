#!/bin/bash
# macOS defaults
# Run: ./macos/defaults.sh
# Note: Some changes require logout/restart to take effect

echo "Configuring macOS defaults..."

# Close System Preferences to prevent overriding changes
osascript -e 'tell application "System Preferences" to quit' 2>/dev/null

###############################################################################
# Keyboard                                                                    #
###############################################################################

# Fast key repeat rate
defaults write NSGlobalDomain KeyRepeat -int 2

# Short delay until key repeat
defaults write NSGlobalDomain InitialKeyRepeat -int 15

# Disable press-and-hold for keys (enables key repeat everywhere)
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

###############################################################################
# Finder                                                                      #
###############################################################################

# Show hidden files
defaults write com.apple.finder AppleShowAllFiles -bool true

# Show path bar at bottom
defaults write com.apple.finder ShowPathbar -bool true

# Show status bar
defaults write com.apple.finder ShowStatusBar -bool true

# Use list view by default
defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"

# Keep folders on top when sorting by name
defaults write com.apple.finder _FXSortFoldersFirst -bool true

# Disable warning when changing file extension
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

# Show the ~/Library folder
chflags nohidden ~/Library

###############################################################################
# Dock                                                                        #
###############################################################################

# Set dock icon size
defaults write com.apple.dock tilesize -int 48

# Minimize windows using scale effect (faster than genie)
defaults write com.apple.dock mineffect -string "scale"

# Don't show recent applications
defaults write com.apple.dock show-recents -bool false

###############################################################################
# Screenshots                                                                 #
###############################################################################

# Save screenshots to Downloads instead of Desktop
defaults write com.apple.screencapture location -string "${HOME}/Downloads"

# Save screenshots as PNG
defaults write com.apple.screencapture type -string "png"

# Disable shadow in screenshots
defaults write com.apple.screencapture disable-shadow -bool true

###############################################################################
# Safari (if used)                                                            #
###############################################################################

# Show full URL in address bar
defaults write com.apple.Safari ShowFullURLInSmartSearchField -bool true

# Enable developer menu
defaults write com.apple.Safari IncludeDevelopMenu -bool true

###############################################################################
# Activity Monitor                                                            #
###############################################################################

# Show all processes
defaults write com.apple.ActivityMonitor ShowCategory -int 0

# Sort by CPU usage
defaults write com.apple.ActivityMonitor SortColumn -string "CPUUsage"
defaults write com.apple.ActivityMonitor SortDirection -int 0

###############################################################################
# Restart affected apps                                                       #
###############################################################################

echo "Restarting affected applications..."
for app in "Finder" "Dock" "Safari"; do
    killall "${app}" &> /dev/null || true
done

echo "Done. Some changes may require logout/restart to take effect."

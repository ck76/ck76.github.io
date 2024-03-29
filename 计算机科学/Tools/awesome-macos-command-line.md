https://github.com/herrbischoff/awesome-macos-command-line

[TOC]

## Appearance

### Subpixel Anti-Aliasing (Font Smoothing)

Setting present since macOS 10.14 (Mojave).

```
# Enable
defaults write -g CGFontRenderingFontSmoothingDisabled -bool false

# Disable (Default)
defaults write -g CGFontRenderingFontSmoothingDisabled -bool true

# Per Application
defaults write com.apple.textedit CGFontRenderingFontSmoothingDisabled -bool false

# Revert for Application
defaults delete com.apple.textedit CGFontRenderingFontSmoothingDisabled
```

### Transparency

#### Transparency in Menu and Windows

```
# Reduce Transparency
defaults write com.apple.universalaccess reduceTransparency -bool true

# Restore Default Transparency
defaults write com.apple.universalaccess reduceTransparency -bool false
```

### Wallpaper

#### Set Wallpaper

```
# Up to OS X 10.8 (Mountain Lion)
osascript -e 'tell application "Finder" to set desktop picture to POSIX file "/path/to/picture.jpg"'

# Since OS X 10.9 (Mavericks)
sqlite3 ~/Library/Application\ Support/Dock/desktoppicture.db "update data set value = '/path/to/picture.jpg'" && killall Dock
```

## Applications

### App Store

#### List All Apps Downloaded from App Store

```
# Via find
find /Applications -path '*Contents/_MASReceipt/receipt' -maxdepth 4 -print |\sed 's#.app/Contents/_MASReceipt/receipt#.app#g; s#/Applications/##'

# Via Spotlight
mdfind kMDItemAppStoreHasReceipt=1
```

#### Show Debug Menu

Works up to OS X 10.10 (Yosemite).

```
# Enable
defaults write com.apple.appstore ShowDebugMenu -bool true

# Disable (Default)
defaults write com.apple.appstore ShowDebugMenu -bool false
```

### Apple Remote Desktop

#### Kickstart Manual Pages

```
sudo /System/Library/CoreServices/RemoteManagement/ARDAgent.app/Contents/Resources/kickstart -help
```

#### Activate And Deactivate the ARD Agent and Helper

```
# Activate And Restart the ARD Agent and Helper
sudo /System/Library/CoreServices/RemoteManagement/ARDAgent.app/Contents/Resources/kickstart -activate -restart -agent -console

# Deactivate and Stop the Remote Management Service
sudo /System/Library/CoreServices/RemoteManagement/ARDAgent.app/Contents/Resources/kickstart -deactivate -stop
```

#### Remote Desktop Sharing

```
# Allow Access for All Users and Give All Users Full Access
sudo /System/Library/CoreServices/RemoteManagement/ARDAgent.app/Contents/Resources/kickstart -configure -allowAccessFor -allUsers -privs -all

# Disable ARD Agent and Remove Access Privileges for All Users (Default)
sudo /System/Library/CoreServices/RemoteManagement/ARDAgent.app/Contents/Resources/kickstart -deactivate -configure -access -off
```

#### Remove Apple Remote Desktop Settings

```
sudo rm -rf /var/db/RemoteManagement ; \
sudo defaults delete /Library/Preferences/com.apple.RemoteDesktop.plist ; \
defaults delete ~/Library/Preferences/com.apple.RemoteDesktop.plist ; \
sudo rm -r /Library/Application\ Support/Apple/Remote\ Desktop/ ; \
rm -r ~/Library/Application\ Support/Remote\ Desktop/ ; \
rm -r ~/Library/Containers/com.apple.RemoteDesktop
```

### Contacts

#### Debug Mode

```
# Enable
defaults write com.apple.addressbook ABShowDebugMenu -bool true

# Disable (Default)
defaults write com.apple.addressbook ABShowDebugMenu -bool false
```

### Google

#### Uninstall Google Update

```
~/Library/Google/GoogleSoftwareUpdate/GoogleSoftwareUpdate.bundle/Contents/Resources/ksinstall --nuke
```

### iTunes

#### Keyboard Media Keys

Works up to OS X 10.10 (Yosemite). System Integrity Protection was introduced in OS X 10.11 (El Capitan) which prevents system Launch Agents from being unloaded.

```
# Stop Responding to Key Presses
launchctl unload -w /System/Library/LaunchAgents/com.apple.rcd.plist

# Respond to Key Presses (Default)
launchctl load -w /System/Library/LaunchAgents/com.apple.rcd.plist
```

From OS X 10.11 (El Capitan) on, you can either disable SIP or resort to a kind of hack, which will make iTunes inaccessible to any user, effectively preventing it from starting itself or its helpers. Be aware that for all intents and purposes this will trash your iTunes installation and may conflict with OS updates down the road.

```
sudo chmod 0000 /Applications/iTunes.app
```

### Mail

#### Show Attachments as Icons

```
defaults write com.apple.mail DisableInlineAttachmentViewing -bool yes
```

#### Vacuum Mail Index

The AppleScript code below will quit Mail, vacuum the SQLite index, then re-open Mail. On a large email database that hasn't been optimized for a while, this can provide significant improvements in responsiveness and speed.

```
(*
Speed up Mail.app by vacuuming the Envelope Index
Code from: http://web.archive.org/web/20071008123746/http://www.hawkwings.net/2007/03/03/scripts-to-automate-the-mailapp-envelope-speed-trick/
Originally by "pmbuko" with modifications by Romulo
Updated by Brett Terpstra 2012
Updated by Mathias Törnblom 2015 to support V3 in El Capitan and still keep backwards compatibility
Updated by Andrei Miclaus 2017 to support V4 in Sierra
*)

tell application "Mail" to quit
set os_version to do shell script "sw_vers -productVersion"
set mail_version to "V2"
considering numeric strings
    if "10.10" <= os_version then set mail_version to "V3"
    if "10.12" <= os_version then set mail_version to "V4"
    if "10.13" <= os_version then set mail_version to "V5"
    if "10.14" <= os_version then set mail_version to "V6"
    if "10.15" <= os_version then set mail_version to "V7"
    if "11" <= os_version then set mail_version to "V8"
end considering

set sizeBefore to do shell script "ls -lnah ~/Library/Mail/" & mail_version & "/MailData | grep -E 'Envelope Index$' | awk {'print $5'}"
do shell script "/usr/bin/sqlite3 ~/Library/Mail/" & mail_version & "/MailData/Envelope\\ Index vacuum"

set sizeAfter to do shell script "ls -lnah ~/Library/Mail/" & mail_version & "/MailData | grep -E 'Envelope Index$' | awk {'print $5'}"

display dialog ("Mail index before: " & sizeBefore & return & "Mail index after: " & sizeAfter & return & return & "Enjoy the new speed!")

tell application "Mail" to activate
```

### Safari

#### Change Default Fonts

```
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2StandardFontFamily Georgia
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2DefaultFontSize 16
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2FixedFontFamily Menlo
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2DefaultFixedFontSize 14
```

#### Develop Menu and Web Inspector

```
# Enable
defaults write com.apple.Safari IncludeInternalDebugMenu -bool true && \
defaults write com.apple.Safari IncludeDevelopMenu -bool true && \
defaults write com.apple.Safari WebKitDeveloperExtrasEnabledPreferenceKey -bool true && \
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled -bool true && \
defaults write -g WebKitDeveloperExtras -bool true

# Disable (Default)
defaults delete com.apple.Safari IncludeInternalDebugMenu && \
defaults delete com.apple.Safari IncludeDevelopMenu && \
defaults delete com.apple.Safari WebKitDeveloperExtrasEnabledPreferenceKey && \
defaults delete com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled && \
defaults delete -g WebKitDeveloperExtras
```

#### Get Current Page Data

Other options: `get source`, `get text`.

```
osascript -e 'tell application "Safari" to get URL of current tab of front window'
```

#### Use Backspace/Delete to Go Back a Page

```
# Enable
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2BackspaceKeyNavigationEnabled -bool YES

# Disable (Default)
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2BackspaceKeyNavigationEnabled -bool NO
```

### Sketch

#### Export Compact SVGs

```
defaults write com.bohemiancoding.sketch3 exportCompactSVG -bool yes
```

### Skim

#### Turn Off Auto Reload Dialog

Removes the dialog and defaults to auto reload.

```
defaults write -app Skim SKAutoReloadFileUpdate -boolean true
```

### Terminal

#### Focus Follows Mouse

```
# Enable
defaults write com.apple.Terminal FocusFollowsMouse -string YES

# Disable (Default)
defaults write com.apple.Terminal FocusFollowsMouse -string NO
```

### TextEdit

#### Use Plain Text Mode as Default

```
defaults write com.apple.TextEdit RichText -int 0
```

### Visual Studio Code

#### VSCodeVim Key Repeat

```
# Enable
defaults write com.microsoft.VSCode ApplePressAndHoldEnabled -bool false

# Disable (Default)
defaults delete com.microsoft.VSCode ApplePressAndHoldEnabled
```

#### Subpixel Anti-Aliasing

Setting present since macOS 10.14 (Mojave). See also system-wide setting: [Subpixel Anti-Aliasing](https://github.com/herrbischoff/awesome-macos-command-line#subpixel-anti-aliasing-font-smoothing)

```
# Enable
defaults write com.microsoft.VSCode CGFontRenderingFontSmoothingDisabled -bool false && \
defaults write com.microsoft.VSCode.helper CGFontRenderingFontSmoothingDisabled -bool false && \
defaults write com.microsoft.VSCode.helper.EH CGFontRenderingFontSmoothingDisabled -bool false && \
defaults write com.microsoft.VSCode.helper.NP CGFontRenderingFontSmoothingDisabled -bool false

# Disable (Default)
defaults delete com.microsoft.VSCode CGFontRenderingFontSmoothingDisabled && \
defaults delete com.microsoft.VSCode.helper CGFontRenderingFontSmoothingDisabled && \
defaults delete com.microsoft.VSCode.helper.EH CGFontRenderingFontSmoothingDisabled && \
defaults delete com.microsoft.VSCode.helper.NP CGFontRenderingFontSmoothingDisabled
```

## Backup

### Time Machine

#### Change Backup Interval

This changes the interval to 30 minutes. The integer value is the time in seconds.

```
sudo defaults write /System/Library/LaunchDaemons/com.apple.backupd-auto StartInterval -int 1800
```

#### Local Backups

Whether Time Machine performs local backups while the Time Machine backup volume is not available.

```
# Status
defaults read /Library/Preferences/com.apple.TimeMachine MobileBackups

# Enable (Default)
sudo tmutil enablelocal

# Disable
sudo tmutil disablelocal
```

Since macOS 10.13 (High Sierra), you cannot disable local snapshots. Time Machine now always creates a local APFS snapshot and uses that snapshot as the data source to create a regular backup, rather than using the live disk as the source, as is the case with HFS formatted disks.

#### Prevent Time Machine from Prompting to Use New Hard Drives as Backup Volume

```
sudo defaults write /Library/Preferences/com.apple.TimeMachine DoNotOfferNewDisksForBackup -bool true
```

#### Show Time Machine Logs

This little script will output the last 12 hours of Time Machine activity followed by live activity.

```
#!/bin/sh

filter='processImagePath contains "backupd" and subsystem beginswith "com.apple.TimeMachine"'

# show the last 12 hours
start="$(date -j -v-12H +'%Y-%m-%d %H:%M:%S')"

echo ""
echo "[History (from $start)]"
echo ""

log show --style syslog --info --start "$start" --predicate "$filter"

echo ""
echo "[Following]"
echo ""

log stream --style syslog --info --predicate "$filter"
```

#### Toggle Backup While on Battery

```
# Status
sudo defaults read /Library/Preferences/com.apple.TimeMachine RequiresACPower

# Enable (Default)
sudo defaults write /Library/Preferences/com.apple.TimeMachine RequiresACPower -bool true

# Disable
sudo defaults write /Library/Preferences/com.apple.TimeMachine RequiresACPower -bool false
```

#### Verify Backup

Beginning in OS X 10.11, Time Machine records checksums of files copied into snapshots. Checksums are not retroactively computed for files that were copied by earlier releases of OS X.

```
sudo tmutil verifychecksums /path/to/backup
```

## Developer

### Vim

#### Compile Sane Vim

Compiling MacVim via Homebrew with all bells and whistles, including overriding system Vim.

```
brew install macvim --HEAD
```

#### Neovim

Install the modern Vim drop-in alternative via Homebrew.

```
brew install neovim
```

### Xcode

#### Install Command Line Tools without Xcode

```
xcode-select --install
```

#### Remove All Unavailable Simulators

```
xcrun simctl delete unavailable
```

## Dock

#### Add a Stack with Recent Applications

Obsolete since macOS 10.14 (Mojave). See [Show Recent Apps](https://github.com/herrbischoff/awesome-macos-command-line#show-recent-apps).

```
defaults write com.apple.dock persistent-others -array-add '{ "tile-data" = { "list-type" = 1; }; "tile-type" = "recents-tile"; }' && \
killall Dock
```

#### Add a Nameless Stack Folder and Small Spacer

```
defaults write com.apple.dock persistent-others -array-add '{ "tile-data" = {}; "tile-type"="small-spacer-tile"; }' && \
killall Dock
```

#### Add a Space

```
defaults write com.apple.dock persistent-apps -array-add '{"tile-type"="spacer-tile";}' && \
killall Dock
```

#### Add a Small Space

```
defaults write com.apple.dock persistent-apps -array-add '{"tile-type"="small-spacer-tile";}' && \
killall Dock
```

#### Auto Rearrange Spaces Based on Most Recent Use

```
# Enable (Default)
defaults write com.apple.dock mru-spaces -bool true && \
killall Dock

# Disable
defaults write com.apple.dock mru-spaces -bool false && \
killall Dock
```

#### Automatically Hide

```
# Enable
defaults write com.apple.dock autohide -bool true && \
killall Dock

# Disable (Default)
defaults write com.apple.dock autohide -bool false && \
killall Dock
```

#### Icon Bounce

Global setting whether Dock icons should bounce when the respective application demands your attention.

```
# Enable (Default)
defaults write com.apple.dock no-bouncing -bool true && \
killall Dock

# Disable
defaults write com.apple.dock no-bouncing -bool false && \
killall Dock
```

#### Lock the Dock Size

```
# Enable
defaults write com.apple.Dock size-immutable -bool yes && \
killall Dock

# Disable (Default)
defaults write com.apple.Dock size-immutable -bool no && \
killall Dock
```

#### Reset Dock

```
defaults delete com.apple.dock && \
killall Dock
```

#### Resize

Fully resize your Dock's body. To resize change the `0` value as an integer.

```
defaults write com.apple.dock tilesize -int 0 && \
killall Dock
```

#### Scroll Gestures

Use your touchpad or mouse scroll wheel to interact with Dock items. Allows you to use an upward scrolling gesture to open stacks. Using the same gesture on applications that are running invokes Exposé/Mission Control.

```
# Enable
defaults write com.apple.dock scroll-to-open -bool true && \
killall Dock

# Disable (Default)
defaults write com.apple.dock scroll-to-open -bool false && \
killall Dock
```

#### Set Auto Show/Hide Delay

The float number defines the show/hide delay in ms.

```
defaults write com.apple.dock autohide-time-modifier -float 0.4 && \
defaults write com.apple.dock autohide-delay -float 0 && \
killall Dock
```

#### Show Hidden App Icons

```
# Enable
defaults write com.apple.dock showhidden -bool true && \
killall Dock

# Disable (Default)
defaults write com.apple.dock showhidden -bool false && \
killall Dock
```

#### Show Only Active Applications

```
# Enable
defaults write com.apple.dock static-only -bool true && \
killall Dock

# Disable (Default)
defaults write com.apple.dock static-only -bool false && \
killall Dock
```

#### Show Recent Apps

Setting present since macOS 10.14 (Mojave).

```
# Disable
defaults write com.apple.dock show-recents -bool false  && \
killall Dock

# Enable (Default)
defaults write com.apple.dock show-recents -bool true && \
killall Dock
```

#### Single App Mode

When clicking an application icon in the Dock, the respective windows will come to the front, but all other application windows will be hidden.

```
# Enable
defaults write com.apple.dock single-app -bool true && \
killall Dock

# Disable (Default)
defaults write com.apple.dock single-app -bool false && \
killall Dock
```

## Documents

#### Convert File to HTML

Supported formats are plain text, rich text (rtf) and Microsoft Word (doc/docx).

```
textutil -convert html file.ext
```

## Files, Disks and Volumes

#### Create an Empty File

Creates an empty 10 gigabyte test file.

```
mkfile 10g /path/to/file
```

#### Disable Sudden Motion Sensor

Leaving this turned on is useless when you're using SSDs.

```
sudo pmset -a sms 0
```

#### Eject All Mountable Volumes

The only reliable way to do this is by sending an AppleScript command to Finder.

```
osascript -e 'tell application "Finder" to eject (every disk whose ejectable is true)'
```

#### Repair File Permissions

You don't have to use the Disk Utility GUI for this.

```
sudo diskutil repairPermissions /
```

> Beginning with OS X 10.11 (El Capitan), system file permissions are automatically protected. It's no longer necessary to verify or repair permissions with Disk Utility. ([Source](https://support.apple.com/en-us/HT201560))

#### Set Boot Volume

```
# Up to OS X 10.10 (Yosemite)
bless --mount "/path/to/mounted/volume" --setBoot

# From OS X 10.11 (El Capitan)
sudo systemsetup -setstartupdisk /System/Library/CoreServices
```

#### Show All Attached Disks and Partitions

```
diskutil list
```

#### View File System Usage

A continuous stream of file system access info.

```
sudo fs_usage
```

### APFS

Present since macOS 10.13 (High Sierra). There is no central utility and usage is inconsistent as most functionality is rolled into `tmutil`.

#### Convert Volume from HFS+ to APFS

```
/System/Library/Filesystems/apfs.fs/Contents/Resources/hfs_convert /path/to/file/system
```

#### Create New APFS Filesystem

```
/System/Library/Filesystems/apfs.fs/Contents/Resources/newfs_apfs /path/to/device
```

#### Create Snapshot

```
tmutil localsnapshot
```

#### Delete Snapshot

```
tmutil deletelocalsnapshots com.apple.TimeMachine.2018-01-26-044042
```

#### List Snapshots

```
tmutil listlocalsnapshots /
```

#### Mount Snapshot

Snapshots are read-only.

```
mkdir ~/mnt
/System/Library/Filesystems/apfs.fs/Contents/Resources/mount_apfs -s com.apple.TimeMachine.2018-01-26-044042 / ~/mnt
```

### Disk Images

#### Create Disk Image From Folder Contents

```
hdiutil create -volname "Volume Name" -srcfolder /path/to/folder -ov diskimage.dmg
```

If you'd like to encrypt the disk image:

```
hdiutil create -encryption -stdinpass -volname "Volume Name" -srcfolder /path/to/folder -ov encrypted.dmg
```

By default, you'll be prompted for a password. You can automate that by piping in a password:

```
echo -n YourPassword | hdiutil create -encryption -stdinpass -volname "Volume Name" -srcfolder /path/to/folder -ov encrypted.dmg
```

#### Burn Disk Images to DVD

This command applies to .iso, .img and .dmg images.

```
hdiutil burn /path/to/image_file
```

#### Create Temporary High-Performance Disk

The disk is backed by physical RAM and will be several times faster than an SSD. The contents of the disk cannot be recovered after it has been ejected. The example below is for a 500 MiB RAM disk, adjust as needed.

```
# Up to macOS 10.14 (Mojave)
let DISKSIZE=500*2048 && \
diskutil erasevolume HFS+ "RAM Disk" `hdiutil attach -nomount ram://$DISKSIZE`

# From macOS 10.15 (Catalina) on
let "DISKSIZE = 500*2048" && \
diskutil erasevolume HFS+ "RAM Disk" `hdiutil attach -nomount ram://$DISKSIZE`
```

#### Disable Disk Image Verification

```
defaults write com.apple.frameworks.diskimages skip-verify -bool true && \
defaults write com.apple.frameworks.diskimages skip-verify-locked -bool true && \
defaults write com.apple.frameworks.diskimages skip-verify-remote -bool true
```

#### Make Volume OS X Bootable

```
bless --folder "/path/to/mounted/volume/System/Library/CoreServices" --bootinfo --bootefi
```

#### Mount Disk Image

```
hdiutil attach /path/to/diskimage.dmg
```

#### Unmount Disk Image

```
hdiutil detach /dev/disk2s1
```

#### Write Disk Image to Volume

Like the Disk Utility "Restore" function.

```
sudo asr -restore -noverify -source /path/to/diskimage.dmg -target /Volumes/VolumeToRestoreTo
```

## Finder

### Desktop

#### Show External Media

External HDs, thumb drives, etc.

```
# Enable
defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool true && \
killall Finder

# Disable (Default)
defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool false && \
killall Finder
```

#### Show Internal Media

Built-in HDs or SSDs.

```
# Enable
defaults write com.apple.finder ShowHardDrivesOnDesktop -bool true && \
killall Finder

# Disable (Default)
defaults write com.apple.finder ShowHardDrivesOnDesktop -bool false && \
killall Finder
```

#### Show Removable Media

CDs, DVDs, iPods, etc.

```
# Enable
defaults write com.apple.finder ShowRemovableMediaOnDesktop -bool true && \
killall Finder

# Disable (Default)
defaults write com.apple.finder ShowRemovableMediaOnDesktop -bool false && \
killall Finder
```

#### Show Network Volumes

AFP, SMB, NFS, WebDAV, etc.

```
# Enable
defaults write com.apple.finder ShowMountedServersOnDesktop -bool true && \
killall Finder

# Disable (Default)
defaults write com.apple.finder ShowMountedServersOnDesktop -bool false && \
killall Finder
```

### Files and Folders

#### Clear All ACLs

```
sudo chmod -RN /path/to/folder
```

#### Increase Number of Recent Places

```
defaults write -g NSNavRecentPlacesLimit -int 10 && \
killall Finder
```

#### Show All File Extensions

```
defaults write -g AppleShowAllExtensions -bool true
```

#### Set Protected Flag

This is equivalent to Finder "Locked" status.

```
# Disable (Default)
sudo chflags -R nouchg /path/to/file/or/folder

# Enable
sudo chflags -R uchg /path/to/file/or/folder
```

#### Show Hidden Files

```
# Show All
defaults write com.apple.finder AppleShowAllFiles true

# Restore Default File Visibility
defaults write com.apple.finder AppleShowAllFiles false
```

#### Show Full Path in Finder Title

```
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true
```

#### Toggle Folder Visibility in Finder

By default, the `~/Library` folder is hidden. You can easily show it again. The same method works with all other folders.

```
# Hidden (Default)
chflags hidden ~/Library

# Visible
chflags nohidden ~/Library
```

### Layout

#### Show "Quit Finder" Menu Item

Makes possible to see Finder menu item "Quit Finder" with default shortcut Cmd + Q

```
# Enable
defaults write com.apple.finder QuitMenuItem -bool true && \
killall Finder

# Disable (Default)
defaults write com.apple.finder QuitMenuItem -bool false && \
killall Finder
```

#### Smooth Scrolling

Useful if you’re on an older Mac that messes up the animation.

```
# Disable
defaults write -g NSScrollAnimationEnabled -bool false

# Enable (Default)
defaults write -g NSScrollAnimationEnabled -bool true
```

#### Rubberband Scrolling

```
# Disable
defaults write -g NSScrollViewRubberbanding -bool false

# Enable (Default)
defaults write -g NSScrollViewRubberbanding -bool true
```

#### Expand Save Panel by Default

```
defaults write -g NSNavPanelExpandedStateForSaveMode -bool true && \
defaults write -g NSNavPanelExpandedStateForSaveMode2 -bool true
```

#### Desktop Icon Visibility

```
# Hide Icons
defaults write com.apple.finder CreateDesktop -bool false && \
killall Finder

# Show Icons (Default)
defaults write com.apple.finder CreateDesktop -bool true && \
killall Finder
```

#### Path Bar

```
# Show
defaults write com.apple.finder ShowPathbar -bool true

# Hide (Default)
defaults write com.apple.finder ShowPathbar -bool false
```

#### Scrollbar Visibility

Possible values: `WhenScrolling`, `Automatic` and `Always`.

```
defaults write -g AppleShowScrollBars -string "Always"
```

#### Status Bar

```
# Show
defaults write com.apple.finder ShowStatusBar -bool true

# Hide (Default)
defaults write com.apple.finder ShowStatusBar -bool false
```

#### Save to Disk by Default

Sets default save target to be a local disk, not iCloud.

```
defaults write -g NSDocumentSaveNewDocumentsToCloud -bool false
```

#### Set Current Folder as Default Search Scope

```
defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"
```

#### Set Default Finder Location to Home Folder

```
defaults write com.apple.finder NewWindowTarget -string "PfLo" && \
defaults write com.apple.finder NewWindowTargetPath -string "file://${HOME}"
```

#### Set Sidebar Icon Size

Sets size to 'medium'.

```
defaults write -g NSTableViewDefaultSizeMode -int 2
```

### Metadata Files

#### Disable Creation of Metadata Files on Network Volumes

Avoids creation of `.DS_Store` and AppleDouble files.

```
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
```

#### Disable Creation of Metadata Files on USB Volumes

Avoids creation of `.DS_Store` and AppleDouble files.

```
defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true
```

### Opening Things

#### Change Working Directory to Finder Path

If multiple windows are open, it chooses the top-most one.

```
cd "$(osascript -e 'tell app "Finder" to POSIX path of (insertion location as alias)')"
```

#### Open URL

```
open https://github.com
```

#### Open File

```
open README.md
```

#### Open Applications

You can open applications using `-a`.

```
open -a "Google Chrome" https://github.com
```

#### Open Folder

```
open /path/to/folder/
```

#### Open Current Folder

```
open .
```

## Fonts

#### Clear Font Cache for Current User

To clear font caches for all users, put `sudo` in front of these commands.

```
atsutil databases -removeUser && \
atsutil server -shutdown && \
atsutil server -ping
```

#### Get SF Mono Fonts

Starting in macOS 10.15 (Catalina), the Utilities apps (including Terminal.app) are now found in the `/System` folder.

```
cp -v /Applications/Xcode-beta.app/Contents/SharedFrameworks/DVTKit.framework/Versions/A/Resources/Fonts/SFMono-* ~/Library/Fonts
```

From macOS 10.12 (Sierra) on, they are included in Terminal.app.

```
cp -v /Applications/Utilities/Terminal.app/Contents/Resources/Fonts/SFMono-* ~/Library/Fonts
```

In older OS versions, you need to download and install Xcode 8 beta for this to work. Afterwards they should be available in all applications.

```
cp -v /System/Applications/Utilities/Terminal.app/Contents/Resources/Fonts/SFMono-* ~/Library/Fonts
```

## Functions

Please see [this file](https://github.com/herrbischoff/awesome-macos-command-line/blob/master/functions.md).

## Hardware

### Bluetooth

Up to OS X 10.12 (Sierra) the Bluetooth daemon is named `blued` instead of `bluetoothd`. You need to adjust the `killall` command accordingly.

```
# Status
defaults read /Library/Preferences/com.apple.Bluetooth ControllerPowerState

# Enable (Default)
sudo defaults write /Library/Preferences/com.apple.Bluetooth ControllerPowerState -int 1

# Disable
sudo defaults write /Library/Preferences/com.apple.Bluetooth ControllerPowerState -int 0 && \
sudo killall -HUP bluetoothd
```

### Harddisks

#### Force Trim

Enable Trim for non-Apple SSDs. This command is present since OS X 10.10 (Yosemite).

```
trimforce
```

### Hardware Information

#### List All Hardware Ports

```
networksetup -listallhardwareports
```

#### Remaining Battery Percentage

```
pmset -g batt | egrep "([0-9]+\%).*" -o --colour=auto | cut -f1 -d';'
```

#### Remaining Battery Time

```
pmset -g batt | egrep "([0-9]+\%).*" -o --colour=auto | cut -f3 -d';'
```

#### Show Connected Device's UDID

```
system_profiler SPUSBDataType | sed -n -e '/iPad/,/Serial/p' -e '/iPhone/,/Serial/p'
```

#### Show Current Screen Resolution

```
system_profiler SPDisplaysDataType | grep Resolution
```

#### Show CPU Brand String

```
sysctl -n machdep.cpu.brand_string
```

### Infrared Receiver

```
# Status
defaults read /Library/Preferences/com.apple.driver.AppleIRController DeviceEnabled

# Enable (Default)
defaults write /Library/Preferences/com.apple.driver.AppleIRController DeviceEnabled -int 1

# Disable
defaults write /Library/Preferences/com.apple.driver.AppleIRController DeviceEnabled -int 0
```

### Power Management

#### Prevent System Sleep

Prevent sleep for 1 hour:

```
caffeinate -u -t 3600
```

#### Show All Power Management Settings

```
sudo pmset -g
```

#### Put Display to Sleep after 15 Minutes of Inactivity

```
sudo pmset displaysleep 15
```

#### Put Computer to Sleep after 30 Minutes of Inactivity

```
sudo pmset sleep 30
```

#### Check System Sleep Idle Time

```
sudo systemsetup -getcomputersleep
```

#### Set System Sleep Idle Time to 60 Minutes

```
sudo systemsetup -setcomputersleep 60
```

#### Turn Off System Sleep Completely

```
sudo systemsetup -setcomputersleep Never
```

#### Automatic Restart on System Freeze

```
sudo systemsetup -setrestartfreeze on
```

#### Chime When Charging

Play iOS charging sound when MagSafe is connected.

```
## Up to macOS 10.12 (Sierra)

# Enable
defaults write com.apple.PowerChime ChimeOnAllHardware -bool true && \
open /System/Library/CoreServices/PowerChime.app

# Disable (Default)
defaults write com.apple.PowerChime ChimeOnAllHardware -bool false && \
killall PowerChime
## From macOS 10.13 (High Sierra) on

# Enable (Default)
defaults write com.apple.PowerChime ChimeOnNoHardware -bool false && \
open /System/Library/CoreServices/PowerChime.app

# Disable
defaults write com.apple.PowerChime ChimeOnNoHardware -bool true && \
killall PowerChime
```

## Input Devices

### Keyboard

#### Auto-Correct

```
# Disable
defaults write -g NSAutomaticSpellingCorrectionEnabled -bool false

# Enable (Default)
defaults write -g NSAutomaticSpellingCorrectionEnabled -bool true

# Show Status
defaults read -g NSAutomaticSpellingCorrectionEnabled
```

#### Full Keyboard Access

Enable Tab in modal dialogs.

```
# Text boxes and lists only (Default)
defaults write NSGlobalDomain AppleKeyboardUIMode -int 0

# All controls
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3
```

#### Key Repeat

Change the "press and hold" behavior.

```
# Enable
defaults write -g ApplePressAndHoldEnabled -bool false

# Disable (Default)
defaults write -g ApplePressAndHoldEnabled -bool true
```

#### Key Repeat Rate

Sets a very fast repeat rate, adjust to taste.

```
defaults write -g KeyRepeat -int 0.02
```

## Launchpad

#### Reset Launchpad Layout

You need to restart `Dock` because Launchpad is tied to it.

```
# Up to OS X 10.10 (Yosemite)
rm ~/Library/Application\ Support/Dock/*.db && \
killall Dock

# From OS X 10.11 (El Capitan)
defaults write com.apple.dock ResetLaunchPad -bool true && \
killall Dock
```

## Media

### Audio

#### Convert Audio File to iPhone Ringtone

```
afconvert input.mp3 ringtone.m4r -f m4af
```

#### Create Audiobook From Text

Uses "Alex" voice, a plain UTF-8 encoded text file for input and AAC output.

```
say -v Alex -f file.txt -o "output.m4a"
```

#### Disable Sound Effects on Boot

```
sudo nvram SystemAudioVolume=" "
```

#### Mute Audio Output

```
osascript -e 'set volume output muted true'
```

#### Set Audio Volume

```
osascript -e 'set volume 4'
```

#### Play Audio File

You can play all audio formats that are natively supported by QuickTime.

```
afplay -q 1 filename.mp3
```

#### Speak Text with System Default Voice

```
say 'All your base are belong to us!'
```

#### Startup Chime

Older Macs:

```
# Enable (Default)
sudo nvram BootAudio=%01

# Disable
sudo nvram BootAudio=%00
```

From 2016 models on:

```
# Enable
sudo nvram StartupMute=%00

# Disable (Default)
sudo nvram StartupMute=%01
```

### Video

#### Auto-Play Videos in QuickTime Player

```
defaults write com.apple.QuickTimePlayerX MGPlayMovieOnOpen 1
```

## Networking

### Bonjour

#### Bonjour Service

```
# Disable
sudo defaults write /System/Library/LaunchDaemons/com.apple.mDNSResponder.plist ProgramArguments -array-add "-NoMulticastAdvertisements"

# Enable (Default)
sudo defaults write /System/Library/LaunchDaemons/com.apple.mDNSResponder.plist ProgramArguments -array "/usr/sbin/mDNSResponder" "-launchd"
```

### DHCP

#### Renew DHCP Lease

```
sudo ipconfig set en0 DHCP
```

#### Show DHCP Info

```
ipconfig getpacket en0
```

### DNS

#### Clear DNS Cache

```
sudo dscacheutil -flushcache && \
sudo killall -HUP mDNSResponder
```

### Hostname

#### Set Computer Name/Host Name

```
sudo scutil --set ComputerName "newhostname" && \
sudo scutil --set HostName "newhostname" && \
sudo scutil --set LocalHostName "newhostname" && \
sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.smb.server NetBIOSName -string "newhostname"
```

### Network Preferences

#### Network Locations

Switch between network locations created in the Network preference pane.

```
# Status
scselect

# Switch Network Location
scselect LocationNameFromStatus
```

#### Set Static IP Address

```
networksetup -setmanual "Ethernet" 192.168.2.100 255.255.255.0 192.168.2.1
```

### Networking Tools

#### Ping a Host to See Whether It’s Available

```
ping -o github.com
```

#### Troubleshoot Routing Problems

```
traceroute github.com
```

### SSH

#### Permanently Add Private Key Passphrase to SSH Agent

> Prior to macOS 10.12 (Sierra), ssh would present a dialog asking for your passphrase and would offer the option to store it into the keychain. This UI was deprecated some time ago and has been removed.
>
> Instead, a new UseKeychain option was introduced in macOS 10.12 (Sierra) allowing users to specify whether they would like for the passphrase to be stored in the keychain. This option was enabled by default on macOS 10.12 (Sierra), which caused all passphrases to be stored in the keychain.
>
> This was not the intended default behavior, so this has been changed in macOS 10.12.2. ([Source](https://developer.apple.com/library/archive/technotes/tn2449/_index.html))

```
ssh-add -K /path/to/private_key
```

Then add to `~/.ssh/config`:

```
Host server.example.com
    IdentityFile /path/to/private_key
    UseKeychain yes
```

#### Remote Login

```
# Enable
sudo launchctl load -w /System/Library/LaunchDaemons/ssh.plist

# Disable (Default)
sudo launchctl unload -w /System/Library/LaunchDaemons/ssh.plist
```

### TCP/IP

#### Show Application Using a Certain Port

This outputs all applications currently using port 80.

```
sudo lsof -i :80
```

#### Show External IP Address

Works if your ISP doesn't replace DNS requests (which it shouldn't).

```
dig +short myip.opendns.com @resolver1.opendns.com
```

Alternative that works on all networks.

```
curl -s https://api.ipify.org && echo
```

#### Show Network Interface Information

Undocumented flag of the `scutil` command.

```
scutil --nwi
```

### TFTP

#### Start Native TFTP Daemon

Files will be served from `/private/tftpboot`.

```
sudo launchctl load -F /System/Library/LaunchDaemons/tftp.plist && \
sudo launchctl start com.apple.tftpd
```

### Wi-Fi

#### Join a Wi-Fi Network

```
networksetup -setairportnetwork en0 WIFI_SSID WIFI_PASSWORD
```

#### Scan Available Access Points

Create a symbolic link to the airport command for easy access:

```
sudo ln -s /System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport /usr/local/bin/airport
```

Run a wireless scan:

```
airport -s
```

#### Show Current SSID

```
/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport -I | awk '/ SSID/ {print substr($0, index($0, $2))}'
```

#### Show Local IP Address

```
ipconfig getifaddr en0
```

#### Show Wi-Fi Connection History

```
defaults read /Library/Preferences/SystemConfiguration/com.apple.airport.preferences | grep LastConnected -A 7
```

#### Show Wi-Fi Network Passwords

Exchange SSID with the SSID of the access point you wish to query the password from.

```
security find-generic-password -D "AirPort network password" -a "SSID" -gw
```

#### Turn on Wi-Fi Adapter

```
networksetup -setairportpower en0 on
```

## Package Managers

- [Fink](http://www.finkproject.org/) - The full world of Unix Open Source software for Darwin. A little outdated.
- [Homebrew](https://brew.sh/) - The missing package manager for OS X. The most popular choice.
- [MacPorts](https://www.macports.org/) - Compile, install and upgrade either command-line, X11 or Aqua based open-source software.

### Homebrew

#### Full Uninstall

```
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/uninstall)"
```

## Printing

#### Clear Print Queue

```
cancel -a -
```

#### Expand Print Panel by Default

```
defaults write -g PMPrintingExpandedStateForPrint -bool true && \
defaults write -g PMPrintingExpandedStateForPrint2 -bool true
```

#### Quit Printer App After Print Jobs Complete

```
defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true
```

## Security

### Application Firewall

#### Firewall Service

```
# Show Status
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --getglobalstate

# Enable
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setglobalstate on

# Disable (Default)
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setglobalstate off
```

#### Add Application to Firewall

```
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --add /path/to/file
```

### Gatekeeper

#### Add Gatekeeper Exception

```
spctl --add /path/to/Application.app
```

#### Remove Gatekeeper Exception

```
spctl --remove /path/to/Application.app
```

#### Manage Gatekeeper

Especially helpful with the annoying macOS 10.15 (Catalina) system popup blocking execution of non-signed apps.

```
# Status
spctl --status

# Enable (Default)
sudo spctl --master-enable

# Disable
sudo spctl --master-disable
```

### Passwords

#### Generate Secure Password and Copy to Clipboard

```
LC_ALL=C tr -dc "[:alnum:]" < /dev/urandom | head -c 20 | pbcopy
```

### Physical Access

#### Launch Screen Saver

```
# Up to macOS 10.12 (Sierra)
open /System/Library/Frameworks/ScreenSaver.framework/Versions/A/Resources/ScreenSaverEngine.app

# From macOS 10.13 (High Sierra)
/System/Library/CoreServices/ScreenSaverEngine.app/Contents/MacOS/ScreenSaverEngine
```

#### Lock Screen

```
/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend
```

#### Screensaver Immediate Lock

```
# Status
defaults read com.apple.screensaver askForPasswordDelay

# Enable (Default)
defaults write com.apple.screensaver askForPasswordDelay -int 0

# Disable (Integer = lock delay in seconds)
defaults write com.apple.screensaver askForPasswordDelay -int 10
```

#### Screensaver Password

```
# Status
defaults read com.apple.screensaver askForPassword

# Enable
defaults write com.apple.screensaver askForPassword -int 1

# Disable (Default)
defaults write com.apple.screensaver askForPassword -int 0
```

### Privacy Database

The `tccutil` command manages the privacy database, which stores decisions the user has made about whether apps may access personal data. You need to close all applications except Terminal before running any of these commands.

```
# Full Reset for All Applications
sudo tccutil reset All

# Reset Adress Book Access
sudo tccutil reset AddressBook

# Reset All Permission for Terminal.app
sudo tccutil reset All com.apple.Terminal
```

### Wiping Data

Note: The `srm` command appears to have been removed on MacOS after 10.9. There is a note on an [Apple support page](https://support.apple.com/en-us/HT201949) hinting as to why:

> With an SSD drive, Secure Erase and Erasing Free Space are not available in Disk Utility. These options are not needed for an SSD drive because a standard erase makes it difficult to recover data from an SSD.

#### Securely Remove File

```
srm /path/to/file
```

#### Securely Remove Folder

```
srm -r /path/to/folder/
```

#### Securely Remove Path (Force)

```
srm -rf /path/to/complete/destruction
```

## Search

### Find

#### Recursively Delete .DS_Store Files

```
find . -type f -name '.DS_Store' -ls -delete
```

### Locate

#### Build Locate Database

```
sudo launchctl load -w /System/Library/LaunchDaemons/com.apple.locate.plist
```

#### Search via Locate

The `-i` modifier makes the search case insensitive.

```
locate -i *.jpg
```

## System

### AirDrop

#### AirDrop over Ethernet on Unsupported Macs

```
# Enable
defaults write com.apple.NetworkBrowser BrowseAllInterfaces -bool true && \
defaults remove com.apple.NetworkBrowser DisableAirDrop

# Disable (Default)
defaults delete com.apple.NetworkBrowser BrowseAllInterfaces && \
defaults write com.apple.NetworkBrowser DisableAirDrop -bool YES
```

### AppleScript

#### Execute AppleScript

```
osascript /path/to/script.scpt
```

### Basics

#### Compare Two Folders

```
diff -qr /path/to/folder1 /path/to/folder2
```

#### Copy Large File with Progress

Make sure you have `pv` installed and replace `/dev/rdisk2` with the appropriate write device or file.

```
FILE=/path/to/file.iso pv -s $(du -h $FILE | awk '/.*/ {print $1}') $FILE | sudo dd of=/dev/rdisk2 bs=1m
```

#### Restore Sane Shell

In case your shell session went insane (some script or application turned it into a garbled mess).

```
stty sane
```

#### Restart

```
sudo reboot
```

#### Shutdown

```
sudo poweroff
```

#### Show Build Number of OS

```
sw_vers
```

#### Uptime

How long since your last restart.

```
uptime
```

### Clipboard

#### Copy data to Clipboard

```
cat whatever.txt | pbcopy
```

#### Convert Clipboard to Plain Text

```
pbpaste | textutil -convert txt -stdin -stdout -encoding 30 | pbcopy
```

#### Convert Tabs to Spaces for Clipboard Content

```
pbpaste | expand | pbcopy
```

#### Copy data from Clipboard

```
pbpaste > whatever.txt
```

#### Sort and Strip Duplicate Lines from Clipboard Content

```
pbpaste | sort | uniq | pbcopy
```

### FileVault

#### Automatically Unlock FileVault on Restart

If FileVault is enabled on the current volume, it restarts the system, bypassing the initial unlock. The command may not work on all systems.

```
sudo fdesetup authrestart
```

#### FileVault Service

```
# Status
sudo fdesetup status

# Enable
sudo fdesetup enable

# Disable (Default)
sudo fdesetup disable
```

### Information/Reports

#### Generate Advanced System and Performance Report

```
sudo sysdiagnose -f ~/Desktop/
```

### Installation

#### Create Bootable Installer

```
# macOS 11 (Big Sur)
sudo /Applications/Install\ macOS\ Big\ Sur.app/Contents/Resources/createinstallmedia --volume /Volumes/USB --nointeraction --downloadassets

# macOS 10.15 (Catalina)
sudo /Applications/Install\ macOS\ Catalina.app/Contents/Resources/createinstallmedia --volume /Volumes/USB --nointeraction --downloadassets

# macOS 10.14 (Mojave)
sudo /Applications/Install\ macOS\ Mojave.app/Contents/Resources/createinstallmedia --volume /Volumes/USB --nointeraction --downloadassets

# macOS 10.13 (High Sierra)
sudo /Applications/Install\ macOS\ High\ Sierra.app/Contents/Resources/createinstallmedia --volume /Volumes/USB --applicationpath /Applications/Install\ macOS\ High\ Sierra.app

# macOS 10.12 (Sierra)
sudo /Applications/Install\ macOS\ Sierra.app/Contents/Resources/createinstallmedia --volume /Volumes/USB --applicationpath /Applications/Install\ macOS\ Sierra.app

# OS X 10.11 (El Capitan)
sudo /Applications/Install\ OS\ X\ El\ Capitan.app/Contents/Resources/createinstallmedia --volume /Volumes/USB --applicationpath /Applications/Install\ OS\ X\ El\ Capitan.app

# OS X 10.10 (Yosemite)
sudo /Applications/Install\ OS\ X\ Yosemite.app/Contents/Resources/createinstallmedia --volume /Volumes/USB --applicationpath /Applications/Install\ OS\ X\ Yosemite.app
```

- For confirmation before erasing the drive, remove `–-nointeraction` from the command.
- The optional `–-downloadassets` flag is new in macOS 10.14 (Mojave). It downloads assets which may be required during installation, like updates.
- The `–-applicationpath` flag is deprecated since macOS 10.14 (Mojave) and will throw an error if used.

#### Download Older OS Versions

| Version       | Codename      | Download                                                     |
| ------------- | ------------- | ------------------------------------------------------------ |
| Mac OS X 10.0 | Cheetah       | n/a                                                          |
| Mac OS X 10.1 | Puma          | n/a                                                          |
| Mac OS X 10.2 | Jaguar        | n/a                                                          |
| Mac OS X 10.3 | Panther       | n/a                                                          |
| Mac OS X 10.4 | Tiger         | n/a                                                          |
| Mac OS X 10.5 | Leopard       | n/a                                                          |
| Mac OS X 10.6 | Snow Leopard  | n/a                                                          |
| Mac OS X 10.7 | Lion          | [Direct Download](https://updates.cdn-apple.com/2021/macos/041-7683-20210614-E610947E-C7CE-46EB-8860-D26D71F0D3EA/InstallMacOSX.dmg) |
| OS X 10.8     | Mountain Lion | [Direct Download](https://updates.cdn-apple.com/2021/macos/031-0627-20210614-90D11F33-1A65-42DD-BBEA-E1D9F43A6B3F/InstallMacOSX.dmg) |
| OS X 10.9     | Mavericks     | n/a                                                          |
| OS X 10.10    | Yosemite      | [Direct Download](http://updates-http.cdn-apple.com/2019/cert/061-41343-20191023-02465f92-3ab5-4c92-bfe2-b725447a070d/InstallMacOSX.dmg) |
| OS X 10.11    | El Capitan    | [Direct Download](http://updates-http.cdn-apple.com/2019/cert/061-41424-20191024-218af9ec-cf50-4516-9011-228c78eda3d2/InstallMacOSX.dmg) |
| macOS 10.12   | Sierra        | [Direct Download](http://updates-http.cdn-apple.com/2019/cert/061-39476-20191023-48f365f4-0015-4c41-9f44-39d3d2aca067/InstallOS.dmg) |
| macOS 10.13   | High Sierra   | [App Store](https://apps.apple.com/de/app/macos-high-sierra/id1246284741) |
| macOS 10.14   | Mojave        | [App Store](https://apps.apple.com/de/app/macos-mojave/id1398502828) |
| macOS 10.15   | Catalina      | [App Store](https://apps.apple.com/de/app/macos-catalina/id1466841314) |
| macOS 11      | Big Sur       | [App Store](https://apps.apple.com/de/app/macos-big-sur/id1526878132) |

### Kernel Extensions

#### Display Status of Loaded Kernel Extensions

```
sudo kextstat -l
```

#### Load Kernel Extension

```
sudo kextload -b com.apple.driver.ExampleBundle
```

#### Unload Kernel Extensions

```
sudo kextunload -b com.apple.driver.ExampleBundle
```

### LaunchAgents

Please see [this file](https://github.com/herrbischoff/awesome-macos-command-line/blob/master/launchagents.md).

### LaunchServices

#### Rebuild LaunchServices Database

To be independent of OS X version, this relies on `locate` to find `lsregister`. If you do not have your `locate` database built yet, [do it](https://github.com/herrbischoff/awesome-macos-command-line#build-locate-database).

```
sudo $(locate lsregister) -kill -seed -r
```

### Login Window

#### Set Login Window Text

```
sudo defaults write /Library/Preferences/com.apple.loginwindow LoginwindowText "Your text"
```

### Memory Management

#### Purge memory cache

```
sudo purge
```

#### Show Memory Statistics

```
# One time
vm_stat

# Table of data, repeat 10 times total, 1 second wait between each poll
vm_stat -c 10 1
```

### Notification Center

#### Notification Center Service

```
# Disable
launchctl unload -w /System/Library/LaunchAgents/com.apple.notificationcenterui.plist && \
killall -9 NotificationCenter

# Enable (Default)
launchctl load -w /System/Library/LaunchAgents/com.apple.notificationcenterui.plist
```

### QuickLook

#### Preview via QuickLook

```
qlmanage -p /path/to/file
```

### Remote Management

See also: [Apple Remote Desktop](https://github.com/herrbischoff/awesome-macos-command-line#apple-remote-desktop).

#### Prevent Double Password Entry

When logging into a Mac remotely via Apple Remote Desktop or VNC, you are sometimes required to enter your password a second time after connecting to the Mac. While you can disable this behavior, it is explicitly not recommend to turn this functionality off unless you are certain that no one else will be able to access your Mac physically when you are away.

```
# Disable
sudo defaults write /Library/Preferences/com.apple.RemoteManagement.plist RestoreMachineState -bool no

# Enable (Default)
sudo defaults write /Library/Preferences/com.apple.RemoteManagement.plist RestoreMachineState -bool yes
```

#### Remote Apple Events

```
# Status
sudo systemsetup -getremoteappleevents

# Enable
sudo systemsetup -setremoteappleevents on

# Disable (Default)
sudo systemsetup -setremoteappleevents off
```

### Root User

```
# Enable
dsenableroot

# Disable (Default)
dsenableroot -d
```

### Safe Mode Boot

```
# Status
nvram boot-args

# Enable
sudo nvram boot-args="-x"

# Disable (Default)
sudo nvram boot-args=""
```

### Save Dialogs

Significantly improve the now rather slow animation in save dialogs.

```
defaults write NSGlobalDomain NSWindowResizeTime .001
```

### Screenshots

#### Take Delayed Screenshot

Takes a screenshot as JPEG after 3 seconds and displays in Preview.

```
screencapture -T 3 -t jpg -P delayedpic.jpg
```

#### Save Screenshots to Given Location

Sets location to `~/Desktop`.

```
defaults write com.apple.screencapture location ~/Desktop && \
killall SystemUIServer
```

#### Save Screenshots in Given Format

Sets format to `png`. Other options are `bmp`, `gif`, `jpg`, `jpeg`, `pdf`, `tiff`.

```
defaults write com.apple.screencapture type -string "png"
```

#### Disable Shadow in Screenshots

```
defaults write com.apple.screencapture disable-shadow -bool true && \
killall SystemUIServer
```

#### Set Default Screenshot Name

Date and time remain unchanged.

```
defaults write com.apple.screencapture name "Example name" && \
killall SystemUIServer
```

### Software Installation

#### Install PKG

```
installer -pkg /path/to/installer.pkg -target /
```

### Sidecar

#### Use on Incompatible Macs

This may or may not work, depending on the age of the machine.

```
# Enable
defaults write com.apple.sidecar.display AllowAllDevices -bool true && \
defaults write com.apple.sidecar.display hasShownPref -bool true

# Disable (Default)
defaults delete com.apple.sidecar.display
```

### Software Update

#### Ignore Specific Software Update

The identifier can be found via `softwareupdate --list`. In the example below, being on macOS 10.14 (Mojave), will ignore all update prompts to macOS 10.15 (Catalina), since the latter removes 32-bit support.

```
sudo /usr/sbin/softwareupdate --ignore "macOS Catalina"
```

#### Install All Available Software Updates

```
sudo softwareupdate -ia
```

#### Set Software Update Check Interval

Set to check daily instead of weekly.

```
defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 1
```

#### Show Available Software Updates

```
sudo softwareupdate --list
```

#### Set Software Update Server

This should only be done for testing purposes or unmanaged clients. To use network-wide, either correctly set up DNS along with [Apple SUS service](http://krypted.com/mac-security/using-the-software-update-service-on-mountain-lion-server/) and bind your clients via OpenDirectory. Alternatively, use [Reposado](https://github.com/wdas/reposado) together with correct network DNS settings to make resolution transparent. [Margarita](https://github.com/jessepeterson/margarita) looks nice to have as well.

```
# Use own SUS
sudo defaults write /Library/Preferences/com.apple.SoftwareUpdate CatalogURL http://su.example.com:8088/index.sucatalog

# Reset to Apple SUS
sudo defaults delete /Library/Preferences/com.apple.SoftwareUpdate CatalogURL
```

### Software Version

#### Show System Software Version

There are several ways to obtain different levels of detail.

```
sw_vers -productVersion
system_profiler SPSoftwareDataType
defaults read loginwindow SystemVersionStampAsString
```

### Spotlight

#### Spotlight Indexing

```
# Disable
mdutil -i off -d /path/to/volume

# Enable (Default)
mdutil -i on /path/to/volume
```

#### Erase Spotlight Index and Rebuild

```
mdutil -E /path/to/volume
```

#### Search via Spotlight

```
mdfind -name 'searchterm'
```

#### Show Spotlight Indexed Metadata

```
mdls /path/to/file
```

### System Integrity Protection

Reboot while holding Cmd + R and open the Terminal application. You will need to `reboot` for the commands to take effect.

```
# Status
csrutil status

# Enable (Default)
csrutil enable

# Disable
csrutil disable
```

### Date and Time

#### List Available Timezones

```
sudo systemsetup -listtimezones
```

#### Set Timezone

```
sudo systemsetup -settimezone Europe/Berlin
```

#### Set Clock Using Network Time

```
# Status
sudo systemsetup getusingnetworktime

# Enable (Default)
sudo systemsetup setusingnetworktime on

# Disable
sudo systemsetup setusingnetworktime off
```

#### Set Menu Bar Clock Output Format

```
# System Preferences > Date & Time > Time options
# Analogue
sudo defaults write com.apple.menuextra.clock IsAnalog -bool true
# Digital (Default)
sudo defaults write com.apple.menuextra.clock IsAnalog -bool false

# System Preferences > Date & Time > Flash the time separators
# Enable
sudo defaults write com.apple.menuextra.clock FlashDateSeparators -bool true
# Disable (Default)
sudo defaults write com.apple.menuextra.clock FlashDateSeparators -bool false

# Thu 18 Aug 23:46:18
# System Preferences > Date & Time > Display time with seconds - Checked [:ss]
# System Preferences > Date & Time > Use a 24-hour clock - Checked [HH:mm]
# System Preferences > Date & Time > Show AM/PM - Unchecked
# System Preferences > Date & Time > Show the day of the week - Checked [EEE]
# System Preferences > Date & Time > Show date - Checked [d MMM]
sudo defaults write com.apple.menuextra.clock DateFormat -string "EEE d MMM HH:mm:ss"

# Thu 23:46:18
# System Preferences > Date & Time > Display time with seconds - Checked [:ss]
# System Preferences > Date & Time > Use a 24-hour clock - Checked [HH:mm]
# System Preferences > Date & Time > Show AM/PM - Unchecked
# System Preferences > Date & Time > Show the day of the week - Checked [EEE]
# System Preferences > Date & Time > Show date - Unchecked
sudo defaults write com.apple.menuextra.clock DateFormat -string "EEE HH:mm:ss"

# 18 Aug 23:46:18
# System Preferences > Date & Time > Display time with seconds - Checked [:ss]
# System Preferences > Date & Time > Use a 24-hour clock - Checked [HH:mm]
# System Preferences > Date & Time > Show AM/PM - Unchecked
# System Preferences > Date & Time > Show the day of the week - Unchecked
# System Preferences > Date & Time > Show date - Checked [d MMM]
sudo defaults write com.apple.menuextra.clock DateFormat -string "d MMM HH:mm:ss"

# 23:46:18
# System Preferences > Date & Time > Display time with seconds - Checked [:ss]
# System Preferences > Date & Time > Use a 24-hour clock - Checked [HH:mm]
# System Preferences > Date & Time > Show AM/PM - Unchecked
# System Preferences > Date & Time > Show the day of the week - Unchecked
# System Preferences > Date & Time > Show date - Unchecked
sudo defaults write com.apple.menuextra.clock DateFormat -string "HH:mm:ss"

# Thu 18 Aug 11:46:18 pm
# System Preferences > Date & Time > Display time with seconds - Checked [:ss]
# System Preferences > Date & Time > Use a 24-hour clock - Unchecked
# System Preferences > Date & Time > Show AM/PM - Checked [a]
# System Preferences > Date & Time > Show the day of the week - Checked [EEE]
# System Preferences > Date & Time > Show date - Checked [d MMM]
sudo defaults write com.apple.menuextra.clock DateFormat -string "EEE d MMM h:mm:ss a"


# Thu 11:46:18 pm
# System Preferences > Date & Time > Display time with seconds - Checked [:ss]
# System Preferences > Date & Time > Use a 24-hour clock - Unchecked
# System Preferences > Date & Time > Show AM/PM - Checked [a]
# System Preferences > Date & Time > Show the day of the week - Checked [EEE]
# System Preferences > Date & Time > Show date - Unchecked
sudo defaults write com.apple.menuextra.clock DateFormat -string "EEE h:mm:ss a"

# 18 Aug 11:46:18 pm
# System Preferences > Date & Time > Display time with seconds - Checked [:ss]
# System Preferences > Date & Time > Use a 24-hour clock - Unchecked
# System Preferences > Date & Time > Show AM/PM - Checked [a]
# System Preferences > Date & Time > Show the day of the week - Unchecked
# System Preferences > Date & Time > Show date - Checked [d MMM]
sudo defaults write com.apple.menuextra.clock DateFormat -string "d MMM h:mm:ss a"

# 11:46:18 pm
# System Preferences > Date & Time > Display time with seconds - Checked [:ss]
# System Preferences > Date & Time > Use a 24-hour clock - Unchecked
# System Preferences > Date & Time > Show AM/PM - Checked [a]
# System Preferences > Date & Time > Show the day of the week - Unchecked
# System Preferences > Date & Time > Show date - Unchecked
sudo defaults write com.apple.menuextra.clock DateFormat -string "h:mm:ss a"

# Thu 18 Aug 11:46:18
# System Preferences > Date & Time > Display time with seconds - Checked [:ss]
# System Preferences > Date & Time > Use a 24-hour clock - Unchecked
# System Preferences > Date & Time > Show AM/PM - Unchecked
# System Preferences > Date & Time > Show the day of the week - Checked [EEE]
# System Preferences > Date & Time > Show date - Checked [d MMM]
sudo defaults write com.apple.menuextra.clock DateFormat -string "EEE d MMM h:mm:ss"

# Thu 11:46:18
# System Preferences > Date & Time > Display time with seconds - Checked [:ss]
# System Preferences > Date & Time > Use a 24-hour clock - Unchecked
# System Preferences > Date & Time > Show AM/PM - Unchecked
# System Preferences > Date & Time > Show the day of the week - Checked [EEE]
# System Preferences > Date & Time > Show date - Unchecked
sudo defaults write com.apple.menuextra.clock DateFormat -string "EEE h:mm:ss"

# 18 Aug 11:46:18
# System Preferences > Date & Time > Display time with seconds - Checked [:ss]
# System Preferences > Date & Time > Use a 24-hour clock - Unchecked
# System Preferences > Date & Time > Show AM/PM - Unchecked
# System Preferences > Date & Time > Show the day of the week - Unchecked
# System Preferences > Date & Time > Show date - Checked [d MMM]
sudo defaults write com.apple.menuextra.clock DateFormat -string "d MMM h:mm:ss"

# 11:46:18
# System Preferences > Date & Time > Display time with seconds - Checked [:ss]
# System Preferences > Date & Time > Use a 24-hour clock - Unchecked
# System Preferences > Date & Time > Show AM/PM - Unchecked
# System Preferences > Date & Time > Show the day of the week - Unchecked
# System Preferences > Date & Time > Show date - Unchecked
sudo defaults write com.apple.menuextra.clock DateFormat -string "h:mm:ss"

# Thu 18 Aug 23:46
# System Preferences > Date & Time > Display time with seconds - Unchecked
# System Preferences > Date & Time > Use a 24-hour clock - Checked [HH:mm]
# System Preferences > Date & Time > Show AM/PM - Unchecked
# System Preferences > Date & Time > Show the day of the week - Checked [EEE]
# System Preferences > Date & Time > Show date - Checked [d MMM]
sudo defaults write com.apple.menuextra.clock DateFormat -string "EEE d MMM HH:mm"

# Thu 23:46
# System Preferences > Date & Time > Display time with seconds - Unchecked
# System Preferences > Date & Time > Use a 24-hour clock - Checked [HH:mm]
# System Preferences > Date & Time > Show AM/PM - Unchecked
# System Preferences > Date & Time > Show the day of the week - Checked [EEE]
# System Preferences > Date & Time > Show date - Unchecked
sudo defaults write com.apple.menuextra.clock DateFormat -string "EEE HH:mm"

# 18 Aug 23:46
# System Preferences > Date & Time > Display time with seconds - Unchecked
# System Preferences > Date & Time > Use a 24-hour clock - Checked [HH:mm]
# System Preferences > Date & Time > Show AM/PM - Unchecked
# System Preferences > Date & Time > Show the day of the week - Unchecked
# System Preferences > Date & Time > Show date - Checked [d MMM]
sudo defaults write com.apple.menuextra.clock DateFormat -string "d MMM HH:mm"

# 23:46
# System Preferences > Date & Time > Display time with seconds - Unchecked
# System Preferences > Date & Time > Use a 24-hour clock - Checked [HH:mm]
# System Preferences > Date & Time > Show AM/PM - Unchecked
# System Preferences > Date & Time > Show the day of the week - Unchecked
# System Preferences > Date & Time > Show date - Unchecked
sudo defaults write com.apple.menuextra.clock DateFormat -string "HH:mm"

# Thu 18 Aug 11:46 pm
# System Preferences > Date & Time > Display time with seconds - Unchecked
# System Preferences > Date & Time > Use a 24-hour clock - Unchecked
# System Preferences > Date & Time > Show AM/PM - Checked [a]
# System Preferences > Date & Time > Show the day of the week - Checked [EEE]
# System Preferences > Date & Time > Show date - Checked [d MMM]
sudo defaults write com.apple.menuextra.clock DateFormat -string "EEE d MMM h:mm a"

# Thu 11:46 pm
# System Preferences > Date & Time > Display time with seconds - Unchecked
# System Preferences > Date & Time > Use a 24-hour clock - Unchecked
# System Preferences > Date & Time > Show AM/PM - Checked [a]
# System Preferences > Date & Time > Show the day of the week - Checked [EEE]
# System Preferences > Date & Time > Show date - Unchecked
sudo defaults write com.apple.menuextra.clock DateFormat -string "EEE h:mm a"

# 18 Aug 11:46 pm
# System Preferences > Date & Time > Display time with seconds - Unchecked
# System Preferences > Date & Time > Use a 24-hour clock - Unchecked
# System Preferences > Date & Time > Show AM/PM - Checked [a]
# System Preferences > Date & Time > Show the day of the week - Unchecked
# System Preferences > Date & Time > Show date - Checked [d MMM]
sudo defaults write com.apple.menuextra.clock DateFormat -string "d MMM h:mm a"

# 11:46 pm
# System Preferences > Date & Time > Display time with seconds - Unchecked
# System Preferences > Date & Time > Use a 24-hour clock - Unchecked
# System Preferences > Date & Time > Show AM/PM - Checked [a]
# System Preferences > Date & Time > Show the day of the week - Unchecked
# System Preferences > Date & Time > Show date - Unchecked
sudo defaults write com.apple.menuextra.clock DateFormat -string "h:mm a"

# Thu 18 Aug 11:46
# System Preferences > Date & Time > Display time with seconds - Unchecked
# System Preferences > Date & Time > Use a 24-hour clock - Unchecked
# System Preferences > Date & Time > Show AM/PM - Unchecked
# System Preferences > Date & Time > Show the day of the week - Checked [EEE]
# System Preferences > Date & Time > Show date - Checked [d MMM]
sudo defaults write com.apple.menuextra.clock DateFormat -string "EEE d MMM h:mm"

# Thu 11:46
# System Preferences > Date & Time > Display time with seconds - Unchecked
# System Preferences > Date & Time > Use a 24-hour clock - Unchecked
# System Preferences > Date & Time > Show AM/PM - Unchecked
# System Preferences > Date & Time > Show the day of the week - Checked [EEE]
# System Preferences > Date & Time > Show date - Unchecked
sudo defaults write com.apple.menuextra.clock DateFormat -string "EEE h:mm"

# 18 Aug 11:46
# System Preferences > Date & Time > Display time with seconds - Unchecked
# System Preferences > Date & Time > Use a 24-hour clock - Unchecked
# System Preferences > Date & Time > Show AM/PM - Unchecked
# System Preferences > Date & Time > Show the day of the week - Unchecked
# System Preferences > Date & Time > Show date - Checked [d MMM]
sudo defaults write com.apple.menuextra.clock DateFormat -string "d MMM h:mm"

# 11:46
# System Preferences > Date & Time > Display time with seconds - Unchecked
# System Preferences > Date & Time > Use a 24-hour clock - Unchecked
# System Preferences > Date & Time > Show AM/PM - Unchecked
# System Preferences > Date & Time > Show the day of the week - Unchecked
# System Preferences > Date & Time > Show date - Unchecked
sudo defaults write com.apple.menuextra.clock DateFormat -string "h:mm"

# Apply changes immediately
sudo killall SystemUIServer
```

([Source](https://github.com/tech-otaku/macos-defaults/blob/master/date-time.sh))

## Terminal

#### Ring Terminal Bell

Rings the terminal bell (if enabled) and puts a badge on it.

```
tput bel
```

### Alternative Terminals

- [Alacritty](https://github.com/jwilm/alacritty) - Cross-platform, GPU-accelerated terminal emulator.
- [iTerm2](https://iterm2.com/) - A better Terminal.app.
- [kitty](https://sw.kovidgoyal.net/kitty/) - Modern, GPU-accelerated terminal emulator.

### Shells

#### Bash

Install the latest version and set as current user's default shell:

```
brew install bash && \
echo $(brew --prefix)/bin/bash | sudo tee -a /etc/shells && \
chsh -s $(brew --prefix)/bin/bash
```

- [Homepage](https://www.gnu.org/software/bash/) - The default shell for OS X and most other Unix-based operating systems.
- [Bash-it](https://github.com/Bash-it/bash-it) - Community Bash framework, like Oh My Zsh for Bash.

#### fish

Install the latest version and set as current user's default shell:

```
brew install fish && \
echo $(brew --prefix)/bin/fish | sudo tee -a /etc/shells && \
chsh -s $(brew --prefix)/bin/fish
```

- [Homepage](http://fishshell.com/) - A smart and user-friendly command line shell for OS X, Linux, and the rest of the family.
- [The Fishshell Framework](https://github.com/oh-my-fish/oh-my-fish) - Provides core infrastructure to allow you to install packages which extend or modify the look of your shell.
- [Installation & Configuration Tutorial](https://github.com/ellerbrock/fish-shell-setup-osx) - How to Setup Fish Shell with Fisherman, Powerline Fonts, iTerm2 and Budspencer Theme on OS X.

#### Zsh

Install the latest version and set as current user's default shell:

```
brew install zsh && \
sudo sh -c 'echo $(brew --prefix)/bin/zsh >> /etc/shells' && \
chsh -s $(brew --prefix)/bin/zsh
```

- [Homepage](http://www.zsh.org/) - Zsh is a shell designed for interactive use, although it is also a powerful scripting language.
- [Oh My Zsh](http://ohmyz.sh/) - An open source, community-driven framework for managing your Zsh configuration.
- [Prezto](https://github.com/sorin-ionescu/prezto) - A speedy Zsh framework. Enriches the command line interface environment with sane defaults, aliases, functions, auto completion, and prompt themes.
- [zgen](https://github.com/tarjoilija/zgen) - Another open source framework for managing your zsh configuration. Zgen will load oh-my-zsh compatible plugins and themes and has the advantage of both being faster and automatically cloning any plugins used in your configuration for you.

### Terminal Fonts

- [Anonymous Pro](http://www.marksimonson.com/fonts/view/anonymous-pro) - A family of four fixed-width fonts designed with coding in mind.
- [Codeface](https://github.com/chrissimpkins/codeface) - A gallery and repository of monospaced fonts for developers.
- [DejaVu Sans Mono](https://dejavu-fonts.github.io/) - A font family based on the Vera Fonts.
- [Fantasque Sans Mono](https://github.com/belluzj/fantasque-sans) - Designed with functionality in mind, and with some wibbly-wobbly handwriting-like fuzziness that makes it unassumingly cool.
- [Hack](http://sourcefoundry.org/hack/) - Hack is hand groomed and optically balanced to be your go-to code face.
- [Inconsolata](http://levien.com/type/myfonts/inconsolata.html) - A monospace font, designed for code listings and the like.
- [Input](http://input.fontbureau.com/) - A flexible system of fonts designed specifically for code.
- [Meslo](https://github.com/andreberg/Meslo-Font) - Customized version of Apple's Menlo font.
- [Operator Mono](https://www.typography.com/fonts/operator/overview/) - A surprisingly usable alternative take on a monospace font (commercial).
- [Powerline Fonts](https://github.com/powerline/fonts) - Repo of patched fonts for the Powerline plugin.
- [Source Code Pro](https://adobe-fonts.github.io/source-code-pro/) - A monospaced font family for user interfaces and coding environments.

## Glossary

### Mac OS X, OS X, and macOS Version Information

| Version                    | Codename           | Release Date       | Most Recent Version                    |
| -------------------------- | ------------------ | ------------------ | -------------------------------------- |
| Rhapsody Developer Release | Grail1Z4 / Titan1U | August 31, 1997    | DR2 (May 14, 1998)                     |
| Mac OS X Server 1.0        | Hera               | March 16, 1999     | 1.2v3 (October 27, 2000)               |
| Mac OS X Developer Preview | n/a                | March 16, 1999     | DP4 (April 5, 2000)                    |
| Mac OS X Public Beta       | Kodiak             | September 13, 2000 | n/a                                    |
| Mac OS X 10.0              | Cheetah            | March 24, 2001     | 10.0.4 (June 22, 2001)                 |
| Mac OS X 10.1              | Puma               | September 25, 2001 | 10.1.5 (June 6, 2002)                  |
| Mac OS X 10.2              | Jaguar             | August 24, 2002    | 10.2.8 (October 3, 2003)               |
| Mac OS X 10.3              | Panther            | October 24, 2003   | 10.3.9 (April 15, 2005)                |
| Mac OS X 10.4              | Tiger              | April 29, 2005     | 10.4.11 (November 14, 2007)            |
| Mac OS X 10.5              | Leopard            | October 26, 2007   | 10.5.8 (August 5, 2009)                |
| Mac OS X 10.6              | Snow Leopard       | August 28, 2009    | 10.6.8 v1.1 (July 25, 2011)            |
| Mac OS X 10.7              | Lion               | July 20, 2011      | 10.7.5 (September 19, 2012)            |
| OS X 10.8                  | Mountain Lion      | July 25, 2012      | 10.8.5 (12F2560) (August 13, 2015)     |
| OS X 10.9                  | Mavericks          | October 22, 2013   | 10.9.5 (13F1911) (July 18, 2016)       |
| OS X 10.10                 | Yosemite           | October 16, 2014   | 10.10.5 (14F2511) (July 19, 2017)      |
| OS X 10.11                 | El Capitan         | September 30, 2015 | 10.11.6 (15G22010) (July 9, 2018)      |
| macOS 10.12                | Sierra             | September 20, 2016 | 10.12.6 (16G2136) (September 26, 2019) |
| macOS 10.13                | High Sierra        | September 25, 2017 | 10.13.6 (17G14042) (November 12, 2020) |
| macOS 10.14                | Mojave             | September 24, 2018 | 10.14.6 (18G9216) (May 24, 2021)       |
| macOS 10.15                | Catalina           | October 7, 2019    | 10.15.7 (19H1217) (May 24, 2021)       |
| macOS 11                   | Big Sur            | November 12, 2020  | 11.5.1 (20G80) (July 26, 2021)         |

## License

[![Creative Commons License](https://camo.githubusercontent.com/8d4f8448276174df87a635f37c9cf5abe8e04037c960c64fc47f25bdef498856/68747470733a2f2f6c6963656e7365627574746f6e732e6e65742f6c2f62792d73612f342e302f38387833312e706e67)](https://creativecommons.org/licenses/by-sa/4.0/)
This work is licensed under a [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/).
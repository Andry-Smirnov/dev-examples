#!/bin/bash
set -e

if ! [ ${OSTYPE:0:6} = "darwin" ]; then
  echo " This script is for OS X only"
  exit 1
fi

appname=
appversion=
pkgversion=

appnamenospaces=${appname//[[:blank:]]/}
dskimage="$appnamenospaces"_$appversion-$pkgversion
appbundle="$appname.app"

# Create a temporary disk image (32 MB).
hdiutil create -size 32m -fs HFS+ -volname $dskimage temp.dmg

# Mount the disk image.
hdiutil attach temp.dmg

# Obtain device information.
DEVS=$(hdiutil attach temp.dmg | cut -f 1)
DEV=$(echo $DEVS | cut -f 1 -d ' ')

# Copy app bundle to the disk image.
sudo cp -R -p -X "$appbundle" /Volumes/$dskimage

# Unmount the disk image.
hdiutil detach $DEV

# Convert the disk image to read-only.
hdiutil convert temp.dmg -format UDZO -o $dskimage.dmg

# Delete the temporary disk image.
rm temp.dmg

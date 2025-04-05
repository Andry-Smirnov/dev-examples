#!/bin/bash
set -e

if [ ${OSTYPE:0:6} = "darwin" ]; then
  echo " This script is for Linux only"
  exit 1
fi

appname=
appversion=
pkgversion=
exefile=

# Note: See commented out mime lines below for associating
#  a file type with app.
#extension=

debfile="$exefile"_$appversion-"$pkgversion"_amd64.deb
bindir=usr/share/$exefile/$appversion

if [ -e "debian/$bindir" ]; then
  echo debian/$bindir already exists
  exit 1
fi

mkdir -p "debian/$bindir"
cp -p "$exefile" "debian/$bindir"
cp -p "$exefile.version" "debian/$bindir"

exedir=usr/share/doc/$exefile
mkdir -p "debian/$exedir"
echo "For copyright information, see About box." >"debian/$exedir/copyright"

mkdir -p debian/DEBIAN
controlfile=debian/DEBIAN/control
echo "Package: $exefile" >$controlfile
echo "Version: $appversion-$pkgversion" >>$controlfile
#echo "Section: " >>$controlfile
echo "Priority: optional" >>$controlfile
echo "Architecture: amd64" >>$controlfile
echo "Depends: libgtk2.0-dev (>= 2.6.0)" >>$controlfile
echo "Installed-Size: 8000" >>$controlfile
#echo "Homepage: " >>$controlfile
#echo "Maintainer: " >>$controlfile
echo "Description: $appname app" >>$controlfile

mkdir -p debian/usr/share/pixmaps
mkdir -p debian/usr/share/applications
#mkdir -p debian/usr/share/mime/packages
mkdir -p debian/usr/bin
install -p -m 644 "$exefile.png" "debian/usr/share/pixmaps/$exefile.png"

echo "[Desktop Entry]" >temp.desktop
echo "Name=$appname" >>temp.desktop
echo "Comment=$appname app" >>temp.desktop
echo "Exec=$exefile %f" >>temp.desktop
echo "Terminal=false" >>temp.desktop
echo "Type=Application" >>temp.desktop
echo "Icon=$exefile" >>temp.desktop
echo "StartupWMClass=$appname" >>temp.desktop
#echo "MimeType=text/$exfile-data;" >>temp.desktop
install -p -m 644 temp.desktop "debian/usr/share/applications/$exefile.desktop"
rm temp.desktop

#echo '<?xml version="1.0" encoding="utf-8"?>' >temp-mime.xml
#echo '<mime-info xmlns="http://www.freedesktop.org/standards/shared-mime-info">' >>temp-mime.xml
#echo '  <mime-type type="text/$exefile-data">' >>temp-mime.xml
#echo '    <sub-class-of type="text/plain"/>' >>temp-mime.xml
#echo '    <alias type="text/$exefile-data"/>' >>temp-mime.xml
#echo '    <comment>$appname data file</comment>' >>temp-mime.xml
#echo '    <acronym>$appname</acronym>' >>temp-mime.xml
#echo '    <expanded-acronym>$appname data</expanded-acronym>' >>temp-mime.xml
#echo '    <glob pattern="*.$extension"/>' >>temp-mime.xml
#echo '  </mime-type>' >>temp-mime.xml
#echo '</mime-info>' >>temp-mime.xml
#install -p -m 644 temp-mime.xml "debian/usr/share/mime/packages/$exefile.xml"
#rm temp-mime.xml

ln -s "/$bindir/$exefile" "debian/usr/bin/$exefile"

find ./debian -type d | xargs chmod 755

fakeroot dpkg-deb --build debian
mv debian.deb "$debfile"

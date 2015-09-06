#!/bin/sh

# Download latest version of `pandoc`
url_latest_release="https://github.com/jgm/pandoc/releases/latest"
package_path="$( \
  # find out real url
  curl --silent --show-error --fail --head "https://github.com/jgm/pandoc/releases/latest" | \
  grep "Location: " | cut -d" " -f2 | sed -e 's/\r//g' -e 's/\n//g' | \
  
  # get and parse page
  xargs curl --silent --show-error --fail | \
  grep -oP "(?<=(href=\"))/[^>\"]+/releases/download/[^>\"]+-amd64.deb(?=\")" | \
  
  # get first result in case more than one file is available
  head -n1 \
)"

if [ -z "${package_path}" ]; then
  echo "* [ERROR]: Unable to determine latest Pandoc package" >&2
  exit 1
fi

set -e
echo "Downloading: https://github.com${package_path}"
curl --location --output "pandoc-latest-amd64.deb" "https://github.com${package_path}" 
ar -x pandoc-latest-amd64.deb
tar xzf data.tar.gz
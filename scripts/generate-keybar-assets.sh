#!/usr/bin/env bash
# Regenerate the tracked Android keybar PNGs without changing label proportions.
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
out="${1:-$script_dir/../assets/android-keybar}"
font="${2:-${KEYBAR_FONT:-$HOME/Library/Fonts/JetBrainsMonoNL-Regular.ttf}}"

command -v magick >/dev/null || {
  echo 'ImageMagick (magick) is required.' >&2
  exit 1
}
[[ -f "$font" ]] || {
  echo 'Pass the JetBrainsMonoNL-Regular.ttf path as argument 2 or KEYBAR_FONT.' >&2
  exit 1
}

labels=(esc tab ctrl shift meta alt super mvmt)
texts=(Esc Tab Ctrl Shift Meta Alt Super Mvmt)

for theme in light dark; do
  for state in idle armed locked; do
    case "$theme/$state" in
      light/idle)   bg='#e2e2e2'; fg='#383a42' ;;
      light/armed)  bg='#3f78b5'; fg='#ffffff' ;;
      light/locked) bg='#000000'; fg='#ffffff' ;;
      dark/idle)    bg='#1e2126'; fg='#bbc2cf' ;;
      dark/armed)   bg='#96cdfb'; fg='#000000' ;;
      dark/locked)  bg='#ffffff'; fg='#000000' ;;
    esac
    mkdir -p "$out/$theme/$state"
    for i in "${!labels[@]}"; do
      # 102px is 1.5 times the former 68px canvas. Increase the painted block
      # from 59px to 89px, but retain its width and the original 37px font.
      # Lisp scales both axes uniformly to fit the complete row on screen.
      magick -size 152x102 xc:none \
        -fill "$bg" -stroke none -draw 'rectangle 3,7 149,95' \
        -font "$font" -pointsize 37 -gravity center \
        -fill "$fg" -stroke none -annotate +0+1 "${texts[$i]}" \
        -depth 8 -strip "$out/$theme/$state/${labels[$i]}.png"
    done
  done
done

# Keep Esc clear of rounded screen edges without changing any button width.
magick -size 18x102 xc:none -depth 8 -strip "$out/spacer.png"

for theme in light dark; do
  if [[ "$theme" == light ]]; then
    bg='#e2e2e2'; fg='#383a42'
  else
    bg='#1e2126'; fg='#bbc2cf'
  fi
  magick -size 92x102 xc:none \
    -fill "$bg" -stroke none -draw 'rectangle 3,7 89,95' \
    -font "$font" -pointsize 42 -gravity center \
    -fill "$fg" -annotate +0+2 '⌄' \
    -depth 8 -strip "$out/$theme/collapse.png"
done

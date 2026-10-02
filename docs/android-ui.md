# Android dashboard and keyboard row

These modules are guarded by `system-type` and do not alter the desktop UI.

## Keyboard row

`+android-keybar.el` follows native **outer-frame** resizing when a docked
keyboard opens or closes. Both native toolbar modes are disabled when hidden,
so no empty row remains. Manual Hide stays in effect until the keyboard is
closed and opened again; another request to show an already-open keyboard does
not override it. Reshowing reinstalls the input decoders without clearing locks.

The Galaxy row is approximately 190 px tall (previously 126 px). The source PNG
canvases are 102 px tall instead of 68 px, and their painted blocks are 89 px
tall instead of 59 px: the visible buttons, not just their hit targets, are
approximately 1.5× taller. The row-width fit produces 80 px images on this phone;
55 px of extra touch padding above and below preserves the 190 px row height.
Labels retain the same font size and proportions. Esc, literal Tab, Shift+Tab,
and Mvmt/Enter behavior are unchanged.

The PNG assets are tracked. To regenerate them with ImageMagick, run:

```sh
bash scripts/generate-keybar-assets.sh assets/android-keybar /path/to/JetBrainsMonoNL-Regular.ttf
```

The generator preserves every button width, the 37 px label font, flat edges,
and existing light/dark idle, blue armed, and black/white locked colors. It does
not stretch an existing image vertically.

Emacs 31 does not expose Android IME visibility directly to Lisp. The resize
signal works for docked keyboards in the normal full-screen phone layout.
Floating keyboards do not resize the frame, and split-screen resizing can
resemble a keyboard transition; these cases need native IME-insets support for
reliable detection. No background shell polling is used.

## Home screen

`+android-dashboard.el` keeps the two-line wordmark and adds up to five saved
Emacs bookmarks as flat text buttons. “Recent” means most recently created or
edited (`last-modified`), not most recently visited. Older records without
timestamps retain their existing creation order. No bookmark records are
created or changed by the dashboard. With no bookmarks, only the wordmark is
shown. Use the normal Emacs bookmark commands to add entries.

Buttons inherit the current font and theme colors. Their full names remain
available to the bookmark handler even when a long visible label is shortened.
The complete group is centered using its rendered pixel height, accounting for
text scaling and the space left by the keyboard. The dashboard modeline is
hidden locally, including when Doom reapplies its own modeline after startup.

Three blank lines separate the wordmark from the bookmarks (customizable with
`my/android-dashboard-bookmark-gap`). Centering uses ordinary blank lines rather
than an oversized display-space glyph: the latter misaligned Android's touch
coordinates at reduced text scale. Each block forwards its actual touch event
to Emacs's button handler, so it opens the touched bookmark rather than the one
at the old cursor position. Background taps show the keyboard on release;
bookmark taps navigate without opening the keyboard mid-gesture.

The dashboard resize hook is also confined to the dashboard buffer. Doom's
point-restoration code must not move an unrelated editing buffer to the saved
dashboard position when a keyboard resize occurs.

## Initial syntax highlighting

`+android.el` restores `redisplay-skip-fontification-on-input` to Emacs's default
of `nil`. Doom enables this optimization, but an Android IME action can leave
a newly displayed buffer unfontified until another command. This can make Org
look partly initialized: raw stars/links and ordinary-weight headings become
styled only after cursor movement, even though `org-mode` was already active.

The Galaxy test reproduced this with Samsung keyboard's Done action and a fresh
synthetic Org buffer. With the override, highlighting appeared on initial
display; a real Org file opened through `SPC SPC` did too. The change is Android
only. It does not eagerly fontify entire files, add timers, change themes, or
alter `fast-but-imprecise-scrolling` / `jit-lock-defer-time`.

## Regression checks

```sh
emacs --batch -Q -l tests/android-keybar-test.el
emacs --batch -Q -l tests/android-dashboard-test.el
emacs --batch -Q -l tests/android-config-test.el
```

These suites also run with native Android Emacs. Live acceptance checks cover
keyboard open/Back dismissal, complete row reclamation, manual Hide/reopen,
touches in the enlarged Esc target, bookmark touch activation, and a fresh
Emacs launch. Temporary bookmark previews are not saved as real bookmarks.

# Android PDF reader

The native Android Emacs app uses the same Doom `:tools pdf` / `pdf-tools`
reader as the desktop config. It needs a working `~/.local/bin/epdfinfo` built
against the phone's Termux libraries. Installing the Lisp package alone is
not sufficient. The Emacs/Termux apps must have compatible signing/shared UID.

## Touch hang fixed on the Galaxy

On Emacs 31.1, PDF-tools' desktop image hotspots route raw touchscreen events
into its mouse-event proxy. The proxy assumes a mouse position directly in
the event, but raw touch begin/end events wrap that position with a contact ID.
Touching PDF text could then hang in native key lookup/replay. Ctrl-G recovered
the hang; it was not an observed out-of-memory termination.

On the Galaxy, the same swipe hung with hotspots enabled and worked with
them disabled, including an on/off reversal. `+android-pdf.el` sets
`pdf-view-inhibit-hotspots` before the first page render, **on Android only**.
PDF link/annotation modes remain enabled for keyboard commands, but their
desktop click/hover targets are intentionally absent from the touch canvas.
The existing Android canvas gestures provide tap-to-keyboard, vertical swipes,
and pinch zoom. Desktop mouse behavior is unchanged.

The pinch cap is 4 PDF-point scale units (not 4 times the screen width), with
an additional 8-megapixel limit per rendered page. That bounds one decoded
RGBA page to 32 MiB; it is not a bound on total Emacs memory usage.

On Android, `pdf-view-roll-minor-mode` is enabled automatically when the installed
PDF-tools provides `pdf-roll` (the Galaxy does). This is the experimental,
genuinely stacked-page view: adjacent pages share one scrolling column. Touch
drags scroll by pixel distance, including slow drags. Pinch zoom retains its
original scale baseline if the fingers pause without lifting. Native images
are flushed when retired/replaced, while still-visible neighbors stay cached.
The Android adapter also limits roll's overlay lookup to actual PDF page/margin
overlays. Upstream's window-only lookup can otherwise overwrite a temporary
line highlight with an image, producing blank or duplicate pages on resize.
Search, outlines, keyboard navigation and midnight mode remain PDF-tools.

`M-x pdf-view-roll-minor-mode` toggles back to the single-page view for the
current buffer. Older PDF-tools without `pdf-roll` retain that view; their
`pdf-view-continuous` option crosses page boundaries without stacking pages.
Desktop defaults are unchanged.

When zoomed wider than the screen, a one-finger drag also pans horizontally;
diagonal drags move both axes. Small horizontal motions accumulate rather than
being discarded (Emacs displays horizontal offsets in frame-character steps).
Panning is bounded to the page and resets when zooming back to fit-width.

## Renderer installation

Install the build dependencies in the compatible Termux installation:

```sh
pkg install clang make autoconf automake libtool pkg-config poppler libpng zlib
```

Build the **checked-out PDF-tools version**, rather than an unrelated newer
release. On Android 15+, starting its `autobuild.android` script directly may
fail with `Operation not permitted`. Run the build through Termux Bash with
Termux's execution shim enabled **only for the build process**. For example,
evaluate this from the running native Emacs (after Doom has installed PDF-tools):

```elisp
(let* ((build-dir (make-temp-file
                   "/data/data/com.termux/files/home/doom-epdfinfo-" t))
       (destination (expand-file-name "~/.local/bin/"))
       (process-environment (copy-sequence process-environment)))
  (copy-directory
   (expand-file-name "~/.emacs.d/.local/straight/repos/pdf-tools/server/")
   build-dir nil t t)
  (make-directory destination t)
  (setenv "LD_PRELOAD"
          "/data/data/com.termux/files/usr/lib/libtermux-exec.so")
  (let ((default-directory build-dir))
    (make-process
     :name "epdfinfo-build" :buffer "*epdfinfo build*"
     :command (list "/data/data/com.termux/files/usr/bin/bash"
                    (expand-file-name "autobuild" build-dir)
                    "-D" "-i" destination)
     :noquery t)))
```

Wait for exit status 0, then run `M-x pdf-info-check-epdfinfo`. Do not set
`LD_PRELOAD` or Termux's `LD_LIBRARY_PATH` globally in the GUI Emacs environment.
The renderer is a device-specific installed binary, not a Git-tracked asset.

## Shared storage

Give Emacs Android's "All files access" permission for direct access to
`/storage/emulated/0/Download`, `Documents`, and other shared directories.
The Galaxy already had this grant; actual PDF and PNG opens are the useful
test, rather than the separate photo-picker permission flags.

Files opened from Android's Share/Open With flow can instead have `/content/`
virtual names. The existing `my/android-materialize-content-uri` copies PDFs
to a private real path before the external renderer reads them. An all-files
grant does not turn `/content/` into an ordinary subprocess-readable path.

## Checks

```sh
emacs --batch -Q -l tests/android-pdf-test.el
```

These checks cover desktop isolation, immediate/deferred PDF-tools loading,
roll availability, pixel scrolling, native image retirement, and pinch gesture
state/allocation limits. Device verification additionally needs real
taps, swipes, two-finger pinches, page changes and midnight toggles. Tests
cannot guarantee that every PDF or future Emacs build will behave identically.

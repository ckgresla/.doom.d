# Android CJK font fallback

The main fonts remain **JetBrains Mono NL** and **Inter Variable**.  The
Android-only `+android-fonts.el` supplies missing Chinese, Japanese, Korean,
Bopomofo and CJK punctuation glyphs using **Noto Sans CJK JP**.  The full
language-specific font has pan-CJK coverage; JP selects Japanese regional
forms for shared Han characters, not a Japanese-only character subset.

Native Emacs's `sfnt-android` backend requires TrueType outlines.  The Galaxy's
bundled Noto CJK collections are not available through this backend.  Use
the **TTF variable font**, not the OTF/OTC downloads.  Variable weights work
without installing separate regular and bold files.

## Install on a new Android device

Download these official upstream files into Emacs's `~/fonts/` directory
(on this Galaxy, `/data/data/org.gnu.emacs/files/fonts/`):

- [NotoSansCJKjp-VF.ttf](https://raw.githubusercontent.com/notofonts/noto-cjk/main/Sans/Variable/TTF/NotoSansCJKjp-VF.ttf)
- [SIL Open Font License](https://raw.githubusercontent.com/notofonts/noto-cjk/main/Sans/LICENSE)
  (save as `NotoSansCJK-LICENSE.txt`).

The font tested on 2026-09-17 is 36,174,296 bytes, SHA-256:

```text
240c9b83bf7b386edbae39995ae7e068ed4583f484d92e4a74c34158b5f27b1a
```

The upstream `main` download can change; a different checksum indicates a
different revision and should be checked against upstream before use.

Restart Emacs fully after adding the font: Android discovers user fonts at
startup.  No root access, APK change, or system-wide font replacement is
needed.  The font binary and license live on the device, not in this config
repository.  Without the font installed, the config safely leaves fallback
selection unchanged.

Test in both regular and variable-pitch text: `中文 漢字 日本語 かな カナ 한글
「引号」 “quotes” ㄅㄆㄇ`.  ASCII should retain its original main font.

Config regression tests (desktop isolation, missing-font safety and
idempotent CJK-only registration):

```sh
emacs --batch -Q -l tests/android-fonts-test.el
```

# Life sync

`SPC g S` (`M-x my/life-sync`) works only inside `~/life`, on Mac and Android.
It prompts to save modified Life buffers, then runs asynchronously:

1. Refuse an existing staged index, detached HEAD, or unfinished Git operation.
2. Fetch `origin`. Use the current upstream, or `origin/<branch>` if unset.
3. Commit tracked changes as `org: sync galaxy @ <timestamp>` on Android,
   or `org: sync hackbook @ <timestamp>` on Mac, including the time zone.
4. Rebase onto the fetched branch. On conflicts, abort: fetched refs and the
   local sync commit remain intact. No automatic conflict resolution or stash.
5. Push, without force, after a successful rebase. A push failure leaves local
   commits intact and reports the sync as stopped; no automatic retry or force.

The default pushes and excludes untracked files. Set `my/life-sync-push` to
`nil` to keep checkpoints local instead. `my/life-sync-include-new` enables an
explicit confirmation before including new non-ignored files. Git output is
in `*Life sync*`. Do not edit the repository while it is syncing. If a commit
hook fails, Git's staged changes remain available to inspect and retry.

Tests use disposable local repositories; they do not contact GitHub or touch
the real Life checkout:

```sh
emacs --batch -Q -l tests/life-sync-test.el
```

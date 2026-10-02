#!/bin/sh
# Invoked by my/life-sync. No stash, forced push, or automatic conflict resolution.
set -eu
root=${1:?repository required}
device=${2:?device label required}
push_after=${3:-no}
include_new=${4:-no}
case "$device" in galaxy|hackbook) ;; *) exit 2 ;; esac
cd "$root"
export GIT_TERMINAL_PROMPT=0 GIT_EDITOR=true GIT_SEQUENCE_EDITOR=true
branch=$(git symbolic-ref --quiet --short HEAD) || {
    echo 'Sync refused: detached HEAD.' >&2; exit 1;
}
for state in rebase-merge rebase-apply MERGE_HEAD CHERRY_PICK_HEAD REVERT_HEAD BISECT_LOG; do
    if [ -e "$(git rev-parse --git-path "$state")" ]; then
        echo "Sync refused: unfinished Git operation ($state)." >&2; exit 1
    fi
done
git diff --cached --quiet || {
    echo 'Sync refused: staged changes already exist. Commit/unstage them first.' >&2; exit 1;
}
lock=$(git rev-parse --git-path life-sync.lock)
mkdir "$lock" || { echo 'Another Life sync is running.' >&2; exit 1; }
trap 'rmdir "$lock"' EXIT
trap 'exit 130' INT
trap 'exit 143' TERM HUP
upstream=$(git rev-parse --abbrev-ref --symbolic-full-name '@{upstream}' 2>/dev/null || true)
case "$upstream" in
    '') upstream="origin/$branch" ;;
    origin/*) ;;
    *) echo 'Sync refused: upstream is not on origin.' >&2; exit 1 ;;
esac
git fetch origin
git rev-parse --verify "$upstream^{commit}" >/dev/null || {
    echo "Fetched origin, but $upstream does not exist; nothing committed." >&2; exit 1;
}
if [ "$include_new" = yes ]; then git add --all; else git add --update; fi
if ! git diff --cached --quiet; then
    git commit -m "org: sync $device @ $(date '+%Y-%m-%d %H:%M:%S %z')"
fi
# Never rewrite published local merge structure implicitly.
if ! git -c rebase.autoStash=false rebase --rebase-merges "$upstream"; then
    if [ -d "$(git rev-parse --git-path rebase-merge)" ] ||
       [ -d "$(git rev-parse --git-path rebase-apply)" ]; then
        git rebase --abort || {
            echo 'Abort failed; resolve the rebase manually. No push attempted.' >&2; exit 1;
        }
    fi
    echo 'Rebase failed and was canceled. Fetched refs and your local sync commit remain; no push.' >&2
    exit 1
fi
if [ "$push_after" = yes ]; then
    git push origin "HEAD:refs/heads/${upstream#origin/}"
fi
echo 'Life sync complete.'

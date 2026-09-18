#!/usr/bin/env bash
# Regression: an unchanged TSE snapshot must still be replaced after code changes.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
REPO="$TMP/repo"
FAKE_BIN="$TMP/bin"
TRACE="$TMP/trace"
mkdir -p "$REPO/logs" "$FAKE_BIN"
cp "$ROOT/daily_etl.sh" "$REPO/daily_etl.sh"
chmod +x "$REPO/daily_etl.sh"

cat > "$FAKE_BIN/git" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
echo "git $*" >> "$TRACE"
if [[ "$1" == "-C" ]]; then shift 2; fi
case "$1" in
  pull) exit 0 ;;
  rev-parse) printf '%s\n' "$FAKE_CODE_REV" ;;
  *) exit 2 ;;
esac
EOF
cat > "$FAKE_BIN/python3" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
if [[ "$1" == "-c" ]]; then exit 0; fi
dd if=/dev/zero of="$3" bs=1048576 count=2 status=none
EOF
cat > "$FAKE_BIN/Rscript" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
echo "Rscript $*" >> "$TRACE"
if [[ "$1" == "-e" ]]; then
  if [[ "$2" == *"readRDS"* ]]; then printf '10|10.00'; else printf '%s' "$FAKE_CUR_SIG"; fi
  exit 0
fi
case "$1" in
  load_data_2026.R) touch e2026.rds ;;
  load_totais.R) touch totais_2026.rds ;;
  update_database.R|update_totais.R) : ;;
  *) exit 2 ;;
esac
EOF
cat > "$FAKE_BIN/sqlite3" <<'EOF'
#!/usr/bin/env bash
exit 0
EOF
chmod +x "$FAKE_BIN"/*

run_etl() {
  PATH="$FAKE_BIN:$PATH" TRACE="$TRACE" FAKE_CODE_REV="$1" FAKE_CUR_SIG='10|10.00' \
    OBS_DB_PATH="$REPO/obseleitoral.db" ETL_LOG_DIR="$REPO/logs" "$REPO/daily_etl.sh"
}

# Different code revision forces replace even though the row/value signature matches.
run_etl new-revision
grep -Fq "git -C $REPO pull --ff-only" "$TRACE"
grep -Fq "Rscript update_database.R e2026.rds 2026 --replace" "$TRACE"
[[ "$(cat "$REPO/logs/etl_last_successful_code")" == "new-revision" ]]

# Once the same revision succeeded, the unchanged snapshot is skipped.
: > "$TRACE"
run_etl new-revision
! grep -Fq "Rscript update_database.R" "$TRACE"

echo "PASS: daily ETL refreshes code and reprocesses classifier changes exactly once"

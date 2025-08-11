#!/usr/bin/env bash
# Minimal debug harness for Lox script tests.
set -u  # (no -e yet so we can see failures)
echo "DEBUG: starting harness"
echo "DEBUG: original PWD=$(pwd)"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
echo "DEBUG: SCRIPT_DIR=$SCRIPT_DIR"
CASES_DIR="$SCRIPT_DIR/cases"
echo "DEBUG: CASES_DIR=$CASES_DIR"

# Determine binary
if [ "${1:-}" != "" ]; then
  BIN="$1"
else
  BIN="$(cd "$SCRIPT_DIR/.." && pwd)/_build/default/src/main.exe"
fi
echo "DEBUG: BIN=$BIN"

if [ ! -x "$BIN" ]; then
  echo "ERROR: binary not executable: $BIN"
  exit 1
fi

echo "DEBUG: listing cases dir:"
ls -l "$CASES_DIR" || { echo "ERROR: cannot list $CASES_DIR"; exit 1; }

total=0
fail=0

for f in "$CASES_DIR"/*.lox; do
  [ -f "$f" ] || continue
  total=$((total+1))
  base="${f%.lox}"
  out_file="${base}.out"
  err_file="${base}.err"
  echo "DEBUG: RUN file=$(basename "$f") out?=$([ -f "$out_file" ] && echo 1 || echo 0) err?=$([ -f "$err_file" ] && echo 1 || echo 0)"

  if [ -f "$out_file" ] && [ -f "$err_file" ]; then
    echo "SKIP (both out+err) $f"
    continue
  fi

  if [ -f "$out_file" ]; then
    echo "DEBUG: positive test -> executing"
    stdout="$("$BIN" "$f" 2>stderr.txt || printf "__EXIT_CODE__$?")"
    rc=$?
    stderr="$(cat stderr.txt 2>/dev/null || true)"
    rm -f stderr.txt
    expected="$(cat "$out_file")"

    # Normalize one trailing newline
    stdout_norm="${stdout%$'\n'}"
    expected_norm="${expected%$'\n'}"

    if [ $rc -ne 0 ]; then
      echo "FAIL $f (exit $rc)"
      [ -n "$stderr" ] && echo "  stderr: $stderr"
      fail=$((fail+1))
    elif [ "$stdout_norm" != "$expected_norm" ]; then
      echo "FAIL $f mismatch"
      printf "  expected=%q\n" "$expected"
      printf "  got=%q\n" "$stdout"
      fail=$((fail+1))
    elif [ -n "$stderr" ]; then
      echo "FAIL $f unexpected stderr"
      printf "  stderr=%q\n" "$stderr"
      fail=$((fail+1))
    else
      echo "PASS $f"
    fi
  elif [ -f "$err_file" ]; then
    echo "DEBUG: negative test -> executing"
    combined="$("$BIN" "$f" 2>&1 || true)"
    rc=$?
    echo "DEBUG: negative test exit=$rc"
    if [ $rc -eq 0 ]; then
      echo "FAIL $f expected non-zero exit"
      fail=$((fail+1))
      continue
    fi
    missing=0
    while IFS= read -r needle || [ -n "$needle" ]; do
      [ -z "$needle" ] && continue
      if ! grep -F -q "$needle" <<<"$combined"; then
        echo "  missing substring: $needle"
        missing=1
      fi
    done < "$err_file"
    if [ $missing -eq 0 ]; then
      echo "PASS $f"
    else
      echo "FAIL $f (missing substrings)"
      fail=$((fail+1))
    fi
  else
    echo "SKIP (no expectations) $f"
  fi
done

echo "DEBUG: completed loop total=$total fail=$fail"
if [ $total -eq 0 ]; then
  echo "WARNING: no .lox files matched ($CASES_DIR/*.lox)"
fi
[ $fail -eq 0 ] || exit 1

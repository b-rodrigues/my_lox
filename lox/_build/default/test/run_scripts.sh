#!/usr/bin/env bash
# Smoke test harness for Lox scripts.
# Usage under Dune: (Dune supplies the binary path)
#   dune runtest
# Manual usage (from repo root or anywhere):
#   bash test/run_scripts.sh [_build/default/src/main.exe]
set -euo pipefail

# Resolve script dir and repo root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
CASES_DIR="${SCRIPT_DIR}/cases"

# Accept binary path from first arg (Dune passes it). If absent, try default path.
if [ "${1:-}" != "" ] && [[ "${1:-}" != -* ]]; then
  BIN="$1"
  shift || true
else
  BIN="${ROOT}/_build/default/src/main.exe"
fi

if [ ! -x "${BIN}" ]; then
  echo "[FATAL] Interpreter binary not found/executable at: ${BIN}"
  echo "Build it first with: dune build"
  exit 1
fi

TOTAL=0 PASS=0 FAIL=0 SKIP=0

color() {
  printf "\033[%sm%s\033[0m" "$1" "$2"
}

pad() { printf "%-40s" "$1"; }

shopt -s nullglob
for lox in "${CASES_DIR}"/*.lox; do
  base="${lox%.lox}"
  name="$(basename "${lox}")"
  out_file="${base}.out"
  err_file="${base}.err"
  ((TOTAL++))

  if [ -f "${out_file}" ] && [ -f "${err_file}" ]; then
    echo "$(color 33 [SKIP]) $(pad "${name}") both .out and .err present"
    ((SKIP++))
    continue
  fi

  if [ -f "${out_file}" ]; then
    # Positive test
    stdout="$("${BIN}" "${lox}" 2>stderr.txt || true)"
    rc=$?
    stderr="$(cat stderr.txt)"
    rm -f stderr.txt

    if [ $rc -ne 0 ]; then
      echo "$(color 31 [FAIL]) $(pad "${name}") unexpected non-zero exit (${rc})"
      if [ -n "${stderr}" ]; then
        echo "  STDERR:"
        sed 's/^/    /' <<<"${stderr}"
      fi
      ((FAIL++))
      continue
    fi

    expected="$(cat "${out_file}")"
    if [ "${stdout}" = "${expected}" ] && [ -z "${stderr}" ]; then
      echo "$(color 32 [PASS]) $(pad "${name}")"
      ((PASS++))
    else
      echo "$(color 31 [FAIL]) $(pad "${name}") output mismatch"
      echo "  Expected:"
      sed 's/^/    /' <<<"${expected}"
      echo "  Got (stdout):"
      sed 's/^/    /' <<<"${stdout}"
      if [ -n "${stderr}" ]; then
        echo "  Got (stderr):"
        sed 's/^/    /' <<<"${stderr}"
      fi
      ((FAIL++))
    fi
  elif [ -f "${err_file}" ]; then
    # Negative test
    combined="$("${BIN}" "${lox}" 2>&1 || true)"
    rc=$?
    if [ $rc -eq 0 ]; then
      echo "$(color 31 [FAIL]) $(pad "${name}") expected error (exit code 0)"
      echo "  Output:"
      sed 's/^/    /' <<<"${combined}"
      ((FAIL++))
      continue
    fi
    missing=0
    while IFS= read -r needle || [ -n "${needle}" ]; do
      [ -z "${needle}" ] && continue
      if ! grep -F -q "${needle}" <<<"${combined}"; then
        echo "$(color 31 [FAIL]) $(pad "${name}") missing substring: ${needle}"
        missing=1
      fi
    done < "${err_file}"
    if [ $missing -eq 0 ]; then
      echo "$(color 32 [PASS]) $(pad "${name}")"
      ((PASS++))
    else
      echo "  Full error output:"
      sed 's/^/    /' <<<"${combined}"
      ((FAIL++))
    fi
  else
    echo "$(color 33 [SKIP]) $(pad "${name}") no .out or .err"
    ((SKIP++))
  fi
done

echo "----------------------------------------------------------------"
echo "Scripts Summary: Total=${TOTAL} Pass=${PASS} Fail=${FAIL} Skip=${SKIP}"
[ ${FAIL} -eq 0 ] || exit 1

#!/bin/bash

APPDIR="$(cd "$(dirname "$0")" && pwd)"
cd "$APPDIR" || exit

# === Step 1: Try system PATH ===
if command -v Rscript &> /dev/null; then
    RSCRIPT="Rscript"
else
    # === Step 2: Try cached path ===
    if [[ -f "$APPDIR/r_path.txt" ]]; then
        RSCRIPT=$(cat "$APPDIR/r_path.txt")
        if [[ ! -x "$RSCRIPT" ]]; then
            echo "Cached Rscript path is invalid: $RSCRIPT"
            RSCRIPT=""
        fi
    fi

    # === Step 3: Ask user ===
    if [[ -z "$RSCRIPT" ]]; then
        read -p "Rscript not found. Please enter full path to Rscript (e.g., /usr/local/bin/Rscript): " RPATH
        if [[ ! -x "$RPATH" ]]; then
            echo "The path '$RPATH' is not valid or not executable. Exiting."
            exit 1
        fi
        RSCRIPT="$RPATH"
        echo "$RSCRIPT" > "$APPDIR/r_path.txt"
        echo "Path saved in r_path.txt for future use."
    fi
fi

# === Step 4: Launch Shiny App ===
"$RSCRIPT" app.R
#!/bin/bash

# Exit on error
set -e

# 1. Clean build

echo "=============================================="
echo "Cleaning old build files..."
echo "=============================================="
stack clean && stack build



# 2. Generate Haddock documentation
echo "=============================================="
echo "Generating haddock documentation..."
echo "=============================================="
stack haddock

# 3. Locate documentation
DOC_ROOT=$(stack path --local-doc-root)
# Find the project directory (handles version suffix like -0.1.0.0)
PROJECT_DOC_DIR=$(find "$DOC_ROOT" -maxdepth 1 -type d -name "haskell-project*" | head -n 1)

if [ -z "$PROJECT_DOC_DIR" ]; then
    echo "Error: Could not find project documentation in $DOC_ROOT"
    exit 1
fi

# 4. Copy to local 'haddock' folder
echo "Copying documentation from $PROJECT_DOC_DIR to local 'haddock' folder..."
if [ -d "haddock" ]; then
    rm -rf haddock
fi
mkdir -p haddock
cp -r "$PROJECT_DOC_DIR/"* haddock/

# 5. Open in browser
echo "Opening documentation in browser..."
if [[ "$OSTYPE" == "darwin"* ]]; then
    open haddock/index.html
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
    xdg-open haddock/index.html
else
    echo "Documentation generated in 'haddock/' folder."
    echo "Please open 'haddock/index.html' manually."
fi


# 6. 
echo "=============================================="
echo "Executing project..."
echo "=============================================="
stack exec haskell-project

echo "Done!"

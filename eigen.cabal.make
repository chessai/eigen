find eigen3 -type f | python -c "import sys; print open('eigen.cabal.template').read().replace('{eigen3}', '    '.join(sys.stdin))" > eigen.cabal

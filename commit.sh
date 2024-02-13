
!/bin/sh
cd "OneDrive - California Department of Water Resources/smelt cages/SMTWaterQuality"
git add --all
timestamp() {
  date +"at %H:%M:%S on %d/%m/%Y"
}
git commit -m "Regular auto-commit $(timestamp)"
git push origin main


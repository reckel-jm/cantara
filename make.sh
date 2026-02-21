#
# Runs Lazbuild to build the project for release purpose
#

cd src
lazbuild -B --ws="qt6" bgrabitmap/bgrabitmap/bgrabitmappack.lpk
lazbuild -B --ws="qt6" metadarkstyle/metadarkstyle.lpk
lazbuild -B --bm="Release" --ws="qt6" Cantara.lpi
cd ..
mv src/cantara cantara

# Convert po files to mo files
for po_file in src/locals/cantara.*.po; do
    lang_code=$(basename "$po_file" .po | cut -d'.' -f2)
    mo_dir="src/languages/$lang_code"
    mo_file="src/languages/$lang_code/cantara.mo"
    mkdir -p "$mo_dir"
    echo "Compiling $po_file to $mo_file"
    echo "msgfmt -o $mo_file $po_file"
    msgfmt -o "$mo_file" "$po_file" 
done    

#!/bin/bash
#
# remove_fuzzy.sh - Entfernt alle fuzzy-Markierungen aus PO-Dateien
#
# Verwendung: ./remove_fuzzy.sh [Pfad zu PO-Dateien]
# Beispiel:   ./remove_fuzzy.sh ./src/locals/
#
# Das Skript entfernt:
#   - "#, fuzzy"-Zeilen
#   - "#| ..."-Zeilen (alte Originaltexte nach fuzzy)
# Und aktualisiert Last-Translator und PO-Revision-Date.
#

set -e

AUTHOR_NAME="Jan Martin Reckel"
AUTHOR_EMAIL="jm.reckel@t-online.de"

# Pfad zu den PO-Dateien (Standard: aktuelles Verzeichnis)
PO_DIR="${1:-.}"

# Prüfen ob Verzeichnis existiert
if [ ! -d "$PO_DIR" ]; then
    echo "Fehler: Verzeichnis '$PO_DIR' existiert nicht."
    exit 1
fi

# Alle PO-Dateien finden
mapfile -t PO_FILES < <(find "$PO_DIR" -name "*.po" -type f)

if [ ${#PO_FILES[@]} -eq 0 ]; then
    echo "Keine PO-Dateien in '$PO_DIR' gefunden."
    exit 0
fi

echo "Verarbeite PO-Dateien in: $PO_DIR"
echo "Autor: $AUTHOR_NAME <$AUTHOR_EMAIL>"
echo ""

PROCESSED=0

for PO_FILE in "${PO_FILES[@]}"; do
    echo "Verarbeite: $PO_FILE"
    
    # Temporäre Datei für die Bearbeitung
    TEMP_FILE=$(mktemp)
    
    # Zähler für entfernte fuzzy-Markierungen
    FUZZY_COUNT=0
    
    # Status: befinden wir uns in einem fuzzy-Block?
    IN_FUZZY_BLOCK=0
    
    # Datei zeilenweise verarbeiten
    while IFS= read -r line || [ -n "$line" ]; do
        # Prüfen ob dies eine fuzzy-Markierung ist
        if [[ "$line" == "#, fuzzy" ]]; then
            IN_FUZZY_BLOCK=1
            FUZZY_COUNT=$((FUZZY_COUNT + 1))
            continue
        fi
        
        # Wenn wir in einem fuzzy-Block sind, prüfe auf #| Kommentarzeilen
        if [ $IN_FUZZY_BLOCK -eq 1 ]; then
            # Prüfen ob dies eine #| Zeile ist (alter Originaltext)
            if [[ "$line" == "#|"* ]]; then
                # Diese Zeile überspringen (nicht zur Ausgabe hinzufügen)
                continue
            fi
            
            # Wir sind am Ende des fuzzy-Blocks
            IN_FUZZY_BLOCK=0
        fi
        
        # Normale Zeile zur Ausgabe hinzufügen
        echo "$line" >> "$TEMP_FILE"
        
    done < "$PO_FILE"
    
    # Wenn fuzzy-Markierungen entfernt wurden, aktualisiere Metadaten
    if [ $FUZZY_COUNT -gt 0 ]; then
        # Erstelle weitere temporäre Datei für Metadaten-Update
        TEMP_FILE2=$(mktemp)
        
        CURRENT_DATE=$(date +"%Y-%m-%d %H:%M")
        
        # Metadaten aktualisieren
        while IFS= read -r line || [ -n "$line" ]; do
            # Last-Translator aktualisieren
            if [[ "$line" == "\"Last-Translator:"* ]]; then
                echo "\"Last-Translator: $AUTHOR_NAME <$AUTHOR_EMAIL>\\n\"" >> "$TEMP_FILE2"
                continue
            fi
            
            # PO-Revision-Date aktualisieren
            if [[ "$line" == "\"PO-Revision-Date:"* ]]; then
                echo "\"PO-Revision-Date: $CURRENT_DATE\\n\"" >> "$TEMP_FILE2"
                continue
            fi
            
            # Normale Zeile übernehmen
            echo "$line" >> "$TEMP_FILE2"
            
        done < "$TEMP_FILE"
        
        # Temporäre Datei ersetzen
        mv "$TEMP_FILE2" "$PO_FILE"
        rm -f "$TEMP_FILE"
        
        echo "  -> $FUZZY_COUNT fuzzy-Markierung(en) entfernt"
        PROCESSED=$((PROCESSED + 1))
    else
        rm -f "$TEMP_FILE"
    fi
    
done

echo ""
echo "Fertig! $PROCESSED Datei(en) wurden aktualisiert."

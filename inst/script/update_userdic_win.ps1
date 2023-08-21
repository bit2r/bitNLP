$PROC_PATH = "C:\mecab\"
$DIC_PATH = "$($PROC_PATH)mecab-ko-dic\"
$DICT_INDEX = "$($PROC_PATH)mecab-dict-index.exe"
$USERDIC_PATH = $args[0]
$OUTFILE = $args[1]

function Compile {
    & $DICT_INDEX -d ${DIC_PATH} -u "${USERDIC_PATH}\${OUTFILE}" -f UTF-8 -t UTF-8 "${USERDIC_PATH}\indexed\merged.csv"

}

function main {
    Write-Output "updating userdic..."
    
    Compile
}

main

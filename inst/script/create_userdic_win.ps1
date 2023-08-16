$PROC_PATH = "C:\mecab\"
$DIC_PATH = "$($PROC_PATH)mecab-ko-dic\"
$MECAB_EXEC_PATH = "$($PROC_PATH)mecab.exe"
$DICT_INDEX = "$($PROC_PATH)mecab-dict-index.exe"
$USERDIC_PATH = $args[0]
$OUTFILE = $args[1]

if (-Not (Test-Path "${USERDIC_PATH}\indexed")) {
    New-Item -Path "${USERDIC_PATH}\indexed" -ItemType Directory
}

function Get-Userdics {
    $Dir = Get-Childitem $USERDIC_PATH -recurse
    $List = $Dir | Where-Object {$_.extension -eq ".csv"}
    $List
}

function Get-Cost {
    $input_dic = $args[0]
    & $DICT_INDEX -m "$($DIC_PATH)model.def" -d ${DIC_PATH} -u "${USERDIC_PATH}\indexed\nosys-$($input_dic)" -f utf-8 -t utf-8 -a "${USERDIC_PATH}\${input_dic}"
}

function Compile {
    Get-ChildItem -Recurse "$($USERDIC_PATH)\indexed\nosys-*.csv" | ForEach-Object { Get-Content -Encoding utf8 $_ } | Out-File -Encoding utf8 "${USERDIC_PATH}\indexed\merged.csv"
    & $DICT_INDEX -d ${DIC_PATH} -u "${USERDIC_PATH}\${OUTFILE}" -f UTF-8 -t UTF-8 "${USERDIC_PATH}\indexed\merged.csv"

}

function main {
    Write-Output "generating userdic..."

    Get-Userdics  | ForEach-Object {
        Get-Cost $_.Name
    }


    Compile
}

main

## ----environment, echo = FALSE, message = FALSE, warning=FALSE----------------
knitr::opts_chunk$set(collapse = TRUE, comment = "", out.width = "600px", dpi = 70,
                      echo = TRUE, message = FALSE, warning = FALSE)
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ---- eval=FALSE--------------------------------------------------------------
#  > doc <- "윤희근은 경찰청장이다."
#  > morpho_mecab(doc, type = "morphe")
#     NNP    NNG     JX    NNG    NNG     VCP     EF     SF
#  "윤희"   "근"    "은"  "경찰" "청장"   "이"   "다"    "."

## -----------------------------------------------------------------------------
#  get_plan_cost(x, topn = 3, dic_path = NULL)

## ---- eval=FALSE--------------------------------------------------------------
#  > get_plan_cost(doc, topn = 2)
#  # A tibble: 16 × 9
#     우선순위 표층형 품사태그 의미부류 좌문맥ID 우문맥ID 낱말비용 연접비용 누적비용
#        <int> <chr>  <chr>    <chr>       <int>    <int>    <int>    <int>    <int>
#   1        1 윤희   NNP      "인명"       1788     3549     5483    -2347     3136
#   2        1 근     NNG      ""           1780     3534     4535      -17     7654
#   3        1 은     JX       ""            682     2377      349    -2614     5389
#   4        1 경찰   NNG      ""           1780     3534     2371      826     8586
#   5        1 청장   NNG      ""           1780     3534     2084      269    10939
#   6        1 이     VCP      ""           2239     3575     1201    -1615    10525
#   7        1 다     EF       ""              3        5     2700    -3228     9997
#   8        1 .      SF       ""           1794     3560     3518    -1948    11567
#   9        2 윤희   NNP      "인명"       1788     3549     5483    -2347     3136
#  10        2 근     NNG      ""           1780     3534     4535      -17     7654
#  11        2 은     JX       ""            682     2377      349    -2614     5389
#  12        2 경찰청 NNG      ""           1780     3534     1896      826     8111
#  13        2 장     NNG      ""           1780     3534     3899      269    12279
#  14        2 이     VCP      ""           2239     3575     1201    -2955    10525
#  15        2 다     EF       ""              3        5     2700    -3228     9997
#  16        2 .      SF       ""           1794     3560     3518    -1948    11567

## -----------------------------------------------------------------------------
#  get_userdic_noun(
#    noun_type = c("person", "place", "nnp", "nng"),
#    userdic_path = NULL
#  )

## -----------------------------------------------------------------------------
#  > get_userdic_noun("person")
#  # A tibble: 2 × 13
#    표층형 미지정1 미지정2 미지정3 품사태그 의미부류 종성유무 읽기   타입  첫번째품사 마지막품사
#    <chr>  <lgl>   <lgl>   <lgl>   <chr>    <chr>    <lgl>    <chr>  <chr> <chr>      <chr>
#  1 까비   NA      NA      NA      NNP      인명     FALSE    까비   *     *          *
#  2 변학도 NA      NA      NA      NNP      인명     FALSE    변학도 *     *          *
#  # ℹ 2 more variables: 표현 <chr>, 인텍스표현 <chr>

## -----------------------------------------------------------------------------
#  > get_userdic_noun("nng")
#  # A tibble: 4 × 13
#    표층형     미지정1 미지정2 미지정3 품사태그 의미부류 종성유무 읽기       타입     첫번째품사 마지막품사 표현                    인텍스표현
#    <chr>      <lgl>   <lgl>   <lgl>   <chr>    <chr>    <lgl>    <chr>      <chr>    <chr>      <chr>      <chr>                   <chr>
#  1 재직증명서 NA      NA      NA      NNG      *        FALSE    재직증명서 Compound *          *          재직/NNG/*+증명서/NNG/* *
#  2 육아휴직   NA      NA      NA      NNG      *        TRUE     육아휴직   Compound *          *          육아/NNG/*+휴직/NNG/*   *
#  3 신혼부부   NA      NA      NA      NNG      *        FALSE    신혼부부   Compound *          *          신혼/NNG/*+부부/NNG/*   *
#  4 타이디버스 NA      NA      NA      NNG      *        FALSE    타이디버스 *        *          *          *                       *

## -----------------------------------------------------------------------------
#  append_userdic_noun(
#    x,
#    type = NULL,
#    prototype = NULL,
#    noun_type = c("person", "place", "nnp", "nng"),
#    userdic_path = NULL
#  )

## -----------------------------------------------------------------------------
#  > append_userdic_noun(c("윤희근"), noun_type = "person")
#  ── 사전 파일에 인명 추가하기 ─────────────────────────────────────────────────────
#  ✔ 신규 추가 건수: 1
#  ✔ 최종 인명 건수: 3
#  > get_userdic_noun("person")
#  # A tibble: 3 × 13
#    표층형 미지정1 미지정2 미지정3 품사태그 의미부류 종성유무 읽기  타입  첫번째품사
#    <chr>  <lgl>   <lgl>   <lgl>   <chr>    <chr>    <lgl>    <chr> <chr> <chr>
#  1 까비   NA      NA      NA      NNP      인명     FALSE    까비  *     *
#  2 변학도 NA      NA      NA      NNP      인명     FALSE    변학… *     *
#  3 윤희근 NA      NA      NA      NNP      인명     TRUE     윤희… *     *
#  # ℹ 3 more variables: 마지막품사 <chr>, 표현 <chr>, 인텍스표현 <chr>

## -----------------------------------------------------------------------------
#  > append_userdic_noun(c("경찰청장"), c("Compound"), c("경찰/NNG/*+청장/NNG/*"),
#  +                     noun_type = "nng")
#  ── 사전 파일에 일반명사 추가하기 ───────────────────────────────────────────────
#  ✔ 신규 추가 건수: 1
#  ✔ 최종 일반명사 건수: 5

## -----------------------------------------------------------------------------
#  add_sysdic()

## ---- echo=FALSE, out.width='95%', fig.align='center', fig.pos="!h"-----------
knitr::include_graphics("images/password.png")

## -----------------------------------------------------------------------------
#  > add_sysdic()
#  Password:generating userdic...
#  nng.csv
#  /usr/local/install_resources/mecab-ko-dic-2.1.1-20180720/tools/../model.def is not a binary model. reopen it as text mode...
#  reading /usr/local/install_resources/mecab-ko-dic-2.1.1-20180720/tools/../user-dic/nng.csv ...
#  done!
#  nnp.csv
#  /usr/local/install_resources/mecab-ko-dic-2.1.1-20180720/tools/../model.def is not a binary model. reopen it as text mode...
#  reading /usr/local/install_resources/mecab-ko-dic-2.1.1-20180720/tools/../user-dic/nnp.csv ...
#  done!
#  person.csv
#  /usr/local/install_resources/mecab-ko-dic-2.1.1-20180720/tools/../model.def is not a binary model. reopen it as text mode...
#  reading /usr/local/install_resources/mecab-ko-dic-2.1.1-20180720/tools/../user-dic/person.csv ...
#  done!
#  place.csv
#  /usr/local/install_resources/mecab-ko-dic-2.1.1-20180720/tools/../model.def is not a binary model. reopen it as text mode...
#  reading /usr/local/install_resources/mecab-ko-dic-2.1.1-20180720/tools/../user-dic/place.csv ...
#  done!
#  test -z "model.bin matrix.bin char.bin sys.dic unk.dic" || rm -f model.bin matrix.bin char.bin sys.dic unk.dic
#  /usr/local/libexec/mecab/mecab-dict-index -d . -o . -f UTF-8 -t UTF-8
#  reading ./unk.def ... 13
#  emitting double-array: 100% |###########################################|
#  reading ./Foreign.csv ... 11690
#  reading ./NNB.csv ... 140
#  reading ./Symbol.csv ... 16
#  reading ./MM.csv ... 453
#  reading ./user-person.csv ... 3
#  reading ./Preanalysis.csv ... 5
#  reading ./NorthKorea.csv ... 3
#  reading ./XPN.csv ... 83
#  reading ./NR.csv ... 482
#  reading ./NP.csv ... 342
#  reading ./VA.csv ... 2360
#  reading ./VV.csv ... 7331
#  reading ./XSV.csv ... 23
#  reading ./XSA.csv ... 19
#  reading ./user-nng.csv ... 5
#  reading ./NNG.csv ... 208524
#  reading ./NNP.csv ... 2371
#  reading ./user-nnp.csv ... 4
#  reading ./EF.csv ... 1820
#  reading ./EP.csv ... 51
#  reading ./user-place.csv ... 3
#  reading ./VCP.csv ... 9
#  reading ./IC.csv ... 1305
#  reading ./MAJ.csv ... 240
#  reading ./Place-address.csv ... 19301
#  reading ./EC.csv ... 2547
#  reading ./NNBC.csv ... 677
#  reading ./ETM.csv ... 133
#  reading ./Person-actor.csv ... 99230
#  reading ./MAG.csv ... 14242
#  reading ./VCN.csv ... 7
#  reading ./Wikipedia.csv ... 36762
#  reading ./ETN.csv ... 14
#  reading ./Person.csv ... 196459
#  reading ./Hanja.csv ... 125750
#  reading ./Place-station.csv ... 1145
#  reading ./Place.csv ... 30303
#  reading ./Inflect.csv ... 44820
#  reading ./J.csv ... 416
#  reading ./XR.csv ... 3637
#  reading ./XSN.csv ... 124
#  reading ./VX.csv ... 125
#  reading ./CoinedWord.csv ... 148
#  reading ./Group.csv ... 3176
#  emitting double-array: 100% |###########################################|
#  reading ./matrix.def ... 3822x2693
#  emitting matrix      : 100% |###########################################|
#  
#  done!
#  echo To enable dictionary, rewrite /usr/local/etc/mecabrc as \"dicdir = /usr/local/lib/mecab/dic/mecab-ko-dic\"
#  To enable dictionary, rewrite /usr/local/etc/mecabrc as "dicdir = /usr/local/lib/mecab/dic/mecab-ko-dic"
#  make[1]: Nothing to be done for `install-exec-am'.
#   ./install-sh -c -d '/usr/local/lib/mecab/dic/mecab-ko-dic'
#   /usr/bin/install -c -m 644 model.bin matrix.bin char.bin sys.dic unk.dic left-id.def right-id.def rewrite.def pos-id.def dicrc '/usr/local/lib/mecab/dic/mecab-ko-dic'
#  >

## -----------------------------------------------------------------------------
#  > get_plan_cost(doc, topn = 2)
#  # A tibble: 13 × 9
#     우선순위 표층형   품사태그 의미부류 좌문맥ID 우문맥ID 낱말비용 연접비용 누적비용
#        <int> <chr>    <chr>    <chr>       <int>    <int>    <int>    <int>    <int>
#   1        1 윤희근   NNP      "인명"       1788     3550     5472    -2347     3125
#   2        1 은       JX       ""            682     2377      349    -2579      895
#   3        1 경찰청장 NNG      ""           1780     3534     2639      826     4360
#   4        1 이       VCP      ""           2239     3575     1201    -1615     3946
#   5        1 다       EF       ""              3        5     2700    -3228     3418
#   6        1 .        SF       ""           1794     3560     3518    -1948     4988
#   7        2 윤희근   NNP      "인명"       1788     3550     5472    -2347     3125
#   8        2 은       JX       ""            682     2377      349    -2579      895
#   9        2 경찰     NNG      ""           1780     3534     2371      826     4092
#  10        2 청장     NNG      ""           1780     3534     2084      269     6445
#  11        2 이       VCP      ""           2239     3575     1201    -3700     3946
#  12        2 다       EF       ""              3        5     2700    -3228     3418
#  13        2 .        SF       ""           1794     3560     3518    -1948     4988

## -----------------------------------------------------------------------------
#  > morpho_mecab(doc, type = "morphe")
#         NNP         JX        NNG        VCP         EF         SF
#    "윤희근"       "은" "경찰청장"       "이"       "다"        "."

## -----------------------------------------------------------------------------
#  > doc <- "대장내시경 검사에서 대장용종을 제거했다."
#  > morpho_mecab(doc, type = "morpheme")
#       NNG      NNG      NNG      JKB      NNG      NNG      JKO      NNG   XSV+EP       EF       SF
#    "대장" "내시경"   "검사"   "에서"   "대장"   "용종"     "을"   "제거"     "했"     "다"      "."

## -----------------------------------------------------------------------------
#  > append_userdic_noun(
#  +     c("대장내시경", "대장용종"),
#  +     type = c("Compound", "Compound"),
#  +     prototype = c("대장/NNG/*+내시경/NNG/*", "대장/NNG/*+용종/NNG/*"),
#  +     noun_type = "nng",
#  +     dic_type = "userdic"
#  + )
#  ── 사전 파일에 일반명사 추가하기 ───────────────────────────────────────────────
#  ✔ 신규 추가 건수: 2
#  ✔ 최종 일반명사 건수: 6

## -----------------------------------------------------------------------------
#  > get_userdic_noun(noun_type = "nng", userdic_path = "./user_dic")
#  # A tibble: 6 × 13
#    표층형     미지정1 미지정2 미지정3 품사태그 의미부류 종성유무 읽기       타입     첫번째품사 마지막품사 표현                    인텍스표현
#    <chr>      <lgl>   <lgl>   <lgl>   <chr>    <chr>    <lgl>    <chr>      <chr>    <chr>      <chr>      <chr>                   <chr>
#  1 재직증명서 NA      NA      NA      NNG      *        FALSE    재직증명서 Compound *          *          재직/NNG/*+증명서/NNG/* *
#  2 육아휴직   NA      NA      NA      NNG      *        TRUE     육아휴직   Compound *          *          육아/NNG/*+휴직/NNG/*   *
#  3 신혼부부   NA      NA      NA      NNG      *        FALSE    신혼부부   Compound *          *          신혼/NNG/*+부부/NNG/*   *
#  4 타이디버스 NA      NA      NA      NNG      *        FALSE    타이디버스 *        *          *          *                       *
#  5 대장내시경 NA      NA      NA      NNG      *        TRUE     대장내시경 Compound *          *          대장/NNG/*+내시경/NNG/* *
#  6 대장용종   NA      NA      NA      NNG      *        TRUE     대장용종   Compound *          *          대장/NNG/*+용종/NNG/*   *

## -----------------------------------------------------------------------------
#  create_userdic(
#    userdic_path = "./user_dic",
#    dic_file = "user-dic.dic"
#  )

## -----------------------------------------------------------------------------
#  > create_userdic()
#  generating userdic...
#  nng.csv
#  /usr/local/install_resources/mecab-ko-dic-2.1.1-20180720/model.def is not a binary model. reopen it as text mode...
#  reading ./user_dic/nng.csv ...
#  done!
#  reading ./user_dic/indexed/merged.csv ... 6
#  emitting double-array: 100% |###########################################|
#  
#  done!

## -----------------------------------------------------------------------------
#  > system("tree ./user_dic")
#  ./user_dic
#  ├── indexed
#  │   ├── merged.csv
#  │   └── nosys-nng.csv
#  ├── nng.csv
#  └── user-dic.dic
#  
#  2 directories, 4 files

## -----------------------------------------------------------------------------
#  > morpho_mecab(doc, type = "morpheme")
#       NNG      NNG      NNG      JKB      NNG      NNG      JKO      NNG   XSV+EP       EF       SF
#    "대장" "내시경"   "검사"   "에서"   "대장"   "용종"     "을"   "제거"     "했"     "다"      "."

## -----------------------------------------------------------------------------
#  > morpho_mecab(doc, type = "morpheme", user_dic = "./user_dic/user-dic.dic")
#           NNG          NNG          JKB          NNG          JKO          NNG       XSV+EP           EF           SF
#  "대장내시경"       "검사"       "에서"   "대장용종"         "을"       "제거"         "했"         "다"          "."


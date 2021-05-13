let hiragana str =
  let rec convert l =
    match l with
    | 'a'::tl -> "あ" ^ convert tl
    | 'i'::tl -> "い" ^ convert tl
    | 'u'::tl -> "う" ^ convert tl
    | 'e'::tl -> "え" ^ convert tl
    | 'o'::tl -> "お" ^ convert tl
    | 'n'::'\''::tl -> "ん" ^ convert tl


    | 'k'::'a'::tl -> "か" ^ convert tl
    | 'k'::'i'::tl -> "き" ^ convert tl
    | 'k'::'u'::tl -> "く" ^ convert tl
    | 'k'::'e'::tl -> "け" ^ convert tl
    | 'k'::'o'::tl -> "こ" ^ convert tl

    | 'k'::'k'::'a'::tl -> "っか" ^ convert tl
    | 'k'::'k'::'i'::tl -> "っき" ^ convert tl
    | 'k'::'k'::'u'::tl -> "っく" ^ convert tl
    | 'k'::'k'::'e'::tl -> "っけ" ^ convert tl
    | 'k'::'k'::'o'::tl -> "っこ" ^ convert tl


    | 'k'::'y'::'a'::tl -> "きゃ" ^ convert tl
    | 'k'::'y'::'u'::tl -> "きゅ" ^ convert tl
    | 'k'::'y'::'o'::tl -> "きょ" ^ convert tl

    | 'k'::'k'::'y'::'a'::tl -> "っきゃ" ^ convert tl
    | 'k'::'k'::'y'::'u'::tl -> "っきゅ" ^ convert tl
    | 'k'::'k'::'y'::'o'::tl -> "っきょ" ^ convert tl


    | 'g'::'a'::tl -> "が" ^ convert tl
    | 'g'::'i'::tl -> "ぎ" ^ convert tl
    | 'g'::'u'::tl -> "ぐ" ^ convert tl
    | 'g'::'e'::tl -> "げ" ^ convert tl
    | 'g'::'o'::tl -> "ご" ^ convert tl

    | 'g'::'g'::'a'::tl -> "っが" ^ convert tl
    | 'g'::'g'::'i'::tl -> "っぎ" ^ convert tl
    | 'g'::'g'::'u'::tl -> "っぐ" ^ convert tl
    | 'g'::'g'::'e'::tl -> "っげ" ^ convert tl
    | 'g'::'g'::'o'::tl -> "っご" ^ convert tl


    | 'g'::'y'::'a'::tl -> "ぎゃ" ^ convert tl
    | 'g'::'y'::'u'::tl -> "ぎゅ" ^ convert tl
    | 'g'::'y'::'o'::tl -> "ぎょ" ^ convert tl

    | 'g'::'g'::'y'::'a'::tl -> "っぎゃ" ^ convert tl
    | 'g'::'g'::'y'::'u'::tl -> "っぎゅ" ^ convert tl
    | 'g'::'g'::'y'::'o'::tl -> "っぎょ" ^ convert tl


    | 's'::'a'::tl -> "さ" ^ convert tl
    | 's'::'i'::tl -> "し" ^ convert tl
    | 's'::'h'::'i'::tl -> "し" ^ convert tl
    | 's'::'u'::tl -> "す" ^ convert tl
    | 's'::'e'::tl -> "せ" ^ convert tl
    | 's'::'o'::tl -> "そ" ^ convert tl

    | 's'::'s'::'a'::tl -> "っさ" ^ convert tl
    | 's'::'s'::'i'::tl -> "っし" ^ convert tl
    | 's'::'s'::'h'::'i'::tl -> "っし" ^ convert tl
    | 's'::'s'::'u'::tl -> "っす" ^ convert tl
    | 's'::'s'::'e'::tl -> "っせ" ^ convert tl
    | 's'::'s'::'o'::tl -> "っそ" ^ convert tl

  
    | 's'::'h'::'a'::tl -> "しゃ" ^ convert tl
    | 's'::'h'::'u'::tl -> "しゅ" ^ convert tl
    | 's'::'h'::'o'::tl -> "しょ" ^ convert tl

    | 's'::'s'::'h'::'a'::tl -> "っしゃ" ^ convert tl
    | 's'::'s'::'h'::'u'::tl -> "っしゅ" ^ convert tl
    | 's'::'s'::'h'::'o'::tl -> "っしょ" ^ convert tl


    | 'z'::'a'::tl -> "ざ" ^ convert tl
    | 'z'::'i'::tl -> "じ" ^ convert tl
    | 'j'::'i'::tl -> "じ" ^ convert tl
    | 'z'::'u'::tl -> "ず" ^ convert tl
    | 'z'::'e'::tl -> "ぜ" ^ convert tl
    | 'z'::'o'::tl -> "ぞ" ^ convert tl

    | 'z'::'z'::'a'::tl -> "っざ" ^ convert tl
    | 'z'::'z'::'i'::tl -> "っじ" ^ convert tl
    | 'j'::'j'::'i'::tl -> "っじ" ^ convert tl
    | 'z'::'z'::'u'::tl -> "っず" ^ convert tl
    | 'z'::'z'::'e'::tl -> "っぜ" ^ convert tl
    | 'z'::'z'::'o'::tl -> "っぞ" ^ convert tl


    | 'j'::'a'::tl -> "じゃ" ^ convert tl
    | 'j'::'u'::tl -> "じゅ" ^ convert tl
    | 'j'::'o'::tl -> "じょ" ^ convert tl

    | 'j'::'j'::'a'::tl -> "っじゃ" ^ convert tl
    | 'j'::'j'::'u'::tl -> "っじゅ" ^ convert tl
    | 'j'::'j'::'o'::tl -> "っじょ" ^ convert tl


    | 't'::'a'::tl -> "た" ^ convert tl
    | 't'::'i'::tl -> "ち" ^ convert tl
    | 'c'::'h'::'i'::tl -> "ち" ^ convert tl
    | 't'::'u'::tl -> "つ" ^ convert tl
    | 't'::'s'::'u'::tl -> "つ" ^ convert tl
    | 't'::'e'::tl -> "て" ^ convert tl
    | 't'::'o'::tl -> "と" ^ convert tl

    | 't'::'t'::'a'::tl -> "った" ^ convert tl
    | 't'::'t'::'i'::tl -> "っち" ^ convert tl
    | 'c'::'c'::'h'::'i'::tl -> "っち" ^ convert tl
    | 't'::'c'::'h'::'i'::tl -> "っち" ^ convert tl
    | 't'::'t'::'u'::tl -> "っつ" ^ convert tl
    | 't'::'t'::'s'::'u'::tl -> "っつ" ^ convert tl
    | 't'::'t'::'e'::tl -> "って" ^ convert tl
    | 't'::'t'::'o'::tl -> "っと" ^ convert tl


    | 'c'::'h'::'a'::tl -> "ちゃ" ^ convert tl
    | 'c'::'h'::'u'::tl -> "ちゅ" ^ convert tl
    | 'c'::'h'::'o'::tl -> "ちょ" ^ convert tl

    | 'c'::'c'::'h'::'a'::tl -> "っちゃ" ^ convert tl
    | 't'::'c'::'h'::'a'::tl -> "っちゃ" ^ convert tl
    | 'c'::'c'::'h'::'u'::tl -> "っちゅ" ^ convert tl
    | 't'::'c'::'h'::'u'::tl -> "っちゅ" ^ convert tl
    | 'c'::'c'::'h'::'o'::tl -> "っちょ" ^ convert tl
    | 't'::'c'::'h'::'o'::tl -> "っちょ" ^ convert tl


    | 'd'::'a'::tl -> "だ" ^ convert tl
    | 'd'::'i'::tl -> "ぢ" ^ convert tl
    | 'd'::'j'::'i'::tl -> "ぢ" ^ convert tl
    | 'd'::'u'::tl -> "づ" ^ convert tl
    | 'd'::'z'::'u'::tl -> "づ" ^ convert tl
    | 'd'::'e'::tl -> "で" ^ convert tl
    | 'd'::'o'::tl -> "ど" ^ convert tl

    | 'd'::'d'::'a'::tl -> "っだ" ^ convert tl
    | 'd'::'d'::'i'::tl -> "っぢ" ^ convert tl
    | 'd'::'d'::'j'::'i'::tl -> "っぢ" ^ convert tl
    | 'd'::'d'::'u'::tl -> "っづ" ^ convert tl
    | 'd'::'d'::'z'::'u'::tl -> "っづ" ^ convert tl
    | 'd'::'d'::'e'::tl -> "っで" ^ convert tl
    | 'd'::'d'::'o'::tl -> "っど" ^ convert tl


    | 'n'::'a'::tl -> "な" ^ convert tl
    | 'n'::'i'::tl -> "に" ^ convert tl
    | 'n'::'u'::tl -> "ぬ" ^ convert tl
    | 'n'::'e'::tl -> "ね" ^ convert tl
    | 'n'::'o'::tl -> "の" ^ convert tl

    | 'n'::'n'::'a'::tl -> "んな" ^ convert tl
    | 'n'::'n'::'i'::tl -> "んに" ^ convert tl
    | 'n'::'n'::'u'::tl -> "んぬ" ^ convert tl
    | 'n'::'n'::'e'::tl -> "んね" ^ convert tl
    | 'n'::'n'::'o'::tl -> "んの" ^ convert tl


    | 'n'::'y'::'a'::tl -> "にゃ" ^ convert tl
    | 'n'::'y'::'u'::tl -> "にゅ" ^ convert tl
    | 'n'::'y'::'o'::tl -> "にょ" ^ convert tl

    | 'n'::'n'::'y'::'a'::tl -> "んにゃ" ^ convert tl
    | 'n'::'n'::'y'::'u'::tl -> "んにゅ" ^ convert tl
    | 'n'::'n'::'y'::'o'::tl -> "んにょ" ^ convert tl


    | 'h'::'a'::tl -> "は" ^ convert tl
    | 'h'::'i'::tl -> "ひ" ^ convert tl
    | 'h'::'u'::tl -> "ふ" ^ convert tl
    | 'f'::'u'::tl -> "ふ" ^ convert tl
    | 'h'::'e'::tl -> "へ" ^ convert tl
    | 'h'::'o'::tl -> "ほ" ^ convert tl

    | 'h'::'h'::'a'::tl -> "っは" ^ convert tl
    | 'h'::'h'::'i'::tl -> "っひ" ^ convert tl
    | 'h'::'h'::'u'::tl -> "っふ" ^ convert tl
    | 'f'::'f'::'u'::tl -> "っふ" ^ convert tl
    | 'h'::'h'::'e'::tl -> "っへ" ^ convert tl
    | 'h'::'h'::'o'::tl -> "っほ" ^ convert tl


    | 'h'::'y'::'a'::tl -> "ひゃ" ^ convert tl
    | 'h'::'y'::'u'::tl -> "ひゅ" ^ convert tl
    | 'h'::'y'::'o'::tl -> "ひょ" ^ convert tl

    | 'h'::'h'::'y'::'a'::tl -> "っひゃ" ^ convert tl
    | 'h'::'h'::'y'::'u'::tl -> "っひゅ" ^ convert tl
    | 'h'::'h'::'y'::'o'::tl -> "っひょ" ^ convert tl


    | 'p'::'a'::tl -> "ぱ" ^ convert tl
    | 'p'::'i'::tl -> "ぴ" ^ convert tl
    | 'p'::'u'::tl -> "ぷ" ^ convert tl
    | 'p'::'e'::tl -> "ぺ" ^ convert tl
    | 'p'::'o'::tl -> "ぽ" ^ convert tl

    | 'p'::'p'::'a'::tl -> "っぱ" ^ convert tl
    | 'p'::'p'::'i'::tl -> "っぴ" ^ convert tl
    | 'p'::'p'::'u'::tl -> "っぷ" ^ convert tl
    | 'p'::'p'::'e'::tl -> "っぺ" ^ convert tl
    | 'p'::'p'::'o'::tl -> "っぽ" ^ convert tl


    | 'p'::'y'::'a'::tl -> "ぴゃ" ^ convert tl
    | 'p'::'y'::'u'::tl -> "ぴゅ" ^ convert tl
    | 'p'::'y'::'o'::tl -> "ぴょ" ^ convert tl

    | 'p'::'p'::'y'::'a'::tl -> "っぴゃ" ^ convert tl
    | 'p'::'p'::'y'::'u'::tl -> "っぴゅ" ^ convert tl
    | 'p'::'p'::'y'::'o'::tl -> "っぴょ" ^ convert tl


    | 'b'::'a'::tl -> "ば" ^ convert tl
    | 'b'::'i'::tl -> "び" ^ convert tl
    | 'b'::'u'::tl -> "ぶ" ^ convert tl
    | 'b'::'e'::tl -> "べ" ^ convert tl
    | 'b'::'o'::tl -> "ぼ" ^ convert tl

    | 'b'::'b'::'a'::tl -> "っば" ^ convert tl
    | 'b'::'b'::'i'::tl -> "っび" ^ convert tl
    | 'b'::'b'::'u'::tl -> "っぶ" ^ convert tl
    | 'b'::'b'::'e'::tl -> "っべ" ^ convert tl
    | 'b'::'b'::'o'::tl -> "っぼ" ^ convert tl


    | 'b'::'y'::'a'::tl -> "びゃ" ^ convert tl
    | 'b'::'y'::'u'::tl -> "びゅ" ^ convert tl
    | 'b'::'y'::'o'::tl -> "びょ" ^ convert tl

    | 'b'::'b'::'y'::'a'::tl -> "っびゃ" ^ convert tl
    | 'b'::'b'::'y'::'u'::tl -> "っびゅ" ^ convert tl
    | 'b'::'b'::'y'::'o'::tl -> "っびょ" ^ convert tl


    | 'm'::'a'::tl -> "ま" ^ convert tl
    | 'm'::'i'::tl -> "み" ^ convert tl
    | 'm'::'u'::tl -> "む" ^ convert tl
    | 'm'::'e'::tl -> "め" ^ convert tl
    | 'm'::'o'::tl -> "も" ^ convert tl

    | 'm'::'m'::'a'::tl -> "っま" ^ convert tl
    | 'm'::'m'::'i'::tl -> "っみ" ^ convert tl
    | 'm'::'m'::'u'::tl -> "っむ" ^ convert tl
    | 'm'::'m'::'e'::tl -> "っめ" ^ convert tl
    | 'm'::'m'::'o'::tl -> "っも" ^ convert tl


    | 'm'::'y'::'a'::tl -> "みゃ" ^ convert tl
    | 'm'::'y'::'u'::tl -> "みゅ" ^ convert tl
    | 'm'::'y'::'o'::tl -> "みょ" ^ convert tl

    | 'm'::'m'::'y'::'a'::tl -> "っみゃ" ^ convert tl
    | 'm'::'m'::'y'::'u'::tl -> "っみゅ" ^ convert tl
    | 'm'::'m'::'y'::'o'::tl -> "っみょ" ^ convert tl


    | 'y'::'a'::tl -> "や" ^ convert tl
    | 'y'::'u'::tl -> "ゆ" ^ convert tl
    | 'y'::'o'::tl -> "よ" ^ convert tl

    | 'y'::'y'::'a'::tl -> "っや" ^ convert tl
    | 'y'::'y'::'u'::tl -> "っゆ" ^ convert tl
    | 'y'::'y'::'o'::tl -> "っよ" ^ convert tl


    | 'r'::'a'::tl -> "ら" ^ convert tl
    | 'r'::'i'::tl -> "り" ^ convert tl
    | 'r'::'u'::tl -> "る" ^ convert tl
    | 'r'::'e'::tl -> "れ" ^ convert tl
    | 'r'::'o'::tl -> "ろ" ^ convert tl

    | 'r'::'r'::'a'::tl -> "っら" ^ convert tl
    | 'r'::'r'::'i'::tl -> "っり" ^ convert tl
    | 'r'::'r'::'u'::tl -> "っる" ^ convert tl
    | 'r'::'r'::'e'::tl -> "っれ" ^ convert tl
    | 'r'::'r'::'o'::tl -> "っろ" ^ convert tl


    | 'r'::'y'::'a'::tl -> "りゃ" ^ convert tl
    | 'r'::'y'::'u'::tl -> "りゅ" ^ convert tl
    | 'r'::'y'::'o'::tl -> "りょ" ^ convert tl

    | 'r'::'r'::'y'::'a'::tl -> "っりゃ" ^ convert tl
    | 'r'::'r'::'y'::'u'::tl -> "っりゅ" ^ convert tl
    | 'r'::'r'::'y'::'o'::tl -> "っりょ" ^ convert tl


    | 'w'::'a'::tl -> "わ" ^ convert tl
    | 'w'::'o'::tl -> "を" ^ convert tl

    | 'w'::'w'::'a'::tl -> "っわ" ^ convert tl
    | 'w'::'w'::'o'::tl -> "っを" ^ convert tl


    | 'n'::tl -> "ん" ^ convert tl


    | ' '::'w'::'a'::' '::tl -> "は" ^ convert tl
    | ' '::'w'::'a'::[] -> "は"

    | ' '::'e'::' '::tl -> "へ" ^ convert tl
    | ' '::'e'::[] -> "へ"
  

    | '('::tl -> "「" ^ convert tl
    | ')'::tl -> "」" ^ convert tl
    | '['::tl -> "『" ^ convert tl
    | ']'::tl -> "』" ^ convert tl
    | ','::tl -> "、" ^ convert tl
    | '.'::tl -> "。" ^ convert tl
    | '?'::tl -> "？" ^ convert tl
    | '!'::tl -> "！" ^ convert tl

    | '\n'::tl -> "\n" ^ convert tl
    | _::tl -> convert tl
    | _ -> ""
  in
  convert (List.of_seq (String.to_seq str))

let katakana str =
  let rec convert l =
    match l with
    | 'a'::tl -> "ア" ^ convert tl
    | 'i'::tl -> "イ" ^ convert tl
    | 'u'::tl -> "ウ" ^ convert tl
    | 'e'::tl -> "エ" ^ convert tl
    | 'o'::tl -> "オ" ^ convert tl
    | 'n'::'\''::tl -> "ン" ^ convert tl


    | 'k'::'a'::tl -> "カ" ^ convert tl
    | 'k'::'i'::tl -> "キ" ^ convert tl
    | 'k'::'u'::tl -> "ク" ^ convert tl
    | 'k'::'e'::tl -> "ケ" ^ convert tl
    | 'k'::'o'::tl -> "コ" ^ convert tl

    | 'k'::'k'::'a'::tl -> "ッカ" ^ convert tl
    | 'k'::'k'::'i'::tl -> "ッキ" ^ convert tl
    | 'k'::'k'::'u'::tl -> "ツク" ^ convert tl
    | 'k'::'k'::'e'::tl -> "ッケ" ^ convert tl
    | 'k'::'k'::'o'::tl -> "ッコ" ^ convert tl


    | 'k'::'y'::'a'::tl -> "キャ" ^ convert tl
    | 'k'::'y'::'u'::tl -> "キュ" ^ convert tl
    | 'k'::'y'::'e'::tl -> "キェ" ^ convert tl
    | 'k'::'y'::'o'::tl -> "キョ" ^ convert tl

    | 'k'::'k'::'y'::'a'::tl -> "ッキャ" ^ convert tl
    | 'k'::'k'::'y'::'u'::tl -> "ッキュ" ^ convert tl
    | 'k'::'k'::'y'::'e'::tl -> "ッキェ" ^ convert tl
    | 'k'::'k'::'y'::'o'::tl -> "ッキョ" ^ convert tl


    | 'g'::'a'::tl -> "ガ" ^ convert tl
    | 'g'::'i'::tl -> "ギ" ^ convert tl
    | 'g'::'u'::tl -> "グ" ^ convert tl
    | 'g'::'e'::tl -> "ゲ" ^ convert tl
    | 'g'::'o'::tl -> "ゴ" ^ convert tl

    | 'g'::'g'::'a'::tl -> "ッガ" ^ convert tl
    | 'g'::'g'::'i'::tl -> "ッギ" ^ convert tl
    | 'g'::'g'::'u'::tl -> "ッグ" ^ convert tl
    | 'g'::'g'::'e'::tl -> "ッゲ" ^ convert tl
    | 'g'::'g'::'o'::tl -> "ッゴ" ^ convert tl


    | 'g'::'y'::'a'::tl -> "ギャ" ^ convert tl
    | 'g'::'y'::'u'::tl -> "ギュ" ^ convert tl
    | 'g'::'y'::'e'::tl -> "ギェ" ^ convert tl
    | 'g'::'y'::'o'::tl -> "ギョ" ^ convert tl

    | 'g'::'g'::'y'::'a'::tl -> "ッギャ" ^ convert tl
    | 'g'::'g'::'y'::'u'::tl -> "ッギュ" ^ convert tl
    | 'g'::'g'::'y'::'e'::tl -> "ッギェ" ^ convert tl
    | 'g'::'g'::'y'::'o'::tl -> "ッギョ" ^ convert tl


    | 's'::'a'::tl -> "サ" ^ convert tl
    | 's'::'i'::tl -> "シ" ^ convert tl
    | 's'::'h'::'i'::tl -> "シ" ^ convert tl
    | 's'::'u'::tl -> "ス" ^ convert tl
    | 's'::'e'::tl -> "セ" ^ convert tl
    | 's'::'o'::tl -> "ソ" ^ convert tl

    | 's'::'s'::'a'::tl -> "ッサ" ^ convert tl
    | 's'::'s'::'i'::tl -> "ッシ" ^ convert tl
    | 's'::'s'::'h'::'i'::tl -> "ッシ" ^ convert tl
    | 's'::'s'::'u'::tl -> "ッス" ^ convert tl
    | 's'::'s'::'e'::tl -> "ッセ" ^ convert tl
    | 's'::'s'::'o'::tl -> "ッソ" ^ convert tl

  
    | 's'::'h'::'a'::tl -> "シャ" ^ convert tl
    | 's'::'h'::'u'::tl -> "シュ" ^ convert tl
    | 's'::'h'::'e'::tl -> "シェ" ^ convert tl
    | 's'::'h'::'o'::tl -> "ショ" ^ convert tl

    | 's'::'s'::'h'::'a'::tl -> "ッシャ" ^ convert tl
    | 's'::'s'::'h'::'u'::tl -> "ッシュ" ^ convert tl
    | 's'::'s'::'h'::'e'::tl -> "ッシェ" ^ convert tl
    | 's'::'s'::'h'::'o'::tl -> "ッショ" ^ convert tl


    | 'z'::'a'::tl -> "ザ" ^ convert tl
    | 'z'::'i'::tl -> "ジ" ^ convert tl
    | 'j'::'i'::tl -> "ジ" ^ convert tl
    | 'z'::'u'::tl -> "ズ" ^ convert tl
    | 'z'::'e'::tl -> "ゼ" ^ convert tl
    | 'z'::'o'::tl -> "ゾ" ^ convert tl

    | 'z'::'z'::'a'::tl -> "ッザ" ^ convert tl
    | 'z'::'z'::'i'::tl -> "ッジ" ^ convert tl
    | 'j'::'j'::'i'::tl -> "ッジ" ^ convert tl
    | 'z'::'z'::'u'::tl -> "ッズ" ^ convert tl
    | 'z'::'z'::'e'::tl -> "ッゼ" ^ convert tl
    | 'z'::'z'::'o'::tl -> "ッゾ" ^ convert tl


    | 'j'::'a'::tl -> "ジャ" ^ convert tl
    | 'j'::'u'::tl -> "ジュ" ^ convert tl
    | 'j'::'e'::tl -> "ジェ" ^ convert tl
    | 'j'::'o'::tl -> "ジョ" ^ convert tl

    | 'j'::'j'::'a'::tl -> "ッジャ" ^ convert tl
    | 'j'::'j'::'u'::tl -> "ッジュ" ^ convert tl
    | 'j'::'j'::'e'::tl -> "ッジェ" ^ convert tl
    | 'j'::'j'::'o'::tl -> "ッジョ" ^ convert tl


    | 't'::'a'::tl -> "タ" ^ convert tl
    | 't'::'i'::tl -> "チ" ^ convert tl
    | 'c'::'h'::'i'::tl -> "チ" ^ convert tl
    | 't'::'u'::tl -> "ツ" ^ convert tl
    | 't'::'s'::'u'::tl -> "ツ" ^ convert tl
    | 't'::'e'::tl -> "テ" ^ convert tl
    | 't'::'o'::tl -> "ト" ^ convert tl

    | 't'::'t'::'a'::tl -> "ッタ" ^ convert tl
    | 't'::'t'::'i'::tl -> "ッチ" ^ convert tl
    | 'c'::'c'::'h'::'i'::tl -> "ッチ" ^ convert tl
    | 't'::'c'::'h'::'i'::tl -> "ッチ" ^ convert tl
    | 't'::'t'::'u'::tl -> "ッツ" ^ convert tl
    | 't'::'t'::'s'::'u'::tl -> "ッツ" ^ convert tl
    | 't'::'t'::'e'::tl -> "ッテ" ^ convert tl
    | 't'::'t'::'o'::tl -> "ット" ^ convert tl


    | 'c'::'h'::'a'::tl -> "チャ" ^ convert tl
    | 'c'::'h'::'u'::tl -> "チュ" ^ convert tl
    | 'c'::'h'::'e'::tl -> "チェ" ^ convert tl
    | 'c'::'h'::'o'::tl -> "チョ" ^ convert tl
    
    | 'c'::'c'::'h'::'a'::tl -> "ッチャ" ^ convert tl
    | 't'::'c'::'h'::'a'::tl -> "ッチャ" ^ convert tl
    | 'c'::'c'::'h'::'u'::tl -> "ッチュ" ^ convert tl
    | 't'::'c'::'h'::'u'::tl -> "ッチュ" ^ convert tl
    | 'c'::'c'::'h'::'e'::tl -> "ッチェ" ^ convert tl
    | 't'::'c'::'h'::'e'::tl -> "ッチェ" ^ convert tl
    | 'c'::'c'::'h'::'o'::tl -> "ッチョ" ^ convert tl
    | 't'::'c'::'h'::'o'::tl -> "ッチョ" ^ convert tl
    
    
    | 't'::'h'::'i'::tl -> "ティ" ^ convert tl
    | 't'::'h'::'u'::tl -> "テュ" ^ convert tl
    
    | 't'::'t'::'h'::'i'::tl -> "ッティ" ^ convert tl
    | 't'::'t'::'h'::'u'::tl -> "ッテユ" ^ convert tl


    | 't'::'s'::'a'::tl -> "ツァ" ^ convert tl
    | 't'::'s'::'i'::tl -> "ツィ" ^ convert tl
    | 't'::'s'::'e'::tl -> "ツェ" ^ convert tl
    | 't'::'s'::'o'::tl -> "ツォ" ^ convert tl

    | 't'::'t'::'s'::'a'::tl -> "ッツァ" ^ convert tl
    | 't'::'t'::'s'::'i'::tl -> "ッツィ" ^ convert tl
    | 't'::'t'::'s'::'e'::tl -> "ッツェ" ^ convert tl
    | 't'::'t'::'s'::'o'::tl -> "ッツォ" ^ convert tl   


    | 'd'::'a'::tl -> "ダ" ^ convert tl
    | 'd'::'i'::tl -> "ヂ" ^ convert tl
    | 'd'::'j'::'i'::tl -> "ヂ" ^ convert tl
    | 'd'::'u'::tl -> "ヅ" ^ convert tl
    | 'd'::'z'::'u'::tl -> "ヅ" ^ convert tl
    | 'd'::'e'::tl -> "デ" ^ convert tl
    | 'd'::'o'::tl -> "ド" ^ convert tl

    | 'd'::'d'::'a'::tl -> "ッダ" ^ convert tl
    | 'd'::'d'::'i'::tl -> "ッヂ" ^ convert tl
    | 'd'::'d'::'j'::'i'::tl -> "ッヂ" ^ convert tl
    | 'd'::'d'::'u'::tl -> "ッヅ" ^ convert tl
    | 'd'::'d'::'z'::'u'::tl -> "ッヅ" ^ convert tl
    | 'd'::'d'::'e'::tl -> "ッデ" ^ convert tl
    | 'd'::'d'::'o'::tl -> "ッド" ^ convert tl


    | 'd'::'h'::'i'::tl -> "ディ" ^ convert tl
    | 'd'::'h'::'u'::tl -> "デュ" ^ convert tl
    
    | 'd'::'d'::'h'::'i'::tl -> "ッディ" ^ convert tl
    | 'd'::'d'::'h'::'u'::tl -> "ッデユ" ^ convert tl


    | 'n'::'a'::tl -> "ナ" ^ convert tl
    | 'n'::'i'::tl -> "ニ" ^ convert tl
    | 'n'::'u'::tl -> "ヌ" ^ convert tl
    | 'n'::'e'::tl -> "ネ" ^ convert tl
    | 'n'::'o'::tl -> "ノ" ^ convert tl

    | 'n'::'n'::'a'::tl -> "ンナ" ^ convert tl
    | 'n'::'n'::'i'::tl -> "ンニ" ^ convert tl
    | 'n'::'n'::'u'::tl -> "ンヌ" ^ convert tl
    | 'n'::'n'::'e'::tl -> "ンネ" ^ convert tl
    | 'n'::'n'::'o'::tl -> "ンノ" ^ convert tl


    | 'n'::'y'::'a'::tl -> "ニャ" ^ convert tl
    | 'n'::'y'::'u'::tl -> "ニュ" ^ convert tl
    | 'n'::'y'::'e'::tl -> "ニェ" ^ convert tl
    | 'n'::'y'::'o'::tl -> "ニョ" ^ convert tl

    | 'n'::'n'::'y'::'a'::tl -> "ンニャ" ^ convert tl
    | 'n'::'n'::'y'::'u'::tl -> "ンニュ" ^ convert tl
    | 'n'::'n'::'y'::'e'::tl -> "ンニェ" ^ convert tl
    | 'n'::'n'::'y'::'o'::tl -> "ンニョ" ^ convert tl


    | 'h'::'a'::tl -> "ハ" ^ convert tl
    | 'h'::'i'::tl -> "ヒ" ^ convert tl
    | 'h'::'u'::tl -> "フ" ^ convert tl
    | 'f'::'u'::tl -> "フ" ^ convert tl
    | 'h'::'e'::tl -> "ヘ" ^ convert tl
    | 'h'::'o'::tl -> "ホ" ^ convert tl

    | 'h'::'h'::'a'::tl -> "ッハ" ^ convert tl
    | 'h'::'h'::'i'::tl -> "ッヒ" ^ convert tl
    | 'h'::'h'::'u'::tl -> "ッフ" ^ convert tl
    | 'f'::'f'::'u'::tl -> "ッフ" ^ convert tl
    | 'h'::'h'::'e'::tl -> "ッヘ" ^ convert tl
    | 'h'::'h'::'o'::tl -> "ッホ" ^ convert tl


    | 'h'::'y'::'a'::tl -> "ヒャ" ^ convert tl
    | 'h'::'y'::'u'::tl -> "ヒュ" ^ convert tl
    | 'h'::'y'::'e'::tl -> "ヒェ" ^ convert tl
    | 'h'::'y'::'o'::tl -> "ヒョ" ^ convert tl

    | 'h'::'h'::'y'::'a'::tl -> "ッヒャ" ^ convert tl
    | 'h'::'h'::'y'::'u'::tl -> "ッヒュ" ^ convert tl
    | 'h'::'h'::'y'::'e'::tl -> "ッヒェ" ^ convert tl
    | 'h'::'h'::'y'::'o'::tl -> "ッヒョ" ^ convert tl


    | 'f'::'a'::tl -> "ファ" ^ convert tl
    | 'f'::'i'::tl -> "フィ" ^ convert tl
    | 'f'::'e'::tl -> "フェ" ^ convert tl
    | 'f'::'o'::tl -> "フォ" ^ convert tl

    | 'f'::'f'::'a'::tl -> "ッファ" ^ convert tl
    | 'f'::'f'::'i'::tl -> "ッフィ" ^ convert tl
    | 'f'::'f'::'e'::tl -> "ッフェ" ^ convert tl
    | 'f'::'f'::'o'::tl -> "ッフォ" ^ convert tl


    | 'p'::'a'::tl -> "パ" ^ convert tl
    | 'p'::'i'::tl -> "ピ" ^ convert tl
    | 'p'::'u'::tl -> "プ" ^ convert tl
    | 'p'::'e'::tl -> "ペ" ^ convert tl
    | 'p'::'o'::tl -> "ポ" ^ convert tl

    | 'p'::'p'::'a'::tl -> "ッパ" ^ convert tl
    | 'p'::'p'::'i'::tl -> "ッピ" ^ convert tl
    | 'p'::'p'::'u'::tl -> "ップ" ^ convert tl
    | 'p'::'p'::'e'::tl -> "ッペ" ^ convert tl
    | 'p'::'p'::'o'::tl -> "ッポ" ^ convert tl


    | 'p'::'y'::'a'::tl -> "ピャ" ^ convert tl
    | 'p'::'y'::'u'::tl -> "ピュ" ^ convert tl
    | 'p'::'y'::'e'::tl -> "ピェ" ^ convert tl
    | 'p'::'y'::'o'::tl -> "ピョ" ^ convert tl

    | 'p'::'p'::'y'::'a'::tl -> "ッピャ" ^ convert tl
    | 'p'::'p'::'y'::'u'::tl -> "ッピュ" ^ convert tl
    | 'p'::'p'::'y'::'e'::tl -> "ッピェ" ^ convert tl
    | 'p'::'p'::'y'::'o'::tl -> "ッピョ" ^ convert tl


    | 'b'::'a'::tl -> "バ" ^ convert tl
    | 'b'::'i'::tl -> "ビ" ^ convert tl
    | 'b'::'u'::tl -> "ブ" ^ convert tl
    | 'b'::'e'::tl -> "ベ" ^ convert tl
    | 'b'::'o'::tl -> "ボ" ^ convert tl

    | 'b'::'b'::'a'::tl -> "ッバ" ^ convert tl
    | 'b'::'b'::'i'::tl -> "ッビ" ^ convert tl
    | 'b'::'b'::'u'::tl -> "ッブ" ^ convert tl
    | 'b'::'b'::'e'::tl -> "ッベ" ^ convert tl
    | 'b'::'b'::'o'::tl -> "ッボ" ^ convert tl


    | 'b'::'y'::'a'::tl -> "ビャ" ^ convert tl
    | 'b'::'y'::'u'::tl -> "ビュ" ^ convert tl
    | 'b'::'y'::'e'::tl -> "ビェ" ^ convert tl
    | 'b'::'y'::'o'::tl -> "ビョ" ^ convert tl

    | 'b'::'b'::'y'::'a'::tl -> "ッビャ" ^ convert tl
    | 'b'::'b'::'y'::'u'::tl -> "ッビュ" ^ convert tl
    | 'b'::'b'::'y'::'e'::tl -> "ッビェ" ^ convert tl
    | 'b'::'b'::'y'::'o'::tl -> "ッビョ" ^ convert tl


    | 'm'::'a'::tl -> "マ" ^ convert tl
    | 'm'::'i'::tl -> "ミ" ^ convert tl
    | 'm'::'u'::tl -> "ム" ^ convert tl
    | 'm'::'e'::tl -> "メ" ^ convert tl
    | 'm'::'o'::tl -> "モ" ^ convert tl

    | 'm'::'m'::'a'::tl -> "ッマ" ^ convert tl
    | 'm'::'m'::'i'::tl -> "ッミ" ^ convert tl
    | 'm'::'m'::'u'::tl -> "ッム" ^ convert tl
    | 'm'::'m'::'e'::tl -> "ッメ" ^ convert tl
    | 'm'::'m'::'o'::tl -> "ッモ" ^ convert tl


    | 'm'::'y'::'a'::tl -> "ミャ" ^ convert tl
    | 'm'::'y'::'u'::tl -> "ミュ" ^ convert tl
    | 'm'::'y'::'e'::tl -> "ミェ" ^ convert tl
    | 'm'::'y'::'o'::tl -> "ミョ" ^ convert tl

    | 'm'::'m'::'y'::'a'::tl -> "ッミャ" ^ convert tl
    | 'm'::'m'::'y'::'u'::tl -> "ッミュ" ^ convert tl
    | 'm'::'m'::'y'::'e'::tl -> "ッミェ" ^ convert tl
    | 'm'::'m'::'y'::'o'::tl -> "ッミョ" ^ convert tl


    | 'y'::'a'::tl -> "ヤ" ^ convert tl
    | 'y'::'u'::tl -> "ユ" ^ convert tl
    | 'y'::'e'::tl -> "イェ" ^ convert tl
    | 'y'::'o'::tl -> "ヨ" ^ convert tl

    | 'y'::'y'::'a'::tl -> "ッヤ" ^ convert tl
    | 'y'::'y'::'u'::tl -> "ッユ" ^ convert tl
    | 'y'::'y'::'e'::tl -> "ッイェ" ^ convert tl
    | 'y'::'y'::'o'::tl -> "ッヨ" ^ convert tl


    | 'r'::'a'::tl -> "ラ" ^ convert tl
    | 'r'::'i'::tl -> "リ" ^ convert tl
    | 'r'::'u'::tl -> "ル" ^ convert tl
    | 'r'::'e'::tl -> "レ" ^ convert tl
    | 'r'::'o'::tl -> "ロ" ^ convert tl

    | 'r'::'r'::'a'::tl -> "ッラ" ^ convert tl
    | 'r'::'r'::'i'::tl -> "ッリ" ^ convert tl
    | 'r'::'r'::'u'::tl -> "ッル" ^ convert tl
    | 'r'::'r'::'e'::tl -> "ッレ" ^ convert tl
    | 'r'::'r'::'o'::tl -> "ッロ" ^ convert tl


    | 'r'::'y'::'a'::tl -> "リャ" ^ convert tl
    | 'r'::'y'::'u'::tl -> "リュ" ^ convert tl
    | 'r'::'y'::'e'::tl -> "リェ" ^ convert tl
    | 'r'::'y'::'o'::tl -> "リョ" ^ convert tl

    | 'r'::'r'::'y'::'a'::tl -> "ッリャ" ^ convert tl
    | 'r'::'r'::'y'::'u'::tl -> "ッリュ" ^ convert tl
    | 'r'::'r'::'y'::'e'::tl -> "ッリェ" ^ convert tl
    | 'r'::'r'::'y'::'o'::tl -> "ッリョ" ^ convert tl


    | 'w'::'a'::tl -> "ワ" ^ convert tl
    | 'w'::'i'::tl -> "ヰ" ^ convert tl
    | 'w'::'e'::tl -> "ヱ" ^ convert tl
    | 'w'::'o'::tl -> "ヲ" ^ convert tl

    | 'w'::'w'::'a'::tl -> "ッワ" ^ convert tl
    | 'w'::'w'::'i'::tl -> "ッヰ" ^ convert tl
    | 'w'::'w'::'e'::tl -> "ッヱ" ^ convert tl
    | 'w'::'w'::'o'::tl -> "ッヲ" ^ convert tl


    | 'v'::'a'::tl -> "ヷ" ^ convert tl
    | 'v'::'i'::tl -> "ヸ" ^ convert tl
    | 'v'::'u'::tl -> "ヴ" ^ convert tl
    | 'v'::'e'::tl -> "ヹ" ^ convert tl
    | 'v'::'o'::tl -> "ヺ" ^ convert tl

    | 'v'::'v'::'a'::tl -> "ッヷ" ^ convert tl
    | 'v'::'v'::'i'::tl -> "ッヸ" ^ convert tl
    | 'v'::'v'::'u'::tl -> "ッヴ" ^ convert tl
    | 'v'::'v'::'e'::tl -> "ッヹ" ^ convert tl
    | 'v'::'v'::'o'::tl -> "ッヺ" ^ convert tl  


    | 'n'::tl -> "ン" ^ convert tl
    | '-'::tl -> "ー" ^ convert tl


    | '('::tl -> "「" ^ convert tl
    | ')'::tl -> "」" ^ convert tl
    | '['::tl -> "『" ^ convert tl
    | ']'::tl -> "』" ^ convert tl
    | ','::tl -> "、" ^ convert tl
    | '.'::tl -> "。" ^ convert tl
    | '?'::tl -> "？" ^ convert tl
    | '!'::tl -> "！" ^ convert tl
    | '/'::tl -> "・" ^ convert tl


    | '\n'::tl -> "\n" ^ convert tl
    | _::tl -> convert tl
    | _ -> ""
  in
  convert (List.of_seq (String.to_seq str))

let () = Printf.printf "%s" ((hiragana "konnichi wa, watashi no namae wa") ^ (katakana "a-sa-") ^ (hiragana "desu.\n yoroshiku onegaishimasu!"))
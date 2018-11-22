![Duckling Logo](https://github.com/facebook/duckling/raw/master/logo.png)

# Duckling [![Build Status](https://travis-ci.org/facebook/duckling.svg?branch=master)](https://travis-ci.org/facebook/duckling)
Duckling is a Haskell library that parses text into structured data.

```
"the first Tuesday of October"
=> {"value":"2017-10-03T00:00:00.000-07:00","grain":"day"}
```

## Requirements
A Haskell environment is required. We recommend using
[stack](https://haskell-lang.org/get-started).

On macOS you'll need to install PCRE development headers.
The easiest way to do that is with [Homebrew](https://brew.sh/):
```
brew install pcre
```
If that doesn't help, try running `brew doctor` and fix
the issues it finds.

## Quickstart
To compile and run the binary:
```
$ stack build
$ stack exec duckling-example-exe
```
The first time you run it, it will download all required packages.

This runs a basic HTTP server. Example request:
```
$ curl -XPOST http://0.0.0.0:8000/parse --data 'locale=en_GB&text=tomorrow at eight'
```

See `exe/ExampleMain.hs` for an example on how to integrate Duckling in your
project.
If your backend doesn't run Haskell or if you don't want to spin your own Duckling server, you can directly use [wit.ai](https://wit.ai)'s built-in entities.

## Supported dimensions
Duckling supports many languages, but most don't support all dimensions yet
(**we need your help!**).
Please look into [this directory](https://github.com/facebook/duckling/blob/master/Duckling/Dimensions) for language-specific support.

| Dimension | Example input | Example value output
| --------- | ------------- | --------------------
| `AmountOfMoney` | "42€" | `{"value":42,"type":"value","unit":"EUR"}`
| `Distance` | "6 miles" | `{"value":6,"type":"value","unit":"mile"}`
| `Duration` | "3 mins" | `{"value":3,"minute":3,"unit":"minute","normalized":{"value":180,"unit":"second"}}`
| `Email` | "duckling-team@fb.com" | `{"value":"duckling-team@fb.com"}`
| `Numeral` | "eighty eight" | `{"value":88,"type":"value"}`
| `Ordinal` | "33rd" | `{"value":33,"type":"value"}`
| `PhoneNumber` | "+1 (650) 123-4567" | `{"value":"(+1) 6501234567"}`
| `Quantity` | "3 cups of sugar" | `{"value":3,"type":"value","product":"sugar","unit":"cup"}`
| `Temperature` | "80F" | `{"value":80,"type":"value","unit":"fahrenheit"}`
| `Time` | "today at 9am" | `{"values":[{"value":"2016-12-14T09:00:00.000-08:00","grain":"hour","type":"value"}],"value":"2016-12-14T09:00:00.000-08:00","grain":"hour","type":"value"}`
| `Url` | "https://api.wit.ai/message?q=hi" | `{"value":"https://api.wit.ai/message?q=hi","domain":"api.wit.ai"}`
| `Volume` | "4 gallons" | `{"value":4,"type":"value","unit":"gallon"}`

[Custom dimensions](https://github.com/facebook/duckling/blob/master/exe/CustomDimensionExample.hs) are also supported.

## Extending Duckling
To regenerate the classifiers and run the test suite:
```
$ stack build :duckling-regen-exe && stack exec duckling-regen-exe && stack test
```

It's important to regenerate the classifiers after updating the code and before
running the test suite.

To extend Duckling's support for a dimension in a given language, typically 3
files need to be updated:
* `Duckling/<Dimension>/<Lang>/Rules.hs`
* `Duckling/<Dimension>/<Lang>/Corpus.hs`
* `Duckling/Dimensions/<Lang>.hs` (if not already present in `Duckling/Dimensions/Common.hs`)

To add a new language:
* Make sure that the language code used follows the [ISO-639-1 standard](https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes).
* The first dimension to implement is `Numeral`.
* Follow [this example](https://github.com/facebook/duckling/commit/24d3f199768be970149412c95b1c1bf5d76f8240).

To add a new locale:
* There should be a need for diverging rules between the locale and the language.
* Make sure that the locale code is a valid [ISO3166 alpha2 country code](https://www.iso.org/obp/ui/#search/code/).
* Follow [this example](https://github.com/facebook/duckling/commit/1ab5f447d2635fe6d48887a501d333a52adff5b9).

Rules have a name, a pattern and a production.
Patterns are used to perform character-level matching (regexes on input) and
concept-level matching (predicates on tokens).
Productions are arbitrary functions that take a list of tokens and return a new
token.

The corpus (resp. negative corpus) is a list of examples that should (resp.
shouldn't) parse. The reference time for the corpus is Tuesday Feb 12, 2013 at
4:30am.

`Duckling.Debug` provides a few debugging tools:
```
$ stack repl --no-load
> :l Duckling.Debug
> debug (makeLocale EN $ Just US) "in two minutes" [This Time]
in|within|after <duration> (in two minutes)
-- regex (in)
-- <integer> <unit-of-duration> (two minutes)
-- -- integer (0..19) (two)
-- -- -- regex (two)
-- -- minute (grain) (minutes)
-- -- -- regex (minutes)
[Entity {dim = "time", body = "in two minutes", value = RVal Time (TimeValue (SimpleValue (InstantValue {vValue = 2013-02-12 04:32:00 -0200, vGrain = Second})) [SimpleValue (InstantValue {vValue = 2013-02-12 04:32:00 -0200, vGrain = Second})] Nothing), start = 0, end = 14}]
```

## The support rule of dimension


| Dimension | Rules | Description| Example
| --------- | ------------- | --------------------| --------------------
| `Time` | R1 |`the day after tomorrow`| 后天\|後天\|後日
|        | R2 |`relative minutes to\|till\|before <integer> (hour-of-day)`| 两点差5
|        | R3 |`relative minutes to\|till\|before noon\|midnight`| 12点\|0点差5
|        | R4 |`relative minutes after\|past <integer> (hour-of-day)`| 12点15
|        | R5 |`relative minutes after\|past noon\|midnight`| 12点\|0点过5
|        | R6 |`quarter to\|till\|before <integer> (hour-of-day)`| 12点差一刻
|        | R7 |`quarter to\|till\|before noon\|midnight`| 12点差一刻
|        | R8 |`half after\|past <integer> (hour-of-day)`| 5点半
|        | R9 |`half to\|till\|before <integer> (hour-of-day)`| 5时差半
|        | R10 |`half to\|till\|before noon\|midnight`| 12时差半
|        | R11 |`hh:mm (time-of-day)`| 12:20\|0:21
|        | R12 |`this <day-of-week>`| 这周一
|        | R13 |`nth <time> of <time>`| 1月的第一个周末
|        | R14 |`week-end`| 周末
|        | R15 |`last year`| 去年\|上年\|过去一年
|        | R16 |`<dim time> <part-of-day>`| 今天下午\|早晨
|        | R17 |`next <time>`| 下个元旦
|        | R18 |`the day before yesterday`| 前天\|前日
|        | R19 |`next <cycle>`| 下年\|下月\|下周
|        | R20 |`next n <cycle>`| 下2年\|下2月\|下2周\|二周后
|        | R21 |`last <cycle>`| 上个月\|上周
|        | R22 |`afternoon`| 2018年6月1日中午\|上午
|        | R23 |`midnight`| 午夜\|凌晨\|半夜
|        | R24 |`in\|during the <part-of-day>`| 早上时\|晚上时
|        | R25 |`intersect by \",\"`| 周日, 儿童节
|        | R26 |`mm/dd`| 12/22\|2/29
|        | R27 |`year (numeric with year symbol)`| 2018年
|        | R28 |`hhmm (military time-of-day)`| 早上1130
|        | R29 |`hh:mm (time-of-day)`| 12:30
|        | R30 |`intersect`| 昨天12:30
|        | R31 |`<dim time> <part-of-day>`| 10点晚上
|        | R32 |`yyyy-mm-dd`| 2012-10-01
|        | R33 |`morning`| 2018年6月1日早晨\|早上
|        | R34 |`next year`| 明年\|下年
|        | R35 |`this <time>`| 这一年
|        | R36 |`yesterday`| 昨天\|昨日\|
|        | R37 |`last night`| 昨晚\|昨天晚上\|
|        | R38 |`<time-of-day> am\|pm`| 10pm
|        | R39 |`<named-month> <day-of-month>`| 1月2号
|        | R40 |`tonight`| 今晚\|今天晚上
|        | R41 |`tomorrow night`| 明晚\|明天晚上
|        | R42 |`this year`| 今年
|        | R43 |`evening\|night`| 儿童节晚上
|        | R44 |`mm/dd/yyyy`| 12/20/2012
|        | R45 |`<time-of-day> o'clock`| 上午10点
|        | R46 |`other rules` | 节日规则




## License
Duckling is BSD-licensed. We also provide an additional patent grant.

## Grammar

This is the rough EBNF grammar for fakt files:

* *Comments are excluded here but are like Python or Bash comments.* Any time a pound-sign or "hashtag" symbol `#` is encountered the rest of the
  line is ignored.

```
<pkg>                  ::= "pkg" <name> (<rule-or-property>)*

<name>                 ::= <identifier> ("." <identifier>)*

<rule-or-property>     ::= <condition-lowest> "{" (<rule-or-property>)* "}"
                         | <property> 

<condition-lowest>     ::= <condition-low> "," <condition-low>
                         | <condition-low> "or" <condition-low>

<condition-low>        ::= <condition-high> "xor" <condition-high>

<condition-high>       ::= <condition-highest> "and"? <condition-lowest>

<condition-highest>    ::= "(" <condition-lowest> ")"
                         | "not" <condition-highest>
                         | "!" <condition-highest>

<fact>                 ::= <name> "[" <args> "]"
                         | <name>

<args>                 ::= <string> ("," <string>)*

<property>             ::= <name> ":" <value>

<value>                ::= <array>
                         | <map>
                         | <string>

<array>                ::= "[" <value> ("," <value>)* "]"

<map>                  ::= "{" <key-value-pair> ("," <key-value-pair>)* "}"

<key-value-pair>       ::= <string> ":" <value>

<string>               ::= <identifier>
                         | <quoted-string>
                         | <double-quoted-string>
                         | <unquoted-string>

<identifier>           ::= /\p{Letter}[\p{Number}]*/

<quoted-string>        ::= /'[^']*'/

<double-quoted-string> ::= /"[^"]*"/

<unquoted-string>      ::= /[^\p{Letter}][^\s!:,.(){}[\]]+/
```

## Examples

```
# Is it the weekend?
pkg test.package

weekend: false

saturday, sunday {
    weekend: true
}
```

```
# It can be on one line!
pkg test.package weekend:false saturday,sunday{weekend:true}
```
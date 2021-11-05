## Grammar

The grammar of fakt files is defined below, using `|` for alternatives, `[]` for optional, `{}` for repeated,
and `()` for grouping.
Tokens are either quoted literals (i.e. `"pkg"`) or defined as regular expressions enclosed in forward slashes (`//`).

```
Pkg                    = "pkg" Name { RuleOrProperty } ;

Name                   = Identifier { "." Identifier } ;

RuleOrProperty         = ConditionLowest "{" { RuleOrProperty } "}"
                       | Property
                       ;

ConditionLowest        = ConditionLow "," ConditionLow
                       | ConditionLow "or" ConditionLow
                       ;

ConditionLow           = ConditionHigh "xor" ConditionHigh ;

ConditionHigh          = ConditionHighest [ "and" ] ConditionLowest ;

ConditionHighest       = "(" ConditionLowest ")"
                       | "not" ConditionHighest
                       | "!" ConditionHighest
                       ;

Fact                   = Name [ "[" Args "]" ] ;

Args                   = Primitive { "," Primitive } ;

Property               = Name ":" Value ;

Value                  = Array | Map | Primitive ;

Array                  = "[" Value { "," Value } "]" ;

Map                    = "{" KeyValuePair { "," KeyValuePair } "}" ;

KeyValuePair           = String ":" Value ;

Primitive              = "true" | "false" | String | INTEGER | UNSIGNED_INTEGER | FLOAT ;

String                 = IDENTIFIER | QUOTED_STRING | DOUBLE_QUOTED_STRING | UNQUOTED_STRING;

INTEGER                = /-?[\p{Number}i?]+/ ;

UNSIGNED_INTEGER       = /[\p{Number}u?]+/ ;

FLOAT                  = /-?\p{Number}*.[eE][+-]?\p{Number}+f?/
                       | /-?\p{Number}+[eE][+-]?\p{Number}+f?/
                       ;

IDENTIFIER             = /\p{Letter}[\p{Number}]*/ ;

QUOTED_STRING          = /'[^']*'/ ;

DOUBLE_QUOTED_STRING   = /"[^"]*"/ ;

UNQUOTED_STRING        = /[^\p{Letter}][^\s!:,.(){}[\]]+/ ;
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

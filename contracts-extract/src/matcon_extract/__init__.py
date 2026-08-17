"""matcon-extract: deterministic entity extraction from contract text.

Four pattern-and-lookup extractors that emit character-offset spans into a common schema. The
distinguishing property, and the reason the package exists, is that none of them can supply a value
the text does not contain: every date requires a written year, every term requires a number and a
unit. A grammar-based extractor will resolve "May 1 of each year" by supplying a year of its own,
and if that year comes from the clock the same document yields different answers on different days.

    Label    Extractor    Extras
    DATE     dateregex    DateValue
    TERM     dateregex    TermN, TermUnit, TermYears
    MONEY    moneyregex   Amount, Currency
    GPE      gazetteer    GeoKey, IsWord, NParent, Iso2, MatchKind
    REDACT   redaction    --

Offsets are 0-based, half-open, over code points: text[Start:Stop] == Span exactly.
"""

__version__ = "0.1.0"

ENGINE = "matcon"

#: Which module owns each label. One module may own several; no label has two owners.
LABEL_OWNER = {
    "DATE": "dateregex",
    "TERM": "dateregex",
    "MONEY": "moneyregex",
    "GPE": "gazetteer",
    "REDACT": "redaction",
}

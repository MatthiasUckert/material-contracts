"""What the gazetteer must find, what it must gate away, and what it must refuse to resolve.

Every test here needs the packaged lookup, so the whole module skips where it is absent -- the
package installs and its other suites pass on a machine that has not copied the 6.3 MB data file in
yet.

THE GATE IS THE SUBJECT, not the dictionary. A gazetteer that matched every name would be a lookup
rather than an extractor; what makes this one usable is that it keeps 31,925 dictionary-word place
names and licenses them on context. So the cases are mostly about which names an anchor rescues and
which it does not.
"""
from __future__ import annotations

import pytest

from matcon_extract import _io, gazetteer as gz

pytestmark = pytest.mark.skipif(
    not gz.DEFAULT_LOOKUP.exists(),
    reason=f"geo lookup absent at {gz.DEFAULT_LOOKUP}",
)


@pytest.fixture(scope="module", autouse=True)
def index():
    """Build the index once for the module rather than once per test."""
    gz.init_worker(0, ("GPE",), str(gz.DEFAULT_LOOKUP))


def extract(text):
    """(Span, LabelRaw, MatchKind) for one text."""
    return sorted((r[3], r[5], r[13]) for r in gz.extract_one(("T", text)) if r[1] is not None)


def cols(text, span):
    """The full extras tuple for one named span."""
    for r in gz.extract_one(("T", text)):
        if r[3] == span:
            return dict(GeoKey=r[8], IsWord=r[9], NParent=r[10], Iso2=r[11], Iso3=r[12],
                        MatchKind=r[13])
    return None


# 1. Anchors -------------------------------------------------------------------------------------

def test_a_state_is_an_anchor_even_though_it_is_a_dictionary_word():
    """The original approach dropped every IsWord name and lost 40 of the 50 states with it,
    including Delaware, California and Texas. A state-level geography built that way is a sample of
    state names by orthography."""
    for state in ("Delaware", "California", "Texas", "Washington"):
        got = extract(f"organized under the laws of {state}")
        assert got == [(state, "US State", "anchor")], state


def test_a_country_is_an_anchor():
    assert extract("the Company is incorporated in France") == [("France", "Country", "anchor")]


def test_an_ambiguous_country_is_demoted_out_of_the_anchor_set():
    """Without this, every "turkey" and every person called Jordan is a geopolitical entity."""
    for word in ("Turkey", "Jordan", "Chad"):
        assert extract(f"a contract with {word} and others") == [], word


# 2. Corroboration -------------------------------------------------------------------------------

def test_a_city_abutting_its_state_is_kept():
    got = extract("offices in Palo Alto, California")
    assert ("Palo Alto", "US Populated Place", "corroborated") in got
    assert ("California", "US State", "anchor") in got


def test_a_street_name_beside_the_same_anchor_is_not():
    """PLAIN PROXIMITY CANNOT GATE A DICTIONARY WORD. In "400 Hamilton Avenue, Palo Alto,
    California" the state sits within forty characters of Hamilton, Avenue and Palo Alto alike.
    What distinguishes the city is that it ABUTS the state with only a comma between."""
    got = [span for span, _k, _m in extract("400 Hamilton Avenue, Palo Alto, California")]
    assert "Palo Alto" in got
    assert "Hamilton" not in got
    assert "Avenue" not in got


def test_a_dictionary_word_place_with_no_anchor_is_dropped():
    """Enterprise, Superior, Eagle, Mobile, Reading and Bath are all US populated places."""
    assert extract("we went reading in the park") == []
    assert extract("the Enterprise division reported a loss") == []


def test_a_document_with_no_anchor_emits_nothing():
    """A city named with no jurisdiction anywhere near it is not evidence about where a contracting
    party sits, and that is the distinction the geography variable has to support."""
    assert extract("the parties met in Springfield to discuss terms") == []


# 3. Matching ------------------------------------------------------------------------------------

def test_names_match_as_tokens_not_substrings():
    """READING inside PROOFREADING cannot fire, which is why no word-boundary regex is needed."""
    assert extract("proofreading services in Delaware") == [("Delaware", "US State", "anchor")]


def test_longest_name_wins_and_consumes_the_shorter():
    got = [span for span, _k, _m in extract("incorporated in New York")]
    assert "New York" in got
    assert "York" not in got


def test_class_precedence_reads_a_us_filing():
    """New York is a state rather than one of the eight populated places sharing the name, and
    Georgia is a state rather than a country. Both are right in a US filing."""
    assert extract("under the laws of New York")[0][1] == "US State"
    assert extract("under the laws of Georgia")[0][1] == "US State"


# 4. The resolved columns ------------------------------------------------------------------------

def test_a_state_resolves_its_iso2():
    c = cols("laws of California", "California")
    assert c["Iso2"] == "US-CA"
    assert c["Iso3"] == "USA"
    assert c["NParent"] == 1
    assert c["MatchKind"] == "anchor"


def test_a_country_resolves_iso3_and_never_iso2():
    """RESOLUTION IS SCOPED TO THE WINNING CLASS, and this is the case that proves why.

    Collapsing FRANCE across ALL its lookup rows and taking the single distinct value gives
    Iso2 = US-ID: there is a town called France in Idaho, the country carries no Iso2, so the town's
    is the only non-null value in the group. Scoped to the winning class the country resolves ISO3
    and nothing else.
    """
    c = cols("incorporated in France", "France")
    assert c["Iso3"] == "FRA"
    assert c["Iso2"] is None


def test_an_ambiguous_city_refuses_to_resolve_and_says_why():
    """SPRINGFIELD is a populated place in 33 states. Emitting one of them would be inventing a
    value; NParent is what tells a consumer the column is absent for a reason."""
    c = cols("offices in Springfield, Illinois", "Springfield")
    assert c["NParent"] > 1
    assert c["Iso2"] is None


def test_an_unambiguous_city_does_resolve():
    c = cols("offices in Mobile, Alabama", "Mobile")
    assert c["NParent"] == 1
    assert c["Iso2"] == "US-AL"


def test_geokey_is_the_lookup_key_not_the_surface_form():
    """So a consumer stops re-deriving the normalisation from Span, and cannot derive it
    differently."""
    assert cols("laws of Delaware", "Delaware")["GeoKey"] == "DELAWARE"
    assert cols("laws of DELAWARE", "DELAWARE")["GeoKey"] == "DELAWARE"


def test_only_capitalised_tokens_are_probed():
    """An UNDOCUMENTED GATE, asserted so it stays visible. scan() probes a token only where its
    first character is upper case, so a lower-case place name never matches at all. That is a real
    filter -- "we went reading in the park" is safe for this reason as much as for the anchor rule
    -- and a reader of the class descriptions would not guess it.
    """
    assert extract("laws of delaware") == []
    assert extract("laws of Delaware") == [("Delaware", "US State", "anchor")]


def test_is_word_records_whether_corroboration_was_required():
    assert cols("laws of California", "California")["IsWord"] == 1
    assert cols("offices in Palo Alto, California", "Palo Alto")["IsWord"] == 0


# 5. Version -------------------------------------------------------------------------------------

def test_the_lookup_is_part_of_the_version():
    """A gazetteer's spans are a function of its dictionary as much as of its gate. A SPEC naming
    only the rules would let a rebuilt lookup change the output under an unchanged model tag."""
    assert "lookup" in gz.SPEC
    assert gz.SPEC["lookup"] == _io.file_hash(gz.DEFAULT_LOOKUP)
    assert gz.SPEC["lookup"] != "absent"


def test_the_windows_are_in_the_spec_not_on_the_command_line():
    """They change output. Leaving them as runtime arguments meant gazetteer-v1 named a rule set
    that two runs could disagree about."""
    assert gz.SPEC["state_window"] == gz.STATE_WINDOW
    assert gz.SPEC["word_window"] == gz.WORD_WINDOW


def test_offsets_index_the_raw_text():
    text = "offices in Palo Alto, California and in Wilmington, Delaware"
    for r in gz.extract_one(("T", text)):
        if r[1] is None:
            continue
        assert text[r[1]:r[2]] == r[3]

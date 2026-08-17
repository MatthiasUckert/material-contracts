"""Build the fixture that ships in the wheel.

Five short documents of ordinary contract boilerplate, written for this purpose rather than drawn
from EDGAR, so the package can be installed and run by someone with no access to the corpus. The
REPO sample -- five per ClassDetailed on seed 42, the same seed the Check-NER-* scripts use -- is a
different artifact with a different job and is built by the R side.

Every construction the extractors are supposed to catch appears at least once, and several they are
supposed to NOT catch appear too: an invalid date, a section number, a notice period that is not a
duration.
"""
import pandas as pd

DOCS = [
    ("FIX0001", """
EMPLOYMENT AGREEMENT

This Employment Agreement is made and entered into as of the 9th day of January, 2014, by and
between Northwind Trading Corp., a Delaware corporation, and the Executive.

1. Term. The initial term of this Agreement shall commence on March 3, 2011 and shall continue for
five (5) years thereafter, unless earlier terminated in accordance with Section 3-1 hereof.

2. Compensation. The Executive shall receive an annual base salary of $250,000. A bonus of up to
$50,000 may be awarded. Options vest on the third anniversary of the Effective Date.

3. Notice. Either party may terminate upon a thirty (30) day period of written notice.
"""),
    ("FIX0002", """
LICENSE AGREEMENT dated 10/1/1999.

The license granted herein shall be perpetual and shall remain in full force and effect until
terminated. Royalties of USD 1,250,000.00 are payable on 2011-03-03 and thereafter on 3.3.2012.

Exhibit 10-15 is incorporated by reference. See also February 30, 2011, which is not a date.
"""),
    ("FIX0003", """
CREDIT AGREEMENT

This Agreement, executed this 20th day of December, 2005, provides for a revolving facility in an
aggregate principal amount of $10,000,000 (ten million dollars).

The Commitment shall terminate on 12/31/2010. The facility has a three (3)-year term.

Certain information has been omitted pursuant to a request for confidential treatment and filed
separately with the Commission. [***]
"""),
    ("FIX0004", """
SUPPLY AGREEMENT

Effective Sept 3, 2011. Deliveries commence in March 2012 and continue for a period of ten (10)
years. Prices are stated in EUR 45,000 per unit.

Facilities are located in Springfield, Illinois and in Wilmington, Delaware. The Purchaser
maintains an office in Washington.
"""),
    ("FIX0005", """
AMENDMENT NO. 2

This Amendment, dated as of 3/3/11, amends the Agreement originally dated Mar. 3, 2011.

No other term is stated. Portions of this exhibit have been omitted and such excluded
information is indicated by [***].

[CONFIDENTIAL TREATMENT REQUESTED]. The royalty rate of *** applies. Schedule [1.4] is
attached. [INTENTIONALLY OMITTED]. The unit price is [___] and the buyer is [NAME].

[Remainder of page intentionally left blank]
*****
"""),
]

df = pd.DataFrame(DOCS, columns=["DocID", "TextRaw"])
df["TextRaw"] = df["TextRaw"].str.strip()
df.to_parquet("tests/data/sample.parquet", index=False)
print(f"{len(df)} docs, {df['TextRaw'].str.len().sum()} chars -> tests/data/sample.parquet")

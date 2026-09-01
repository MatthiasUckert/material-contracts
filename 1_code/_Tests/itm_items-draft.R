# 0. The item vocabulary ---------------------------------------------------------------------------------------------------

#: EVERY 8-K ITEM CODE THAT EXISTS, UNDER BOTH TAXONOMIES. 46 rows: 33 dotted codes from the form as
#: reformed on 23 August 2004, and 13 undotted ones from the form before it. That is the complete
#: legal vocabulary of each, and a corpus-wide count returns exactly these and nothing else -- which
#: is the check that the parser is reading codes rather than inventing them.
#:
#: ItemKind IS THE RESEARCH DECISION IN THIS FILE and the reason it is declared rather than derived.
#: Three kinds, not two:
#:
#:   Voluntary   the registrant chooses whether and when. 2.02 and 7.01 are FURNISHED rather than
#:               filed, which is the Section 18 safe harbour; 8.01 is filed but the rule says the
#:               registrant "may, at its option, disclose". Pre-reform these are 12, 9 and 5.
#:   Exhibits    not a disclosure event at all -- it says the filing carries financial statements or
#:               exhibits. Counting it as either of the others makes every contract-bearing 8-K look
#:               one item busier than it is.
#:   Mandatory   triggered by an event outside the registrant's control, four business days.
#:
#: 2.02 IS THE CONTESTED ONE and the dictionary says so. It is required ONCE the registrant announces
#: results, but the announcement is discretionary, which is why the bundling literature counts it as
#: voluntary. ItemFurnished separates the legal cut from the discretion cut, so a robustness column
#: that drops 8.01 or keeps only the furnished pair costs a filter rather than a re-read.
#:
#: ItemSuccessor IS THE PRE-TO-POST CROSSWALK and it is what makes a pre/post count comparable at all.
#: Four matter for the voluntary measure: 5 -> 8.01, 9 -> 7.01, 12 -> 2.02, 7 -> 9.01. It is NA on
#: post-reform rows, and NA on Item 13, which the reform did not carry forward.
#:
#: THE LABEL IS A SHORT CANONICAL FORM, NOT THE FILING'S OWN. Filers punctuate the regulated titles
#: inconsistently -- one spells Item 10 with a full stop where an apostrophe belongs -- so the raw
#: label stays in the data as ItemLabelRaw and this one is what tables print.
#:
#: NO EFFECTIVE DATES ARE DECLARED. Several codes postdate their own era: 9 arrives with Regulation FD
#: in 2000, 10, 11, 12 and 13 in 2003, 6.06 in 2006, 5.08 in 2010, 1.04 in 2011 and 1.05 in December
#: 2023. Hand-entering 46 dates is a second thing to keep right; the runbook reports first-seen and
#: last-seen year per code from the corpus instead, which validates the era and surfaces the mid-era
#: additions without anyone having to remember them.
.itm_items <- tibble::tribble(
  ~ItemCode, ~ItemEra, ~ItemKind,   ~ItemFurnished, ~ItemSuccessor, ~ItemLabel,

  # -- Post-reform: Section 1, the registrant's business and operations ------------------------------
  "1.01",    "Post",   "Mandatory", 0L,             NA_character_,  "Entry into a Material Definitive Agreement",
  "1.02",    "Post",   "Mandatory", 0L,             NA_character_,  "Termination of a Material Definitive Agreement",
  "1.03",    "Post",   "Mandatory", 0L,             NA_character_,  "Bankruptcy or Receivership",
  "1.04",    "Post",   "Mandatory", 0L,             NA_character_,  "Mine Safety - Shutdowns and Patterns of Violations",
  "1.05",    "Post",   "Mandatory", 0L,             NA_character_,  "Material Cybersecurity Incidents",

  # -- Post-reform: Section 2, financial information -------------------------------------------------
  "2.01",    "Post",   "Mandatory", 0L,             NA_character_,  "Completion of Acquisition or Disposition",
  "2.02",    "Post",   "Voluntary", 1L,             NA_character_,  "Results of Operations and Financial Condition",
  "2.03",    "Post",   "Mandatory", 0L,             NA_character_,  "Creation of a Direct Financial Obligation",
  "2.04",    "Post",   "Mandatory", 0L,             NA_character_,  "Triggering Events That Accelerate an Obligation",
  "2.05",    "Post",   "Mandatory", 0L,             NA_character_,  "Costs Associated with Exit or Disposal Activities",
  "2.06",    "Post",   "Mandatory", 0L,             NA_character_,  "Material Impairments",

  # -- Post-reform: Section 3, securities and trading markets ----------------------------------------
  "3.01",    "Post",   "Mandatory", 0L,             NA_character_,  "Notice of Delisting or Listing Failure",
  "3.02",    "Post",   "Mandatory", 0L,             NA_character_,  "Unregistered Sales of Equity Securities",
  "3.03",    "Post",   "Mandatory", 0L,             NA_character_,  "Material Modifications to Rights of Security Holders",

  # -- Post-reform: Section 4, accountants and financial statements ----------------------------------
  "4.01",    "Post",   "Mandatory", 0L,             NA_character_,  "Changes in Registrant's Certifying Accountant",
  "4.02",    "Post",   "Mandatory", 0L,             NA_character_,  "Non-Reliance on Issued Financial Statements",

  # -- Post-reform: Section 5, corporate governance and management -----------------------------------
  "5.01",    "Post",   "Mandatory", 0L,             NA_character_,  "Changes in Control of Registrant",
  "5.02",    "Post",   "Mandatory", 0L,             NA_character_,  "Departure or Election of Directors or Officers",
  "5.03",    "Post",   "Mandatory", 0L,             NA_character_,  "Amendments to Articles; Change in Fiscal Year",
  "5.04",    "Post",   "Mandatory", 0L,             NA_character_,  "Temporary Suspension of Trading Under Benefit Plans",
  "5.05",    "Post",   "Mandatory", 0L,             NA_character_,  "Amendments to the Code of Ethics, or Waiver of It",
  "5.06",    "Post",   "Mandatory", 0L,             NA_character_,  "Change in Shell Company Status",
  "5.07",    "Post",   "Mandatory", 0L,             NA_character_,  "Submission of Matters to a Vote of Security Holders",
  "5.08",    "Post",   "Mandatory", 0L,             NA_character_,  "Shareholder Director Nominations",

  # -- Post-reform: Section 6, asset-backed securities -----------------------------------------------
  "6.01",    "Post",   "Mandatory", 0L,             NA_character_,  "ABS Informational and Computational Material",
  "6.02",    "Post",   "Mandatory", 0L,             NA_character_,  "Change of Servicer or Trustee",
  "6.03",    "Post",   "Mandatory", 0L,             NA_character_,  "Change in Credit Enhancement or External Support",
  "6.04",    "Post",   "Mandatory", 0L,             NA_character_,  "Failure to Make a Required Distribution",
  "6.05",    "Post",   "Mandatory", 0L,             NA_character_,  "Securities Act Updating Disclosure",
  "6.06",    "Post",   "Mandatory", 0L,             NA_character_,  "Static Pool",

  # -- Post-reform: Sections 7, 8 and 9, the three that are not event-triggered -----------------------
  "7.01",    "Post",   "Voluntary", 1L,             NA_character_,  "Regulation FD Disclosure",
  "8.01",    "Post",   "Voluntary", 0L,             NA_character_,  "Other Events",
  "9.01",    "Post",   "Exhibits",  0L,             NA_character_,  "Financial Statements and Exhibits",

  # -- Pre-reform: the form as it stood before 23 August 2004 -----------------------------------------
  "1",       "Pre",    "Mandatory", 0L,             "5.01",         "Changes in Control of Registrant",
  "2",       "Pre",    "Mandatory", 0L,             "2.01",         "Acquisition or Disposition of Assets",
  "3",       "Pre",    "Mandatory", 0L,             "1.03",         "Bankruptcy or Receivership",
  "4",       "Pre",    "Mandatory", 0L,             "4.01",         "Changes in Registrant's Certifying Accountant",
  "5",       "Pre",    "Voluntary", 0L,             "8.01",         "Other Events",
  "6",       "Pre",    "Mandatory", 0L,             "5.02",         "Resignations of Registrant's Directors",
  "7",       "Pre",    "Exhibits",  0L,             "9.01",         "Financial Statements and Exhibits",
  "8",       "Pre",    "Mandatory", 0L,             "5.03",         "Change in Fiscal Year",
  "9",       "Pre",    "Voluntary", 1L,             "7.01",         "Regulation FD Disclosure",
  "10",      "Pre",    "Mandatory", 0L,             "5.05",         "Amendments to the Registrant's Code of Ethics",
  "11",      "Pre",    "Mandatory", 0L,             "5.04",         "Temporary Suspension of Trading Under Benefit Plans",
  "12",      "Pre",    "Voluntary", 1L,             "2.02",         "Results of Operations and Financial Condition",
  "13",      "Pre",    "Mandatory", 0L,             NA_character_,  "Receipt of an Attorney's Written Notice"
)

#: THE THREE KINDS, REGISTERED SO A TYPO IN THE TABLE ABOVE CANNOT PASS.
.itm_item_kinds <- c("Voluntary", "Mandatory", "Exhibits")

#: THE TWO ERAS AND THE DATE THAT SEPARATES THEM. 2004 is not assigned to either: a filing from that
#: year is only interpretable once you know which side of 23 August it fell on, and reporting it as
#: its own row is also the check that the boundary is in the right place.
.itm_reform_date <- as.Date("2004-08-23")

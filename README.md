# Auto Pricing: Frequency-Severity GLMs and a Rating Plan (freMTPL2)

## Business question
How should a personal auto insurer price a book of policies? This project builds a
frequency-severity model, validates it out of sample, and turns it into a rating plan
with a loaded premium, documenting where the data can and cannot support conclusions.

## Data
- freMTPL2: French motor third-party liability policies and claims (Dutang and
  Charpentier, CASdatasets). This is the Kaggle copy: 678,013 policies and 26,639 claim records.
- It is not California data, and it has no annual mileage or driving-experience variable.
- Raw data is not included (see How to run).

## Cleaning decisions
- 195 claim records with no matching policy were dropped (about 1.3% of claim dollars).
- A "claim" means a claim with a dollar amount: the frequency file counts 36,102 claims but
  only 26,444 have amounts (9,117 policies are affected). Frequency is therefore 0.074
  per policy-year on priced claims, not 0.10.
- Exposure capped at 1 year (0.18% of policies); claim counts capped at 4 for frequency (8 policies).
- Severity capped at 100,000 per claim; the excess is priced with a flat load (below).

## Key findings
1. **Bonus-malus dominates frequency, and it interacts with age.** One-way, ages 18-20 have
   3.2x the frequency of ages 41-50; after controlling for bonus-malus, that falls to 1.1x
   (interval includes 1.0). At the same bonus-malus level (61-80), frequency rises with age,
   from 0.070 (21-25) to 0.199 (71+), the opposite of the one-way pattern. One hypothesis,
   untested: the same score means "recent driver" for the young and "claims history" for the old.
2. **Frequency model.** Poisson GLM with a log-exposure offset, bonus-malus x age (5 bands),
   density area, vehicle age, fuel, power, and brand. Holdout deviance fell 4.7% from the
   null model; adding the age x bonus-malus interaction gained another 0.17%. Dispersion was
   1.73, so intervals were scaled. Region improved holdout deviance by only about 0.08% and
   was left out.
3. **Severity is flat.** About 41% of claims fall on four fixed amounts. A gamma severity GLM
   was 0.2% worse on the holdout and 0.5% worse in 5-fold cross-validation than one average
   severity (1,864 capped). In-sample F-tests looked significant but used a dispersion (about 1.4)
   far below the Pearson estimate (8.2), so I did not rely on them.
4. **Pure premium validation (holdout, capped losses).** Overall actual/expected 1.011; Gini
   0.326. Actual loss cost rose from about 77 (lowest decile) to 406 (highest). A bootstrap showed
   no Gini gain from the severity model (difference median -0.008, 95% interval -0.018 to +0.0004).
5. **Rating plan.** Relativities from the full-data model; sparse age x bonus-malus cells
   blended with the no-interaction model using Z = sqrt(claims / 1,082); non-monotone cells pooled so
   a worse record never pays less. Rate balance 0.9985.
6. **Excess layer.** 41 claims exceed 100,000, totalling about 10.6M of excess. The three largest
   claims make up 61% of it, and all three fall in the top predicted-risk decile. The capped layer
   is well calibrated in every decile (A/E 0.93-1.06). The excess is not credible by segment.
   The base premium (about 197) would be about 7% lower without the largest claim and about 11%
   lower without the three claims over 1M. The relativities do not change, because the load is flat.

## Illustrative premium
Base risk (bonus-malus 50, age 41-50, area C, vehicle age 6-10, regular fuel, power 6, brand B1):
loss cost about 128, base premium about 197 at an assumed 65% permissible loss ratio (30% expenses
+ 5% profit, an illustrative assumption). Quotes range from about 142 (experienced, claim-free,
rural) to about 1,188 (new driver, urban, powerful diesel). Currency not verified [presumably euros].

## How a California filing would differ
- Prop 103 requires auto rates to weight driving safety record, annual miles, and years of
  driving experience in that order of importance, with other factors allowed only if adopted by
  regulation (see 10 CCR 2632.5, https://www.law.cornell.edu/regulations/california/10-CCR-2632.5).
- This dataset has no mileage or experience variable, and bonus-malus is claims-based, not a
  motor-vehicle-record measure, so this plan could not be filed as is.
- Bonus-malus has the widest relativity spread by far (about 10x from lowest to highest); the other
  factors range from about 1.3x to 1.6x. That is directionally record-first, but I did not test
  the regulation's weighting rules.
- California is a prior-approval state, so rates would face regulatory and intervenor review.
  [Source for this is a study guide; verify.]
- The permitted optional factors change over time; check the current regulation.

## Limitations
- French third-party liability data; no dates, so no trend or development.
- No loss adjustment expenses, reinsurance, or investment income; flat excess load from 41 claims.
- Claim definition (priced claims only) affects the frequency/severity split, not total loss cost.
- Expense and profit loads are assumptions, not company data.
- Rate balance is in-sample; validation was on capped losses only.

## How to run
1. Download freMTPL2freq.csv and freMTPL2sev.csv (Kaggle: "freMTPL2 French Motor TPL
   Insurance Claims") into a data/ folder.
2. Run the numbered scripts in order. Requires R and tidyverse.

## Files
Numbered scripts (adjust to your names) and result tables: age_bm_relativities.csv,
other_relativities.csv, excess_load_sensitivity.csv, excess_layer_by_decile.csv;
charts: lift_chart.png, bm_frequency.png
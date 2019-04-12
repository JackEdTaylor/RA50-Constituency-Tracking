# RA50 Constituency Tracking

This is some very ugly R code that I used to generate a .gif animation of the proportion of signatures from each constituency on the Revoke Article 50 petition over time.

It shows how as time goes on these proportions increasingly reflect the population of the constituency.

The script produces:

* `yesterday.png`: how the proportion of votes looked in the most recently available dataset
* `animation.gif`: tracking changes over each day

The animation is produced using temporary html files which are deleted when no longer needed.

Days with no data (no new signatures) are excluded.
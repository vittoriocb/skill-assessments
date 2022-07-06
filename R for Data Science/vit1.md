This is an excellent first pass! You've clearly down this type of work before. Great use of the tidy style and I like that you used inline R code in your Markdown sections too. Most of the feedback will largely be nitpicky things.

Markdown things to fix:
## Recall that one of the requirements is that report should be "user-friendly for a non-programmer". We want to mainly show relevant information in a clear format, and exclude anything that may confuse a non-programmer reader:

> Section I Q1: Those distribution plots are basically unreadable. You probably only want 2 or 3 columns in your grid of plots

Adjusted to 2 columns

> Section I Q2&5, and Section II Q3&4: The kable columns with mostly NAs and row counts aren't needed

Fixed

> Section I Q3: Avoid raw prints and put your results in something presentable (a table or otherwise)

Fixed

> Section II Q3: The question is asking for "highest average ranking across each time point" where you have just used raw pop. density

Now using average ranking of pop. density instead of average pop. density

> Also, with your inline R code `r top_countries`, you probably want to use a different separator than `, ` since some country names have that separator within them, which makes things unclear

Changed separator to `; `


> Section II Q4: We want our plots to have clarity on what metric is being plotted (ie. absolute increase, percent or multiplier). It's also helpful to have the end date in the plot titles too

Fixed


## Code things to fix:

> Line 47: `readr::read_csv` will read a CSV as a tibble

Fixed

> Line 50: Try to maintain consistency with using the tidy style. You can drop a column via `dplyr::select(-column_to_drop)`

Fixed

> Line 430: Can you come up with a `tidy` way to extract that information?

Fixed
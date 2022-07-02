# R Programming review: <https://github.com/vittoriocb/skill-assessments/tree/main/R%20Programming> 

**Commit reviewed**: [b7f4b39](https://github.com/vittoriocb/skill-assessments/commit/b7f4b3939cb8ecf7d15605aa854949640ea317db)

> 1. Functioning: The programme fails to comply to the Person vs Computer requirement as mentioned in the [skill assessment](https://github.com/Bioinformatics-Research-Network/skill-assessments/tree/main/R%20Programming).

I previously missed this instruction, fixed now


> 2. Formatting style:  
The code styling needs improvement, example, the character limit for each line should be around 80, `Line 22` you have 99 characters, which is not good.
The naming practices for variables like `STATUS.WON` is incorrect as it is not a method. There are errors with `whitespaces` in many places.
    - <https://rstudio-pubs-static.s3.amazonaws.com/390511_286f47c578694d3dbd35b6a71f3af4d6.html>
    - <https://web.stanford.edu/class/cs109l/unrestricted/resources/google-style.html>
	- <https://style.tidyverse.org/syntax.html#long-lines>
You can also use [auto-style in R studio](https://styler.r-lib.org/index.html).

Added and ran packages styler and lintr to fix this and all lint issues

> 3. Coding Practices:
    For the `if..else` don't specify `T` Example: `Line 25-27`.
	```
	if(any(cond == T)) {
	return(STATUS.WON)
	}
	```

Fixed
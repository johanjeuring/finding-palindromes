## What does this MR do and why?

%{first_multiline_commit}

## References

<!--
Include [links](https://handbook.gitlab.com/handbook/communication/#start-with-a-merge-request:~:text=Cross%20link%20issues,alternate%20if%20duplicate.) to any resources that are relevant to this MR.
This will give reviewers and future readers helpful context.
-->

## Screenshots or screen recordings

<!---
Screenshots are required for UI changes, and strongly recommended for all other merge requests.
-->

| Before | After |
| ------ | ----- |
|        |       |

<!--
OPTIONAL: For responsive UI changes, you can use the viewport size table below.
Delete this table if not needed or delete rows that are not relevant to your changes.

| Viewport size   | Before     | After      |
| ----------------| ---------- | ---------- |
| `xs` (<576px)   |            |            |
| `sm` (>=576px)  |            |            |
| `md` (>=768px)  |            |            |
| `lg` (>=992px)  |            |            |
| `xl` (>=1200px) |            |            |
-->

## How to set up and validate locally

For example:

1. Run

```
cabal run palindromes -- input.txt -Q -L
```

to validate the longest palindrome is not found correctly

1. Check that the largest palindrome found is ...

## MR acceptance checklist

- [ ] The code in the merge request completes all the elements in the ticket it is linked to.
- [ ] The code is sufficiently tested, use HPC as described in the DEVELOPER.md to check this.
- [ ] All tests pass.
- [ ] There are no new HLint warnings.
- [ ] The code adheres to the style guide and is properly formatted.
- [ ] The code structure makes sense and can not be easily improved.
- [ ] No unused/commented out code and no TODO comments.

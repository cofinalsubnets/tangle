tangle
======
Tangle is a command-line tool for automatic random-ish text generation based on supplied corpi. It reads text from standard input, uses it to construct a (by default) first-order Markov model, then uses the model to generate novel text. Example usage:

```shell
tangle -w 5000 -h 2 my/seed/text.txt
```
where the flags denote, respectively, up to 5000 words of output, and use a 2nd-order model.


# quote-db

`quote-db` is a simple solution for all ye, who keep a list of favourite
(literature / song lyric) quotations.

`quote-db` can
* search quotes by author, source, content or any combination of them
* output to (prettified) plain text
* output to LaTeX
* output to HTML 5 (with CSS)

The list / database is a CSV file in the following format

```csv
author,source title,location,"quote"
```

where the `location` parameter is optional and has the following syntax:

```
<location> ::= <number>
            |  <number>f
            |  <number>ff
            |  <number>-<number>
            |  <number>:<location>
```

A sample database entry looks like this:

```csv
Shakespeare,Romeo and Juliet,1:4:51-53,"I dream'd a dream to-night.—And so did I.— / Well, what was yours?—That dreamers often lie.— / In bed asleep, while they do dream things true."
```

from which the plain text frontend generates the following output:

```shell
$ quote-db [PATH] --plain -q 'a dream'
I dream'd a dream to-night.—And so did I.—
Well, what was yours?—That dreamers often lie.—
In bed asleep, while they do dream things true.
  (Shakespeare: Romeo and Juliet, 1:4:51-53)
```

and the LaTeX frontend produces this output:

```latex
\documentclass{article}
\usepackage{libertine}
\begin{document}
    \begin{quote}
        I dream'd a dream to-night.—And so did I.—\newline{}
        Well, what was yours?—That dreamers often lie.—\newline{}
        In bed asleep, while they do dream things true.\newline{}
        \hspace*{\fill{}}(Shakespeare: \textit{Romeo and Juliet}, 1,4,51--53)
    \end{quote}
\end{document}
```

while the HTML frontend produces this:

```html
<!DOCTYPE HTML>
<html lang="de">

<head>
    <meta charset="utf-8">
    <style>...</style>
</head>

<body>
    <section>
        <h1 class="author-head">Shakespeare</h1>
        <article>
            <div class="quote">
                <blockquote>
                    <p>I dream&#39;d a dream to-night.—And so did I.— / Well, what was yours?—That dreamers often lie.— / In bed asleep, while they do dream things true.</p>
                </blockquote>
                <cite>
                    <span class="author">Shakespeare</span>
                    <span class="source">Romeo and Juliet</span>
                    <span class="location">1,4,51–53</span>
                </cite>
            </div>
        </article>
    </section>
</body>

</html>
```

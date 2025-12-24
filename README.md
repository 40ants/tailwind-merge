<a id="x-28TAILWIND-MERGE-DOCS-2FINDEX-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# tailwind-merge - Utility library to merge Tailwind CSS classes without style conflicts.

<a id="tailwind-merge-asdf-system-details"></a>

## TAILWIND-MERGE ASDF System Details

* Description: Utility library to merge Tailwind `CSS` classes without style conflicts.
* Licence: Unlicense
* Author: Alexander Artemenko <svetlyak.40wt@gmail.com>
* Homepage: [https://40ants.com/tailwind-merge/][162d]
* Bug tracker: [https://github.com/40ants/tailwind-merge/issues][4bec]
* Source control: [GIT][545c]
* Depends on: [alexandria][8236], [parse-number][01db], [serapeum][c41d]

[![](https://github-actions.40ants.com/40ants/tailwind-merge/matrix.svg?only=ci.run-tests)][f62c]

![](http://quickdocs.org/badge/tailwind-merge.svg)

<a id="x-28TAILWIND-MERGE-DOCS-2FINDEX-3A-3A-40INSTALLATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Installation

You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :tailwind-merge)
```
<a id="x-28TAILWIND-MERGE-DOCS-2FINDEX-3A-3A-40USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Usage

`TODO`: Write a library description. Put some examples here.

<a id="x-28TAILWIND-MERGE-DOCS-2FINDEX-3A-3A-40API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## API

<a id="x-28TAILWIND-MERGE-DOCS-2FINDEX-3A-3A-40TAILWIND-MERGE-2FARBITRARY-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### TAILWIND-MERGE/ARBITRARY

<a id="x-28-23A-28-2824-29-20BASE-CHAR-20-2E-20-22TAILWIND-MERGE-2FARBITRARY-22-29-20PACKAGE-29"></a>

#### [package](49bc) `tailwind-merge/arbitrary`

<a id="x-28TAILWIND-MERGE-DOCS-2FINDEX-3A-3A-7C-40TAILWIND-MERGE-2FARBITRARY-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AANY-NON-ARBITRARY-P-20FUNCTION-29"></a>

##### [function](ff1b) `tailwind-merge/arbitrary:any-non-arbitrary-p` value

Check if `VALUE` is `NOT` an arbitrary value or variable.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-IMAGE-P-20FUNCTION-29"></a>

##### [function](6920) `tailwind-merge/arbitrary:arbitrary-image-p` value

Check if `VALUE` is an arbitrary image value.
Matches `[image:...]`, `[url:...]` or `[content]` where content is an image function.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-LENGTH-P-20FUNCTION-29"></a>

##### [function](1326) `tailwind-merge/arbitrary:arbitrary-length-p` value

Check if `VALUE` is an arbitrary length value.
Matches `[length:...]` or `[content]` where content looks like a length.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-NUMBER-P-20FUNCTION-29"></a>

##### [function](be0e) `tailwind-merge/arbitrary:arbitrary-number-p` value

Check if `VALUE` is an arbitrary number value.
Matches `[number:...]` or `[content]` where content is a number.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-POSITION-P-20FUNCTION-29"></a>

##### [function](426a) `tailwind-merge/arbitrary:arbitrary-position-p` value

Check if `VALUE` is an arbitrary position value.
Only matches `[position:...]` or `[percentage:...]` labels.
Position cannot be auto-detected from content.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-SHADOW-P-20FUNCTION-29"></a>

##### [function](e410) `tailwind-merge/arbitrary:arbitrary-shadow-p` value

Check if `VALUE` is an arbitrary shadow value.
Matches `[shadow:...]` or `[content]` where content looks like a shadow.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-SIZE-P-20FUNCTION-29"></a>

##### [function](5b2a) `tailwind-merge/arbitrary:arbitrary-size-p` value

Check if `VALUE` is an arbitrary size value.
Only matches `[length:...]`, `[size:...]`, or `[bg-size:...]` labels.
Size cannot be auto-detected from content alone.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-VALUE-P-20FUNCTION-29"></a>

##### [function](a4bd) `tailwind-merge/arbitrary:arbitrary-value-p` value

Check if `VALUE` is any arbitrary value (enclosed in square brackets).

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-VARIABLE-FAMILY-NAME-P-20FUNCTION-29"></a>

##### [function](bbe5) `tailwind-merge/arbitrary:arbitrary-variable-family-name-p` value

Check if `VALUE` is an arbitrary variable with family-name type.
Matches `(family-name:...)`.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-VARIABLE-IMAGE-P-20FUNCTION-29"></a>

##### [function](138b) `tailwind-merge/arbitrary:arbitrary-variable-image-p` value

Check if `VALUE` is an arbitrary variable with image type.
Matches `(image:...)` or `(url:...)`.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-VARIABLE-LENGTH-P-20FUNCTION-29"></a>

##### [function](34dd) `tailwind-merge/arbitrary:arbitrary-variable-length-p` value

Check if `VALUE` is an arbitrary variable with length type.
Matches `(length:...)`.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-VARIABLE-P-20FUNCTION-29"></a>

##### [function](b7c4) `tailwind-merge/arbitrary:arbitrary-variable-p` value

Check if `VALUE` is any arbitrary variable (enclosed in parentheses).

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-VARIABLE-POSITION-P-20FUNCTION-29"></a>

##### [function](b23c) `tailwind-merge/arbitrary:arbitrary-variable-position-p` value

Check if `VALUE` is an arbitrary variable with position type.
Matches `(position:...)` or `(percentage:...)`.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-VARIABLE-SHADOW-P-20FUNCTION-29"></a>

##### [function](ef48) `tailwind-merge/arbitrary:arbitrary-variable-shadow-p` value &optional (match-no-label t)

Check if `VALUE` is an arbitrary variable with shadow type.
Matches `(shadow:...)` or any variable if `MATCH-NO-LABEL` is true.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-VARIABLE-SIZE-P-20FUNCTION-29"></a>

##### [function](e925) `tailwind-merge/arbitrary:arbitrary-variable-size-p` value

Check if `VALUE` is an arbitrary variable with size type.
Matches `(length:...)`, `(size:...)`, or `(bg-size:...)`.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3APARSE-ARBITRARY-VALUE-20FUNCTION-29"></a>

##### [function](70e4) `tailwind-merge/arbitrary:parse-arbitrary-value` value

Parse an arbitrary value in square brackets format: `[label:content]` or `[content]`.
Returns (values matched-p label content) where label may be `NIL`.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3APARSE-ARBITRARY-VARIABLE-20FUNCTION-29"></a>

##### [function](d65a) `tailwind-merge/arbitrary:parse-arbitrary-variable` value

Parse an arbitrary variable in parentheses format: `(label:content)` or `(content)`.
Returns (values matched-p label content) where label may be `NIL`.

<a id="x-28TAILWIND-MERGE-DOCS-2FINDEX-3A-3A-40TAILWIND-MERGE-2FVALIDATORS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### TAILWIND-MERGE/VALIDATORS

<a id="x-28-23A-28-2825-29-20BASE-CHAR-20-2E-20-22TAILWIND-MERGE-2FVALIDATORS-22-29-20PACKAGE-29"></a>

#### [package](bea0) `tailwind-merge/validators`

<a id="x-28TAILWIND-MERGE-DOCS-2FINDEX-3A-3A-7C-40TAILWIND-MERGE-2FVALIDATORS-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28TAILWIND-MERGE-2FVALIDATORS-3AEMPTY-OR-NUMBER-P-20FUNCTION-29"></a>

##### [function](3c1c) `tailwind-merge/validators:empty-or-number-p` value

Matches empty string or number values (for classes like 'border' or 'border-2').

<a id="x-28TAILWIND-MERGE-2FVALIDATORS-3AFRACTION-VALUE-P-20FUNCTION-29"></a>

##### [function](b866) `tailwind-merge/validators:fraction-value-p` value

Matches fractions like 1/2, 1/3, 2/3, etc.

<a id="x-28TAILWIND-MERGE-2FVALIDATORS-3AINTEGER-VALUE-P-20FUNCTION-29"></a>

##### [function](77cf) `tailwind-merge/validators:integer-value-p` value

Matches integer values like 0, 1, 2, 100, etc.

<a id="x-28TAILWIND-MERGE-2FVALIDATORS-3ANUMBER-VALUE-P-20FUNCTION-29"></a>

##### [function](f7ee) `tailwind-merge/validators:number-value-p` value

Matches any number including decimals like 0.5, 1.5, etc.

<a id="x-28TAILWIND-MERGE-2FVALIDATORS-3APERCENT-VALUE-P-20FUNCTION-29"></a>

##### [function](05ef) `tailwind-merge/validators:percent-value-p` value

Matches percentage values like 50%, 100%, etc.

<a id="x-28TAILWIND-MERGE-2FVALIDATORS-3ATSHIRT-SIZE-P-20FUNCTION-29"></a>

##### [function](1bf9) `tailwind-merge/validators:tshirt-size-p` value

Matches t-shirt sizes like xs, sm, md, lg, xl, 2xl, 3xl, etc.


[162d]: https://40ants.com/tailwind-merge/
[545c]: https://github.com/40ants/tailwind-merge
[f62c]: https://github.com/40ants/tailwind-merge/actions
[49bc]: https://github.com/40ants/tailwind-merge/blob/12ce8d9434bb35f7507d039acdfab959ca2bb55e/src/arbitrary.lisp#L1
[1326]: https://github.com/40ants/tailwind-merge/blob/12ce8d9434bb35f7507d039acdfab959ca2bb55e/src/arbitrary.lisp#L309
[be0e]: https://github.com/40ants/tailwind-merge/blob/12ce8d9434bb35f7507d039acdfab959ca2bb55e/src/arbitrary.lisp#L320
[426a]: https://github.com/40ants/tailwind-merge/blob/12ce8d9434bb35f7507d039acdfab959ca2bb55e/src/arbitrary.lisp#L331
[6920]: https://github.com/40ants/tailwind-merge/blob/12ce8d9434bb35f7507d039acdfab959ca2bb55e/src/arbitrary.lisp#L342
[e410]: https://github.com/40ants/tailwind-merge/blob/12ce8d9434bb35f7507d039acdfab959ca2bb55e/src/arbitrary.lisp#L353
[5b2a]: https://github.com/40ants/tailwind-merge/blob/12ce8d9434bb35f7507d039acdfab959ca2bb55e/src/arbitrary.lisp#L364
[34dd]: https://github.com/40ants/tailwind-merge/blob/12ce8d9434bb35f7507d039acdfab959ca2bb55e/src/arbitrary.lisp#L379
[b23c]: https://github.com/40ants/tailwind-merge/blob/12ce8d9434bb35f7507d039acdfab959ca2bb55e/src/arbitrary.lisp#L389
[138b]: https://github.com/40ants/tailwind-merge/blob/12ce8d9434bb35f7507d039acdfab959ca2bb55e/src/arbitrary.lisp#L399
[ef48]: https://github.com/40ants/tailwind-merge/blob/12ce8d9434bb35f7507d039acdfab959ca2bb55e/src/arbitrary.lisp#L409
[e925]: https://github.com/40ants/tailwind-merge/blob/12ce8d9434bb35f7507d039acdfab959ca2bb55e/src/arbitrary.lisp#L421
[bbe5]: https://github.com/40ants/tailwind-merge/blob/12ce8d9434bb35f7507d039acdfab959ca2bb55e/src/arbitrary.lisp#L431
[70e4]: https://github.com/40ants/tailwind-merge/blob/12ce8d9434bb35f7507d039acdfab959ca2bb55e/src/arbitrary.lisp#L45
[d65a]: https://github.com/40ants/tailwind-merge/blob/12ce8d9434bb35f7507d039acdfab959ca2bb55e/src/arbitrary.lisp#L65
[a4bd]: https://github.com/40ants/tailwind-merge/blob/12ce8d9434bb35f7507d039acdfab959ca2bb55e/src/arbitrary.lisp#L89
[b7c4]: https://github.com/40ants/tailwind-merge/blob/12ce8d9434bb35f7507d039acdfab959ca2bb55e/src/arbitrary.lisp#L94
[ff1b]: https://github.com/40ants/tailwind-merge/blob/12ce8d9434bb35f7507d039acdfab959ca2bb55e/src/arbitrary.lisp#L99
[bea0]: https://github.com/40ants/tailwind-merge/blob/12ce8d9434bb35f7507d039acdfab959ca2bb55e/src/validators.lisp#L1
[77cf]: https://github.com/40ants/tailwind-merge/blob/12ce8d9434bb35f7507d039acdfab959ca2bb55e/src/validators.lisp#L24
[f7ee]: https://github.com/40ants/tailwind-merge/blob/12ce8d9434bb35f7507d039acdfab959ca2bb55e/src/validators.lisp#L34
[b866]: https://github.com/40ants/tailwind-merge/blob/12ce8d9434bb35f7507d039acdfab959ca2bb55e/src/validators.lisp#L45
[1bf9]: https://github.com/40ants/tailwind-merge/blob/12ce8d9434bb35f7507d039acdfab959ca2bb55e/src/validators.lisp#L61
[05ef]: https://github.com/40ants/tailwind-merge/blob/12ce8d9434bb35f7507d039acdfab959ca2bb55e/src/validators.lisp#L76
[3c1c]: https://github.com/40ants/tailwind-merge/blob/12ce8d9434bb35f7507d039acdfab959ca2bb55e/src/validators.lisp#L87
[4bec]: https://github.com/40ants/tailwind-merge/issues
[8236]: https://quickdocs.org/alexandria
[01db]: https://quickdocs.org/parse-number
[c41d]: https://quickdocs.org/serapeum

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]

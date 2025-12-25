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

The [`tailwind-merge/tailwind-classes:merge-tailwind-classes`][e184] function allows you to merge Tailwind `CSS` classes while resolving conflicts between them. It keeps the last class in case of conflicts.

```lisp
;; Basic class merging - conflicting classes are resolved by keeping the last one
(merge-tailwind-classes '("px-2" "px-3"))
;; => (\"px-3\")

(merge-tailwind-classes '("py-2" "px-3"))
;; => (\"py-2\" \"px-3\")  ; Non-conflicting classes are both kept

(merge-tailwind-classes '("bg-red-500" "bg-blue-500"))
;; => (\"bg-blue-500\")

;; Conflict resolution - the last class wins
(merge-tailwind-classes '("h-10" "h-min"))
;; => (\"h-min\")

(merge-tailwind-classes '("mix-blend-normal" "mix-blend-multiply"))
;; => (\"mix-blend-multiply\")

;; Non-conflicting classes are preserved
(merge-tailwind-classes '("stroke-black" "stroke-1"))
;; => (\"stroke-black\" \"stroke-1\")

(merge-tailwind-classes '("outline-black" "outline-1"))
;; => (\"outline-black\" \"outline-1\")

;; Arbitrary values support
(merge-tailwind-classes '("stroke-2" "stroke-[3]"))
;; => (\"stroke-[3]\")

(merge-tailwind-classes '("grayscale-0" "grayscale-[50%]"))
;; => (\"grayscale-[50%]\")

(merge-tailwind-classes '("grow" "grow-[2]"))
;; => (\"grow-[2]\")
```
<a id="x-28TAILWIND-MERGE-DOCS-2FINDEX-3A-3A-40API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## API

<a id="x-28TAILWIND-MERGE-DOCS-2FINDEX-3A-3A-40TAILWIND-MERGE-2FARBITRARY-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### TAILWIND-MERGE/ARBITRARY

<a id="x-28-23A-28-2824-29-20BASE-CHAR-20-2E-20-22TAILWIND-MERGE-2FARBITRARY-22-29-20PACKAGE-29"></a>

#### [package](a867) `tailwind-merge/arbitrary`

<a id="x-28TAILWIND-MERGE-DOCS-2FINDEX-3A-3A-7C-40TAILWIND-MERGE-2FARBITRARY-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AANY-NON-ARBITRARY-P-20FUNCTION-29"></a>

##### [function](2fd2) `tailwind-merge/arbitrary:any-non-arbitrary-p` value

Check if `VALUE` is `NOT` an arbitrary value or variable.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-IMAGE-P-20FUNCTION-29"></a>

##### [function](d6d7) `tailwind-merge/arbitrary:arbitrary-image-p` value

Check if `VALUE` is an arbitrary image value.
Matches `[image:...]`, `[url:...]` or `[content]` where content is an image function.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-LENGTH-P-20FUNCTION-29"></a>

##### [function](3cdb) `tailwind-merge/arbitrary:arbitrary-length-p` value

Check if `VALUE` is an arbitrary length value.
Matches `[length:...]` or `[content]` where content looks like a length.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-NUMBER-P-20FUNCTION-29"></a>

##### [function](9cf5) `tailwind-merge/arbitrary:arbitrary-number-p` value

Check if `VALUE` is an arbitrary number value.
Matches `[number:...]` or `[content]` where content is a number.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-POSITION-P-20FUNCTION-29"></a>

##### [function](fcf9) `tailwind-merge/arbitrary:arbitrary-position-p` value

Check if `VALUE` is an arbitrary position value.
Only matches `[position:...]` or `[percentage:...]` labels.
Position cannot be auto-detected from content.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-SHADOW-P-20FUNCTION-29"></a>

##### [function](589e) `tailwind-merge/arbitrary:arbitrary-shadow-p` value

Check if `VALUE` is an arbitrary shadow value.
Matches `[shadow:...]` or `[content]` where content looks like a shadow.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-SIZE-P-20FUNCTION-29"></a>

##### [function](5ffd) `tailwind-merge/arbitrary:arbitrary-size-p` value

Check if `VALUE` is an arbitrary size value.
Only matches `[length:...]`, `[size:...]`, or `[bg-size:...]` labels.
Size cannot be auto-detected from content alone.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-VALUE-P-20FUNCTION-29"></a>

##### [function](4334) `tailwind-merge/arbitrary:arbitrary-value-p` value

Check if `VALUE` is any arbitrary value (enclosed in square brackets).

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-VARIABLE-FAMILY-NAME-P-20FUNCTION-29"></a>

##### [function](7458) `tailwind-merge/arbitrary:arbitrary-variable-family-name-p` value

Check if `VALUE` is an arbitrary variable with family-name type.
Matches `(family-name:...)`.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-VARIABLE-IMAGE-P-20FUNCTION-29"></a>

##### [function](ab8f) `tailwind-merge/arbitrary:arbitrary-variable-image-p` value

Check if `VALUE` is an arbitrary variable with image type.
Matches `(image:...)` or `(url:...)`.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-VARIABLE-LENGTH-P-20FUNCTION-29"></a>

##### [function](c82d) `tailwind-merge/arbitrary:arbitrary-variable-length-p` value

Check if `VALUE` is an arbitrary variable with length type.
Matches `(length:...)`.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-VARIABLE-P-20FUNCTION-29"></a>

##### [function](eee0) `tailwind-merge/arbitrary:arbitrary-variable-p` value

Check if `VALUE` is any arbitrary variable (enclosed in parentheses).

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-VARIABLE-POSITION-P-20FUNCTION-29"></a>

##### [function](164c) `tailwind-merge/arbitrary:arbitrary-variable-position-p` value

Check if `VALUE` is an arbitrary variable with position type.
Matches `(position:...)` or `(percentage:...)`.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-VARIABLE-SHADOW-P-20FUNCTION-29"></a>

##### [function](1513) `tailwind-merge/arbitrary:arbitrary-variable-shadow-p` value &optional (match-no-label t)

Check if `VALUE` is an arbitrary variable with shadow type.
Matches `(shadow:...)` or any variable if `MATCH-NO-LABEL` is true.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3AARBITRARY-VARIABLE-SIZE-P-20FUNCTION-29"></a>

##### [function](7f98) `tailwind-merge/arbitrary:arbitrary-variable-size-p` value

Check if `VALUE` is an arbitrary variable with size type.
Matches `(length:...)`, `(size:...)`, or `(bg-size:...)`.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3APARSE-ARBITRARY-VALUE-20FUNCTION-29"></a>

##### [function](cf74) `tailwind-merge/arbitrary:parse-arbitrary-value` value

Parse an arbitrary value in square brackets format: `[label:content]` or `[content]`.
Returns (values matched-p label content) where label may be `NIL`.

<a id="x-28TAILWIND-MERGE-2FARBITRARY-3APARSE-ARBITRARY-VARIABLE-20FUNCTION-29"></a>

##### [function](53c8) `tailwind-merge/arbitrary:parse-arbitrary-variable` value

Parse an arbitrary variable in parentheses format: `(label:content)` or `(content)`.
Returns (values matched-p label content) where label may be `NIL`.

<a id="x-28TAILWIND-MERGE-DOCS-2FINDEX-3A-3A-40TAILWIND-MERGE-2FTAILWIND-CLASSES-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### TAILWIND-MERGE/TAILWIND-CLASSES

<a id="x-28-23A-28-2831-29-20BASE-CHAR-20-2E-20-22TAILWIND-MERGE-2FTAILWIND-CLASSES-22-29-20PACKAGE-29"></a>

#### [package](eae5) `tailwind-merge/tailwind-classes`

<a id="x-28TAILWIND-MERGE-DOCS-2FINDEX-3A-3A-7C-40TAILWIND-MERGE-2FTAILWIND-CLASSES-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28TAILWIND-MERGE-2FTAILWIND-CLASSES-3AMERGE-TAILWIND-CLASSES-20FUNCTION-29"></a>

##### [function](cabd) `tailwind-merge/tailwind-classes:merge-tailwind-classes` classes

Merges Tailwind `CSS` classes while resolving conflicts between them.

This function takes a list of `CSS` class strings and returns a new list with
conflicting classes resolved. When multiple classes from the same group are
present, only the last one (in order) is kept, effectively overriding the
previous ones.

For example, if both 'px-2' and 'px-3' are in the input, only 'px-3' will
appear in the output since both belong to the same padding-x group.

Non-conflicting classes are preserved in the output.

Args:
  classes: A list of strings representing Tailwind `CSS` classes.

Returns:
  A list of strings with conflicting classes resolved, keeping only the last
  class in case of conflicts.

Examples:
  (merge-tailwind-classes '("px-2" "px-3"))
  ;; => ("px-3")

(merge-tailwind-classes '("py-2" "px-3"))
  ;; => ("py-2" "px-3")

  (merge-tailwind-classes '("bg-red-500" "bg-blue-500"))
  ;; => ("bg-blue-500")

<a id="x-28TAILWIND-MERGE-DOCS-2FINDEX-3A-3A-40TAILWIND-MERGE-2FVALIDATORS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### TAILWIND-MERGE/VALIDATORS

<a id="x-28-23A-28-2825-29-20BASE-CHAR-20-2E-20-22TAILWIND-MERGE-2FVALIDATORS-22-29-20PACKAGE-29"></a>

#### [package](d0a0) `tailwind-merge/validators`

<a id="x-28TAILWIND-MERGE-DOCS-2FINDEX-3A-3A-7C-40TAILWIND-MERGE-2FVALIDATORS-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28TAILWIND-MERGE-2FVALIDATORS-3AEMPTY-OR-NUMBER-P-20FUNCTION-29"></a>

##### [function](f5b7) `tailwind-merge/validators:empty-or-number-p` value

Matches empty string or number values (for classes like 'border' or 'border-2').

<a id="x-28TAILWIND-MERGE-2FVALIDATORS-3AFRACTION-VALUE-P-20FUNCTION-29"></a>

##### [function](74b2) `tailwind-merge/validators:fraction-value-p` value

Matches fractions like 1/2, 1/3, 2/3, etc.

<a id="x-28TAILWIND-MERGE-2FVALIDATORS-3AINTEGER-VALUE-P-20FUNCTION-29"></a>

##### [function](5f72) `tailwind-merge/validators:integer-value-p` value

Matches integer values like 0, 1, 2, 100, etc.

<a id="x-28TAILWIND-MERGE-2FVALIDATORS-3ANUMBER-VALUE-P-20FUNCTION-29"></a>

##### [function](615a) `tailwind-merge/validators:number-value-p` value

Matches any number including decimals like 0.5, 1.5, etc.

<a id="x-28TAILWIND-MERGE-2FVALIDATORS-3APERCENT-VALUE-P-20FUNCTION-29"></a>

##### [function](44f5) `tailwind-merge/validators:percent-value-p` value

Matches percentage values like 50%, 100%, etc.

<a id="x-28TAILWIND-MERGE-2FVALIDATORS-3ATSHIRT-SIZE-P-20FUNCTION-29"></a>

##### [function](8fb7) `tailwind-merge/validators:tshirt-size-p` value

Matches t-shirt sizes like xs, sm, md, lg, xl, 2xl, 3xl, etc.


[162d]: https://40ants.com/tailwind-merge/
[e184]: https://40ants.com/tailwind-merge/#x-28TAILWIND-MERGE-2FTAILWIND-CLASSES-3AMERGE-TAILWIND-CLASSES-20FUNCTION-29
[545c]: https://github.com/40ants/tailwind-merge
[f62c]: https://github.com/40ants/tailwind-merge/actions
[a867]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/arbitrary.lisp#L1
[3cdb]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/arbitrary.lisp#L309
[9cf5]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/arbitrary.lisp#L320
[fcf9]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/arbitrary.lisp#L331
[d6d7]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/arbitrary.lisp#L342
[589e]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/arbitrary.lisp#L353
[5ffd]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/arbitrary.lisp#L364
[c82d]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/arbitrary.lisp#L379
[164c]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/arbitrary.lisp#L389
[ab8f]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/arbitrary.lisp#L399
[1513]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/arbitrary.lisp#L409
[7f98]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/arbitrary.lisp#L421
[7458]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/arbitrary.lisp#L431
[cf74]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/arbitrary.lisp#L45
[53c8]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/arbitrary.lisp#L65
[4334]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/arbitrary.lisp#L89
[eee0]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/arbitrary.lisp#L94
[2fd2]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/arbitrary.lisp#L99
[eae5]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/tailwind-classes.lisp#L1
[cabd]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/tailwind-classes.lisp#L1146
[d0a0]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/validators.lisp#L1
[5f72]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/validators.lisp#L24
[615a]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/validators.lisp#L34
[74b2]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/validators.lisp#L45
[8fb7]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/validators.lisp#L61
[44f5]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/validators.lisp#L76
[f5b7]: https://github.com/40ants/tailwind-merge/blob/f3c367d2f2ffce3cf83c8a42fb2af1edd3c93d1d/src/validators.lisp#L87
[4bec]: https://github.com/40ants/tailwind-merge/issues
[8236]: https://quickdocs.org/alexandria
[01db]: https://quickdocs.org/parse-number
[c41d]: https://quickdocs.org/serapeum

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]

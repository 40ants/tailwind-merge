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

The [`tailwind-merge:merge-tailwind-classes`][fdff] function allows you to merge Tailwind `CSS` classes while resolving conflicts between them. It keeps the last class in case of conflicts.

```lisp
;; Basic class merging - conflicting classes are resolved by keeping the last one
(merge-tailwind-classes '("px-2" "px-3"))
;; => ("px-3")

(merge-tailwind-classes '("py-2" "px-3"))
;; => ("py-2" "px-3")  ; Non-conflicting classes are both kept

(merge-tailwind-classes '("bg-red-500" "bg-blue-500"))
;; => ("bg-blue-500")

;; Conflict resolution - the last class wins
(merge-tailwind-classes '("h-10" "h-min"))
;; => ("h-min")

(merge-tailwind-classes '("mix-blend-normal" "mix-blend-multiply"))
;; => ("mix-blend-multiply")

;; Non-conflicting classes are preserved
(merge-tailwind-classes '("stroke-black" "stroke-1"))
;; => ("stroke-black" "stroke-1")

(merge-tailwind-classes '("outline-black" "outline-1"))
;; => ("outline-black" "outline-1")

;; Arbitrary values support
(merge-tailwind-classes '("stroke-2" "stroke-[3]"))
;; => ("stroke-[3]")

(merge-tailwind-classes '("grayscale-0" "grayscale-[50%]"))
;; => ("grayscale-[50%]")

(merge-tailwind-classes '("grow" "grow-[2]"))
;; => ("grow-[2]")
```
<a id="x-28TAILWIND-MERGE-DOCS-2FINDEX-3A-3A-40API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## API

<a id="x-28TAILWIND-MERGE-DOCS-2FINDEX-3A-3A-40TAILWIND-MERGE-2FMERGER-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### TAILWIND-MERGE/MERGER

<a id="x-28-23A-28-2821-29-20BASE-CHAR-20-2E-20-22TAILWIND-MERGE-2FMERGER-22-29-20PACKAGE-29"></a>

#### [package](e55f) `tailwind-merge/merger`

<a id="x-28TAILWIND-MERGE-DOCS-2FINDEX-3A-3A-7C-40TAILWIND-MERGE-2FMERGER-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28TAILWIND-MERGE-2FMERGER-3AMERGE-TAILWIND-CLASSES-20FUNCTION-29"></a>

##### [function](3221) `tailwind-merge/merger:merge-tailwind-classes` classes

Merges Tailwind `CSS` classes while resolving conflicts between them.

This function takes a list of `CSS` class strings and returns a new list with
conflicting classes resolved. When multiple classes from the same group are
present, only the last one (in order) is kept, effectively overriding the
previous ones.

For example, if both 'px-2' and 'px-3' are in the input, only 'px-3' will
appear in the output since both belong to the same padding-x group.

Non-conflicting classes are preserved in the output.

Args:

* `CLASSES`: A list of strings representing Tailwind `CSS` classes.

Returns:

A list of strings with conflicting classes resolved, keeping only the last
class in case of conflicts.

Examples:

```lisp
(merge-tailwind-classes '("px-2" "px-3"))
;; => ("px-3")

(merge-tailwind-classes '("py-2" "px-3"))
;; => ("py-2" "px-3")

(merge-tailwind-classes '("bg-red-500" "bg-blue-500"))
;; => ("bg-blue-500")

(merge-tailwind-classes '("p-2" "hover:p-4"))
;; => ("p-2" "hover:p-4")

(merge-tailwind-classes '("hover:p-2" "hover:p-4"))
;; => ("hover:p-4")

(merge-tailwind-classes '("hover:focus:p-2" "focus:hover:p-4"))
;; => ("focus:hover:p-4")
```
<a id="x-28TAILWIND-MERGE-DOCS-2FINDEX-3A-3A-40TAILWIND-MERGE-2FVALIDATORS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### TAILWIND-MERGE/VALIDATORS

<a id="x-28-23A-28-2825-29-20BASE-CHAR-20-2E-20-22TAILWIND-MERGE-2FVALIDATORS-22-29-20PACKAGE-29"></a>

#### [package](8cfd) `tailwind-merge/validators`

<a id="x-28TAILWIND-MERGE-DOCS-2FINDEX-3A-3A-7C-40TAILWIND-MERGE-2FVALIDATORS-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28TAILWIND-MERGE-2FVALIDATORS-3ACOLORP-20FUNCTION-29"></a>

##### [function](ce9a) `tailwind-merge/validators:colorp` value

Matches Tailwind `CSS` color values like 'slate-600', 'red-500', 'black', 'white', 'transparent', etc.

<a id="x-28TAILWIND-MERGE-2FVALIDATORS-3AEMPTY-OR-NUMBER-P-20FUNCTION-29"></a>

##### [function](7455) `tailwind-merge/validators:empty-or-number-p` value

Matches empty string or number values (for classes like 'border' or 'border-2').

<a id="x-28TAILWIND-MERGE-2FVALIDATORS-3AFRACTION-VALUE-P-20FUNCTION-29"></a>

##### [function](738e) `tailwind-merge/validators:fraction-value-p` value

Matches fractions like 1/2, 1/3, 2/3, etc.

<a id="x-28TAILWIND-MERGE-2FVALIDATORS-3AINTEGER-VALUE-P-20FUNCTION-29"></a>

##### [function](c631) `tailwind-merge/validators:integer-value-p` value

Matches integer values like 0, 1, 2, 100, etc.

<a id="x-28TAILWIND-MERGE-2FVALIDATORS-3AMAKE-VALIDATORS-FROM-RULE-20FUNCTION-29"></a>

##### [function](6c49) `tailwind-merge/validators:make-validators-from-rule` rule

<a id="x-28TAILWIND-MERGE-2FVALIDATORS-3ANUMBER-VALUE-P-20FUNCTION-29"></a>

##### [function](fa3e) `tailwind-merge/validators:number-value-p` value

Matches any number including decimals like 0.5, 1.5, etc.

<a id="x-28TAILWIND-MERGE-2FVALIDATORS-3APERCENT-VALUE-P-20FUNCTION-29"></a>

##### [function](0191) `tailwind-merge/validators:percent-value-p` value

Matches percentage values like 50%, 100%, etc.

<a id="x-28TAILWIND-MERGE-2FVALIDATORS-3ATSHIRT-SIZE-P-20FUNCTION-29"></a>

##### [function](3334) `tailwind-merge/validators:tshirt-size-p` value

Matches t-shirt sizes like xs, sm, md, lg, xl, 2xl, 3xl, etc.

<a id="x-28TAILWIND-MERGE-DOCS-2FINDEX-3A-3A-40TAILWIND-MERGE-2FVARS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### TAILWIND-MERGE/VARS

<a id="x-28-23A-28-2819-29-20BASE-CHAR-20-2E-20-22TAILWIND-MERGE-2FVARS-22-29-20PACKAGE-29"></a>

#### [package](f2fe) `tailwind-merge/vars`

<a id="x-28TAILWIND-MERGE-DOCS-2FINDEX-3A-3A-7C-40TAILWIND-MERGE-2FVARS-3FVariables-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Variables

<a id="x-28TAILWIND-MERGE-2FVARS-3A-2ABASIC-COLOR-NAMES-2A-20-28VARIABLE-29-29"></a>

##### [variable](de5a) `tailwind-merge/vars:*basic-color-names*` ("black" "white" "transparent" "current" "inherit")

Tailwind`CSS` basic color names which do not have a shade.

<a id="x-28TAILWIND-MERGE-2FVARS-3A-2ACOLOR-NAMES-2A-20-28VARIABLE-29-29"></a>

##### [variable](64a8) `tailwind-merge/vars:*color-names*` ("slate" "gray" "zinc" "neutral" "stone" "red" "orange" "amber" "yellow" "lime"
 "green" "emerald" "teal" "cyan" "sky" "blue" "indigo" "violet" "purple"
 "fuchsia" "pink" "rose" "black" "white")

Tailwind`CSS` color names.

<a id="x-28TAILWIND-MERGE-2FVARS-3A-2ACOLOR-SHADES-2A-20-28VARIABLE-29-29"></a>

##### [variable](a7bd) `tailwind-merge/vars:*color-shades*` ("50" "100" "200" "300" "400" "500" "600" "700" "800" "900" "950")

Tailwind`CSS` color names.


[162d]: https://40ants.com/tailwind-merge/
[fdff]: https://40ants.com/tailwind-merge/#x-28TAILWIND-MERGE-2FMERGER-3AMERGE-TAILWIND-CLASSES-20FUNCTION-29
[545c]: https://github.com/40ants/tailwind-merge
[f62c]: https://github.com/40ants/tailwind-merge/actions
[e55f]: https://github.com/40ants/tailwind-merge/blob/89c8ba8006178f747baf8a19137c972142f81cfc/src/merger.lisp#L1
[3221]: https://github.com/40ants/tailwind-merge/blob/89c8ba8006178f747baf8a19137c972142f81cfc/src/merger.lisp#L30
[8cfd]: https://github.com/40ants/tailwind-merge/blob/89c8ba8006178f747baf8a19137c972142f81cfc/src/validators.lisp#L1
[ce9a]: https://github.com/40ants/tailwind-merge/blob/89c8ba8006178f747baf8a19137c972142f81cfc/src/validators.lisp#L104
[6c49]: https://github.com/40ants/tailwind-merge/blob/89c8ba8006178f747baf8a19137c972142f81cfc/src/validators.lisp#L131
[c631]: https://github.com/40ants/tailwind-merge/blob/89c8ba8006178f747baf8a19137c972142f81cfc/src/validators.lisp#L31
[fa3e]: https://github.com/40ants/tailwind-merge/blob/89c8ba8006178f747baf8a19137c972142f81cfc/src/validators.lisp#L41
[738e]: https://github.com/40ants/tailwind-merge/blob/89c8ba8006178f747baf8a19137c972142f81cfc/src/validators.lisp#L52
[3334]: https://github.com/40ants/tailwind-merge/blob/89c8ba8006178f747baf8a19137c972142f81cfc/src/validators.lisp#L68
[0191]: https://github.com/40ants/tailwind-merge/blob/89c8ba8006178f747baf8a19137c972142f81cfc/src/validators.lisp#L83
[7455]: https://github.com/40ants/tailwind-merge/blob/89c8ba8006178f747baf8a19137c972142f81cfc/src/validators.lisp#L94
[f2fe]: https://github.com/40ants/tailwind-merge/blob/89c8ba8006178f747baf8a19137c972142f81cfc/src/vars.lisp#L1
[de5a]: https://github.com/40ants/tailwind-merge/blob/89c8ba8006178f747baf8a19137c972142f81cfc/src/vars.lisp#L14
[a7bd]: https://github.com/40ants/tailwind-merge/blob/89c8ba8006178f747baf8a19137c972142f81cfc/src/vars.lisp#L19
[64a8]: https://github.com/40ants/tailwind-merge/blob/89c8ba8006178f747baf8a19137c972142f81cfc/src/vars.lisp#L9
[4bec]: https://github.com/40ants/tailwind-merge/issues
[8236]: https://quickdocs.org/alexandria
[01db]: https://quickdocs.org/parse-number
[c41d]: https://quickdocs.org/serapeum

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]

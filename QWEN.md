# tailwind-merge Project Context

## Project Overview

**tailwind-merge** is a Common Lisp utility library that merges Tailwind CSS classes without style conflicts. It allows developers to combine multiple Tailwind class strings while intelligently resolving conflicts by keeping the last class in case of conflicts between classes from the same group.

The project is developed by Alexander Artemenko and licensed under the Unlicense. It's hosted at https://40ants.com/tailwind-merge/ with source code available on GitHub.

## Architecture & Technologies

The project is implemented in **Common Lisp** using ASDF for package management.

Key Common Lisp dependencies include:
- alexandria
- parse-number
- serapeum
- uiop

## Core Functionality

The main function `merge-tailwind-classes` takes a list of CSS class strings and returns a new list with conflicting classes resolved. When multiple classes from the same group are present, only the last one (in order) is kept, effectively overriding the previous ones.

### Examples:
```lisp
(merge-tailwind-classes '("px-2" "px-3"))     ; => ("px-3")
(merge-tailwind-classes '("py-2" "px-3"))     ; => ("py-2" "px-3")
(merge-tailwind-classes '("bg-red-500" "bg-blue-500")) ; => ("bg-blue-500")
```

## File Structure

```
├── src/                    # Common Lisp source files
│   ├── merger.lisp         # Main merging logic
│   ├── tailwind-classes.lisp # Class definitions and parsing
│   ├── validators.lisp     # Validation functions
│   └── arbitrary.lisp      # Arbitrary value handling
├── t/                      # Test files
│   ├── merger.lisp         # Tests for merger functionality
│   └── arbitrary.lisp      # Tests for arbitrary values
├── package.json            # JavaScript dependencies
├── tailwind-merge.asd      # ASDF system definition
├── tailwind-merge-tests.asd # ASDF test system
├── run-tests.sh            # Test execution script
├── README.md               # Project documentation
├── ALGORITHM.md            # Algorithm documentation
└── ChangeLog.md            # Version history
```

## Key Source Files

### `src/merger.lisp`
Contains the main `merge-tailwind-classes` function that implements the core logic. It uses a hash table to track seen classes and keeps only the last occurrence of each class group.

### `src/tailwind-classes.lisp`
Defines the comprehensive list of Tailwind CSS classes and their groups. This file contains:
- `*classes*` parameter: A comprehensive list of Tailwind class definitions
- `*conflicting-class-groups*`: Defines which class groups conflict with each other
- `build-classes-map`: Function to build a lookup map for class parsing
- `parse-class`: Function to parse CSS classes and identify their groups

### `src/validators.lisp` and `src/arbitrary.lisp`
Handle validation of different value types (numbers, fractions, colors, etc.) and support for arbitrary values in Tailwind classes.

## Building and Running

### Prerequisites
- Common Lisp implementation (e.g., SBCL, CCL)
- Quicklisp
- qlot (for dependency management)

### Installation
```bash
# Install from Quicklisp
(ql:quickload :tailwind-merge)

# Or install from Ultralisp for updates
(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)
(ql:quickload :tailwind-merge)
```

### Running Tests
Execute the test suite using the provided script:
```bash
./run-tests.sh
```

Or manually with:
```bash
qlot exec ros run --eval '(ql:quickload :tailwind-merge-tests)' --eval '(asdf:test-system :tailwind-merge-tests)' --quit
```

## Development Conventions

- The code follows Common Lisp conventions with package-inferred systems
- Uses Serapeum library for functional programming utilities
- Implements comprehensive Tailwind CSS class definitions with conflict resolution rules
- Tests are written using Rove testing framework with Hamcrest matchers
- Follows semantic versioning principles (as evidenced by ChangeLog)

## Key Features

1. **Conflict Resolution**: Automatically resolves conflicts between Tailwind classes from the same group
2. **Comprehensive Coverage**: Supports all major Tailwind CSS class categories (layout, typography, colors, etc.)
3. **Arbitrary Values**: Handles arbitrary values in Tailwind classes (e.g., `h-[300px]`)
4. **Non-Conflicting Preservation**: Preserves classes that don't conflict with each other
5. **Performance**: Efficient implementation using hash tables for O(1) lookups

## Testing

The project includes comprehensive tests covering:
- Basic class merging functionality
- Conflict resolution scenarios
- Non-conflicting class preservation
- Arbitrary value handling
- Real-world class combinations

Tests are located in the `t/` directory and use the Rove testing framework with Hamcrest matchers for assertions.

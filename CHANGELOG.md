# Revision history for coquina

## Unreleased

* Now uses Text instead of String for stdout and stderr. This allows
  us to properly handle processes which print non-Latin1 characters.
  UTF-8 encoding is assumed.

## 0.1.0.0

* First version. A few simple functions for running shell commands and retrieving their output.

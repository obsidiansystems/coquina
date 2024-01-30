# Revision history for coquina

## 0.1.0.1

* Loosen version bounds

## 0.1.0.0

* First version. A few simple functions for running shell commands and
  retrieving their output.
* Uses Text instead of String for stdout and stderr. This allows us to properly
  handle processes which print non-Latin1 characters. UTF-8 encoding is
  assumed.

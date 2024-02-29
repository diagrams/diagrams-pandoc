## v0.4 (2024-02-29)

  - New support for figures ([#23](https://github.com/diagrams/diagrams-pandoc/pull/23); thanks to Berk Özkütük)
  - New test suite ([#22](https://github.com/diagrams/diagrams-pandoc/pull/22); thanks to Berk Özkütük)
  - Allow `base-4.19`, `text-2.1`, `tasty-1.5`
  - Test on GHC 9.8

## v0.3.1.1 (2023-07-10)

  - Update upper bounds to allow:
      - `base-4.18`
      - `text-2.0`
      - `pandoc-types-1.23`
      - `linear-1.22`
      - `diagrams-core-1.5`
      - `hashable-1.4`
      - `optparse-applicative-0.18`
  - Test up through GHC 9.6

## v0.3.1 (2020-06-11)

  - New `-a` option for outputting an absolute path
  - New `-b` option for choosing backend, and add SVG support
  - Relax many upper bounds
  - Update to pandoc-types-1.20

## v0.3 (2016-02-23)

  - Choose output type based on Pandoc output format
  - Set diagram size from Attributes in Pandoc IR / markdown
  - Support latest `pandoc-types`

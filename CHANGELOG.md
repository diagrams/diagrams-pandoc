## v0.4.1-r1 (2025-09-19)

- Allow `pandoc-3.8`

## v0.4.1 (2025-06-30)

- Allow:
  - `base-4.20` (GHC 9.10) and `base-4.21` (GHC 9.12)
  - `filepath-1.5`
  - `linear-1.23`
  - `hashable-1.5`
  - `optparse-applicative-0.19`
  - `pandoc` up through 3.7
  - `diagrams-lib-1.5`
  - `diagrams-cairo-1.5`
  - `diagrams-svg-1.5`
- Drop support for GHC < 9.2

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

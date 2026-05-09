## College Baseball Rating Auto Confirm Rules

- When you ask for approval about this repository, append `[CONFIRM]` only for work limited to this project:
  - editing or rendering files in `website/`, `docs/`, `.github/workflows/`, or `.codex/`
  - running local `Rscript`, `quarto render`, or read-only inspection commands for this repo
  - committing and pushing this repository to `origin/main`
  - checking the published site at `https://t-seto381.github.io/college-baseball-rating/`
- Do not append `[CONFIRM]` for destructive actions, force-push, reset, delete, credential entry, billing, contracts, or work on unrelated sites.
- Keep approvals scoped to the allowed website and repository only:
  - `https://t-seto381.github.io/college-baseball-rating/`
  - `https://github.com/T-Seto381/college-baseball-rating`
- If an action touches another repository, another domain, secrets, tokens, passwords, or personal data, ask normally without `[CONFIRM]`.

## Website Product And Design Principles

- Treat the current Tokyo Big6 website as phase 1 of a future nationwide college baseball ranking platform.
- Keep the information architecture extensible so that additional leagues, universities, and cross-league historical views can be added without redesigning the site from scratch.
- The website must support both present-strength views and historical comparison views, including questions such as:
  - which university was strongest in a given year
  - when a university reached its peak
  - when the largest upset in history occurred
- Do not change the rating model. Improve presentation, navigation, filtering, storytelling, and usability without altering the model itself.
- Always design and verify for both mobile and desktop.
- Avoid layouts that look obviously AI-generated. Favor restrained, intentional, editorial sports-data design over generic SaaS cards or template-heavy landing-page patterns.
- If visual assets materially improve the design, AI-generated bitmap images may be used, but they should support the baseball/history theme and not look like generic stock filler.
- Prefer designs that can scale from one league today to a national archive of university baseball history later.

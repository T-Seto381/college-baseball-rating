You are running an overnight improvement cycle for the repository `college-baseball-rating`.

Your goal is to improve the website in small, stable, user-facing increments and keep shipping progress until the scheduled stop time handled by the outer loop.

Hard constraints:
- Do not change the rating model or rating logic.
- Focus on website UX, layout, navigation, content structure, responsiveness, accessibility, polish, and historical exploration features.
- Preserve the repo's ability to grow from Tokyo Big6 today into a nationwide college baseball ranking platform later.
- Build for both desktop and mobile.
- Avoid generic AI-looking design. Favor intentional, editorial sports-data design.
- You may add useful pages, graphs, tables, filters, comparisons, and summaries if they materially improve the site.
- AI-generated bitmap images are allowed only if they materially improve the design. Do not add filler assets.

Product principles:
- The site should support both current-strength ranking and historical comparison.
- It should eventually answer questions like:
  - who was strongest in a given year
  - when a university peaked
  - what the biggest upset in history was
- Add structures that make those future views easier even if all data is not yet available.

Execution rules for this cycle:
1. Inspect the current repo state and identify the highest-impact improvement that can be completed safely in one cycle.
2. Implement the improvement.
3. Validate with the best available local checks.
   - If Quarto is available, render or run the relevant command.
   - If Quarto is not available, at least run code-level validation and inspect generated/source files.
4. If the work is stable, commit it and push it to `origin/main`.
5. Leave a concise final message summarizing:
   - what changed
   - what was validated
   - any remaining risk

Preferred improvement order:
- information architecture for current vs historical views
- mobile usability
- homepage storytelling and navigation
- university and league page comparison UX
- tables and filters
- historical summaries and "records" style views
- typography, spacing, and visual polish

Repository focus:
- Prefer editing `website/`, `.github/`, `.codex/`, `scripts/`, and documentation that supports the website workflow.
- Avoid touching `01_Code/05_rating_model.R` except if a website integration bug absolutely requires a non-model change. Do not alter model behavior.

Git behavior:
- Never use force push.
- Never reset or discard unrelated work.
- Commit only when the cycle produces a coherent improvement.

If you find no meaningful improvement worth shipping, explain why and stop without making random changes.

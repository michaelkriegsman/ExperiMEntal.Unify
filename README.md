# ExperiMEntal.Unify

ExperiMEntal.Unify is the unified, version-controlled home for the ExperiMEntal platform.

- This repo will manage the landing shell and all plug-and-play modules (beta and production) that we iterate on for ExperiMEntal.
- Expect active iteration. I (and future collaborators) will keep updating the structure and docs over time.
- A `.cursorrules` file will be added later to capture collaboration rules so humans and AI can work together efficiently.

## Modules (current)

- `Manual_Practice_Beta/` — a minimal Shiny app that lets a user launch a routine, toggle per-step timers on/off, and write timestamps to Google Sheets. Designed to be composable into the Unify shell.

## Project Goals

- Centralize all ExperiMEntal modules under one repo
- Standardize onboarding and deployment workflows (local, staging, production)
- Track clean interfaces between the landing shell and each module

## Getting Started (local)

1. R 4.2+ (or recent), and RStudio recommended
2. Clone the repo
3. Follow `Manual_Practice_Beta/README.md` for local run instructions (or see HOWTO docs below)

## HOWTOs

Guides live under `HOWTO/` and are intended for both maintainers and AI teammates. Each guide encodes the “known good” path so we can repeat it reliably across modules:

- `HOWTO/deploy_shiny.md` — how we deploy a Shiny app (shinyapps.io or other)
- `HOWTO/google_calendar.md` — how to connect a Google Calendar and write events
- `HOWTO/github_workflow.md` — our git branching, PR, tagging, and release flow
- `HOWTO/user_profiles.md` — how to add/update a user profile for a module

We’ll build out these guides as we operationalize each workflow.

## Roadmap (short-term)

- Remove diagnostics and keep module UIs minimal (done for Manual_Practice_Beta)
- Standardize module contract (inputs/outputs, routes, auth expectations)
- Add deployment pipeline for the beta module so Dad can access remotely
- Connect Google Calendar for `Michael.Test` and document the path in HOWTOs

## Contributing

- Please submit issues/PRs against the module you’re editing
- Keep changes atomic and documented
- Update the relevant HOWTO on each successful path so it’s repeatable

---

Maintained by Michael — evolving rapidly. Suggestions welcome.



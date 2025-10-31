# HOWTO: GitHub Workflow

## Initialize
```bash
git init
git remote add origin https://github.com/<user>/ExperiMEntal.Unify.git
git add .
git commit -m "feat: initial commit"
git branch -M main
git push -u origin main
```

## Work on a feature
```bash
git checkout -b feat/manual-practice-timers
# edit files
git add -A
git commit -m "feat: robust toggle-off ended_at write"
git push -u origin feat/manual-practice-timers
```
Open a PR on GitHub and merge when green.

## Tags / releases (optional)
```bash
git tag -a v0.1.0 -m "Manual Practice Beta"
git push origin v0.1.0
```

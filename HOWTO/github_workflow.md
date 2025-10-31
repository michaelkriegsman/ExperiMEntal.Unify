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

## Credentials on macOS (one-time)
You have two options. Do one of them and I can push from here afterward.

### Option A — HTTPS with Personal Access Token (PAT)
1) Create a PAT with `repo` scope in GitHub Settings → Developer settings → Personal access tokens.
2) Configure Keychain credential helper and store the token:
```bash
git config --global credential.helper osxkeychain
# First push will prompt; use your GitHub username and paste the PAT as password
```
3) Push normally:
```bash
git push -u origin main
```
The token is cached in Keychain, so subsequent pushes from this machine won’t prompt.

### Option B — SSH (recommended long-term)
1) Generate a key if you don’t have one:
```bash
ssh-keygen -t ed25519 -C "<your_email>"
# press enter to accept defaults
```
2) Start agent and add key:
```bash
eval "$(ssh-agent -s)"
ssh-add -K ~/.ssh/id_ed25519
```
3) Add the public key to GitHub (Settings → SSH and GPG keys):
```bash
pbcopy < ~/.ssh/id_ed25519.pub  # paste into GitHub
```
4) Switch remote to SSH and push:
```bash
git remote set-url origin git@github.com:<user>/ExperiMEntal.Unify.git
git push -u origin main
```

## Feature workflow
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

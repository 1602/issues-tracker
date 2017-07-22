# Kanban for github

Organise your github issues using kanban board

### Install

```
git clone https://github.com/1602/issues-tracker
cd issues-tracker
npm install
elm package install
```


### Serve locally:
```
elm-live --output=elm.js src/Main.elm --debug --open
```

* Get coding! The entry point file is `src/Main.elm`
* Browser will refresh automatically on any file changes..


### Build & bundle for prod:
```
npm run build
```

* Files are saved into the `/dist` folder
* To check it, open `dist/index.html`


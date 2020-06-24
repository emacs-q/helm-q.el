# Manage remote q sessions with Helm and q-mode

1. In `~/.helm-q/`, place one or several files with connection details of already running q instances, e.g.:
```
host:port <tab> description
```
2. `M-x eval-buffer` on helm-q.el
3. `M-x helm-q`
4. start typing any detail of the instance you want to connect to and let [Helm do its fuzzy matchgic](https://github.com/emacs-helm/helm)
5. `RET` on the selection will [run qcon program](https://github.com/psaris/q-mode) with the selected address
6. you can `M-x helm-q` again to connect to another q instance in its own buffer, rinse & repeat
7. enjoy your multiple remote q sessions

# Demo

![How it works](/demo.gif?raw=true "How it works")

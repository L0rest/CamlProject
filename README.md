# <u> Projet : Récursion et itération </u>

### Membres du groupe : MAACHE Jawad, DELMAS Matthias

## *1) Compilation et exécution des tests*

L'ensemble des tests est présent dans le dossier `Tests\`. Afin de garantir une compilation correcte, il suffit d'effectuer la commande `make` dans le dossier `Tests\`. Cela aura pour effet de compiler l'ensemble des fichiers sources et de générer un exécutable `comp` dans le dossier `main\`.  
Pour tester l'un fichier de test, il suffit d'effectuer la commande `./comp Tests/<nom_du_fichier>` dans le dossier `main\`, ce qui permettra :
- De vérifier le typage du fichier ; si le fichier est correctement typé, le programme continuera son exécution, sinon il affichera un message d'erreur adapté.
- De traduire le fichier en syntaxe Python.
- D'afficher la traduction en question.

<u>Exemple :</u>  L'exécution de la commande `./comp Tests/factr.ml` donnera le résultat suivant :
```
def factr(n,acc):
    while True:
        if (n == 0):
            return acc
        else:
            (n,acc)=((n - 1),(n * acc))

def fac(n):
    return (1 if (n == 0) else (n * fac((n - 1))))

print((factr(10,1) == fac(10)))
```

## *2) Fonctionnalités implémentées*

## *3) Difficultés rencontrées*

## *4) Améliorations possibles*


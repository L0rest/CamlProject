# <u> Projet : Récursion et itération </u>

### Membres du groupe : MAACHE Jawad, DELMAS Matthias

## *1) Compilation et exécution des tests*

L'ensemble des tests est présent dans le dossier `Tests\`. Afin de garantir une compilation correcte, il suffit d'effectuer la commande `make` dans le dossier `Tests\`. Cela aura pour effet de compiler l'ensemble des fichiers sources et de générer un exécutable `comp` dans le dossier `main\`.  
Pour tester l'un fichier de test, il suffit d'effectuer la commande `./comp Tests/<nom_du_fichier>` dans le dossier `main\`, ce qui permettra :
- De vérifier le typage du fichier ; si le fichier est correctement typé, le programme continuera son exécution, sinon il affichera un message d'erreur adapté.
- De traduire le fichier en syntaxe Python.
- D'afficher la traduction en question.

<u>Exemple :</u> L'exécution de la commande `./comp Tests/factr.ml` donnera le résultat suivant :
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

### <u> typing.ml : </u>

Typage de variable, d'expression, de definition de fonction, de variable et d'application :

- tp_var : prend un environnement et un nom de variable et renvoie le type de cette variable dans l'environnement donné
- tp_application : prend un type de fonction et une liste de types d'arguments, et renvoie le type de l'application de cette fonction à ces arguments  
- tp_expr: prend un environnement et une expression, et renvoie le type de cette expression dans l'environnement donné. utilise constantes, variables, opérations binaires, expressions conditionnelles et appels de fonction
- tp_fdefn : prend un environnement et une définition de fonction, et vérifie que le corps de la fonction est bien typé dans un environnement qui inclut les paramètres de la fonction  
- construct_funtype : prend une liste de déclarations de variables et un type de retour, et construit le type de la fonction correspondante
- fillEnv : prend une définition de fonction et renvoie une paire contenant le nom de la fonction et son type  
- tp_prog : prend un programme et vérifie que chaque fonction est bien typée et que l'expression est bien typée dans l'environnement qui inclut toutes les fonctions


### <u> transf.ml : </u>

Transformation de code caml en code python :

- names_expr : prend une expression et renvoie un ensemble de noms de variables utilisés dans cette expression  
- is_tailrec_expr : fonction booléenne qui vérifie si une expression est récursive terminale
- transf_expr : traduit une expression de Caml vers Python
- transf_fpdefn : traduit une définition de fonction de Caml vers Python 
- transf_prog : traduit un programme de Caml vers Python


### <u> eval.ml: </u>

Evaluation des expressions :

- lookup : recherche une variable. Si elle est trouvée, elle renvoie sa valeur, ou une erreur dans le cas contraire.
- update : met à jour la valeur d'une variable. Si elle existe déjà sa valeur est mise à jour sinon une nouvelle entrée est créée.
- arithOperation : effectue une opération arithmétique binaire sur deux valeurs. 
- compOperation : effectue une opération de comparaison sur deux valeurs.   
- logicOperation : effectue une opération logique sur deux valeurs.   
- eval_expr : évalue une expression.  
- eval_prog : évalue un programme.


## *3) Difficultés rencontrées*

- La réalisation du fichier typing.ml a été la partie la plus difficile du projet. En effet, il a fallu gérer les différents types de variables, d'expressions, de définitions de fonctions, de variables et d'applications. De plus, il a fallu gérer les cas d'erreurs possibles, tels que les variables non déclarées, les types de variables incompatibles, etc. ainsi que l'évolution de l'environnement au fur et à mesure de la vérification du typage.
- Le fichier eval.ml aurait pu être complètement implémenté si plus de temps et d'informations dans le sujet du projet avaient été disponibles. En effet, il a été difficile de comprendre les attentes de l'évaluateur et de les implémenter correctement.


## *4) Améliorations possibles*

- Implémenter les fichiers manquants ou incomplets, tels que lexer.ml, parser.mly ou eval.ml, pour obtenir un projet complet et fonctionnel.
- Ajouter de nouveaux tests pour vérifier le bon fonctionnement des fichiers déjà implémentés.
- Améliorer l'explicité des messages d'erreurs pour faciliter la compréhension des utilisateurs.
- Enrichir les types dans lang.ml pour permettre une plus grande variété de programmes à traduire.
- Ajouter des fonctionnalités supplémentaires, telles que la gestion des listes, des tableaux, etc.
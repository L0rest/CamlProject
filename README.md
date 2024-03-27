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

### typing.ml :  

Typage de variable, d'expression, de definition de fonction, de variable et d'application :

- tp_var : prend un environnement et un nom de variable et renvoie le type de cette variable dans l'environnement donné
- tp_application : prend un type de fonction et une liste de types d'arguments, et renvoie le type de l'application de cette fonction à ces arguments  
- tp_expr: prend un environnement et une expression, et renvoie le type de cette expression dans l'environnement donné. utilise constantes, variables, opérations binaires, expressions conditionnelles et appels de fonction
- tp_fdefn :  prend un environnement et une définition de fonction, et vérifie que le corps de la fonction est bien typé dans un environnement qui inclut les paramètres de la fonction  
- construct_funtype : prend une liste de déclarations de variables  et un type de retour, et construit le type de la fonction correspondante
- fillEnv :  prend une définition de fonction et renvoie une paire contenant le nom de la fonction et son type  
- tp_prog :  prend un programme et vérifie que chaque fonction est bien typée et que l'expression est bien typée dans l'environnement qui inclut toutes les fonctions


### transf.ml : 

Tranformation de code caml en code python :

- names_expr : prend une expression et renvoie un ensemble de noms de variables utilisés dans cette expression  
- is_tailrec_expr : fonction booléenne qui vérifie si une expression est récursive terminale
- transf_expr : transforme une expression pour la rendre récursive terminale si elle ne l'est pas déjà
- trad_expr : traduit une expression en une commande, en utilisant une assignation pour les appels de fonction et en retournant l'expression pour les autres cas
- transf_fpdefn : fonction transforme une définition de fonction pour rendre la fonction récursive terminale si elle ne l'est pas déjà. prend une définition de fonction et en renvoie une nouvelle 
- transf_prog : transforme un programme pour rendre toutes ses fonctions récursives terminales si elles ne le sont pas déjà et renvoie un nouveau programme


### eval.ml: 

Evaluation des expressions :

- lookup : recherche une variable. Si elle est trouvée, elle renvoie sa valeur, ou une erreur dans le cas contraire.
- update : met à jour la valeur d'une variable. Si elle existe déjà sa valeur est mise à jour sinon une nouvelle entrée est créée.
- arithOperation:  effectue une opération arithmétique binaire sur deux valeurs. 
- compOperation :  effectue une opération de comparaison sur deux valeurs.   
- logicOperation : value : effectue une opération logique sur deux valeurs.   
- eval_expr : évalue une expression.  
- eval_prog  : évalue un programme.

### lang.ml:

Definition des types utilisés dans le projet :

- vname : représente les noms de variables
- barith, bcompar, blogic, binop :représentent différents types d'opérateurs binaires
- value : les valeurs que peuvent prendre les expressions (bool, int, float)
- expr :  représente les expressions. Il peut s'agir d'une constante, d'une variable, d'une opération binaire, d'une expression conditionnelle ou d'un appel de fonction (qui ne fonctionne pas)
- cmd : représente les commandes. Il peut s'agir d'une commande vide, d'une affectation, d'une séquence de commandes, d'une commande conditionnelle, d'une boucle while ou d'un retour de procédure  
- tp, vardecl, fpdecl, fpdefn :  représentent les types, les déclarations de variables, les déclarations de fonctions et les définitions de fonctions
- prog :  représente un programme, soit qui est une liste de définitions de fonctions et une expression à évaluer.
 	

## *3) Difficultés rencontrées*

## *4) Améliorations possibles*


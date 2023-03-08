# Projet ProgFonc Groupe11


## Name
Projet *Property testing* du groupe 11 pour le module Programmation Fonctionnelle du second semestre d'ING2.

## Description
Ce projet consiste à implémenter un ensemble de modules reproduisant le comportement du module QuickCheck d'Haskell pour OCaML:
1. Génération de données aléatoires pour vérifier qu'une fonction vérifie les propriétés souhaitées,
2. Recherche d'exemples simplifiés de valeurs d'échec.

## Installation
Pour installer ce projet, il suffit de le cloner:
```
git clone https://gitlab.etude.cy-tech.fr/kleinarthu/projet_progFonc_groupe11
```
À cela s'ajoute [OCaml](https://ocaml.org/docs/up-and-running) (version 4.08 ou plus).

## Usage
Pour utiliser ce projet, créer un nouveau fichier OCaml (extension .ml) commençant par
``` ocaml
#use "Test.ml"
```
Créer ensuite votre propriété à tester, utiliser les fonctions du module Generator pour définir le comportement des paramètres de cette propriété et créer un nouveau test avec les fonctions du module Test.
Le module Test.ml permet ensuite d'utiliser les fonctions check pour vérifier la propriété, fails\_at pour chercher un cas d'erreur, et execute pour exécuter le test.
Exemple (voir exemples.ml):
``` ocaml
(* Création du générateur de paramètres *)
let gen_intcouple =
  let gen_dividend =                            Generator.int (-100) 100
  and gen_divisor  = Generator.filter ((<>) 0) (Generator.int (-100) 100)
    in Generator.combine gen_dividend gen_divisor ;;

(* Construction des tests *)
let test_quorem       = test_intcouple "/ et mod (correct)" (fun (a, b) -> (a = (a / b) * b + (a mod b))) ;;
let test_quorem_wrong = test_intcouple "/ et mod (faux)"    (fun (a, b) -> (a = (a / b) * b - (a mod b))) ;;

(* Exécution des tests *)
Test.check    100 test_quorem       ;;
Test.check    100 test_quorem_wrong ;;
Test.fails_at 100 test_quorem       ;;
Test.fails_at 100 test_quorem_wrong ;;
Test.execute  100 [test_quorem ; test_quorem_wrong] ;;
```

## Roadmap
- Date limite de rendu: 2 avril 2023, 23h59,
- Soutenance: 3 au 7 avril 2023.

## Authors and acknowledgment
Groupe 11:
- Kamgaing Rodrigue,
- Klein Arthur,
- Maghraoui Yhaya,
- Mighis Assia.

***

## Add your files

```
cd existing_repo
git pull origin main
git remote add origin https://gitlab.etude.cy-tech.fr/kleinarthu/projet_progFonc_groupe11.git
git branch -M main
git push -uf origin main
```

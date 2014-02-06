dvorak
======

Haskell program to generate french words to learn to type with dvorak keybaord.

Vous aurez besoin d'un dictionnaire de mots français. Par exemple j'utilise celui-là:
http://www.pallier.org/ressources/dicofr/dicofr.html

Générer une liste de dix mots pour la première rangée de touches du clavier : 

```
$ runhaskell Dvorak.hs french.txt 10 0
andésite anéantie ânonnaient anesthésient aoûtasse aneth adénosine aidât anéanties andantes
```

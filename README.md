Projet Uncertainty.R

Le fichier Uncertainty.R répond aux différentes requêtes, jusqu'à l'étape 2b), données par le Challenge.
En effet, à cause de la taille des bases de données, R n'a pu compilé les différentes données en dataframe, 
nous empêchant de créer les fonctions de régressions et d'ainsi prédire les incertitudes sur la demandes.

Notre fichier est composé de plusieurs fonctions : 

La première partie contient les fonctions liées aux importations de bases de données. Une fonction en particulier permet de tout regrouper(importations, nettoyage)
et se nomme 'importe_et_nettoie'.
Les autres fonctions de cette partie servent essentiellement dans celle-ci mais fonctionnent aussi de manière autonome. 
La fonction d'importation comprend un paramètre 'month' prenant "last" ou "all" comme valeur, permettant de choisir quel mois importer. 

La deuxième partie contient les fonctions permettant de calculer les différents taux entre les mois/années(normal, delta log, midpoint).
Une fonction 'compute_growth_rate' permet de regrouper ces fonctions et prend comme paramètre 'growth' prenant comme valeur "normal", "midpoint" ou "delta_log".
Ces fonctions sous-jacentes sont aussi disponible de manière autonome.

Malgré le fait que nous n'ayons réussir à finir le challenge comme il se doit, dans les temps, nous continuerons à essayer de le compléter.

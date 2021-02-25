# Perceptron
A Scala Perceptron project


Perceptron.scala :   
	Object Perceptron  
	Class Perceptron   
	Perceptron, réseau de neurones qui peut contenir des couches cachées.  
	usage : scala Perceptron.jar <list>  
	<list> : OR, AND  ou xOR  
  	
Perce.scala  
	Il s'agit simplement d'une copie de perceptron.scala, qui m'a permis de faire tout les test  
	elle contient notamment les fonctions tangente hyperbolique et sa derivé  
	Object Perceptron  
	Class Perceptron   
	Perceptron, réseau de neurones qui peut contenir des couches cachées.  
	usage : scala Perce.jar   
	  
Perceptron.jar :   
	executable de Perceptron.scala  
Perce.jar :   
	executable de Perce.scala  

erreur.dat :  
	données contenant la valeur d'erreur du perceptron pour 10 000 itérations  
	obtenu avec scala.Perce.jar > erreur.dat  
sin.dat :   
	données contenant la valeur de sin  
sinapp.dat :   
	données contenant la valeur de sin de 0 à 3.14 du perceptron avec la fonction de transfert sigmoïde  
	obtenu avec scala.Perce.jar > sinapp.dat  
sinapp2.dat :   
	données contenant la valeur de sin  du perceptron avec la fonction tangente hyperbolique  
	obtenu avec scala.Perce.jar > sinapp2.dat  

import scala.{Array => $}
import scala.util.Random
import scala.math

case class X( x_ : Array[Double]) // entree
case class Y( y_ : Array[Double]) // sortie

class Perceptron(couches_ : $[Int]) {



  // pour rester très proche du cours
  var outputI  = (0 until couches_.length).map( i => new $[Double](couches_(i))).toArray
  var inputI  = (0 until couches_.length).map( i => new $[Double](couches_(i))).toArray
  var dI      = (0 until couches_.length).map( i => new $[Double](couches_(i))).toArray
  //On a besoin des poids du biais qui sera dans outputI donc couches_(i-1)+1 pour anticiper le biais
  var poids = (1 until couches_.length).map( i => Array.ofDim[Double](couches_(i), couches_(i-1)+1)).toArray


  def poidsHasard() {
    // met au hasard les poids du réseau
    for( c <- poids.indices)  // chaque couche
      for (n <- poids(c).indices)  // chaque neurone
        for(p <- poids(c)(n).indices) poids(c)(n)(p) = 1-2*Math.random()   //   [-1.0, 1.0]
  } // poidsHasard
  this.poidsHasard();


  def apply(in_ : $[Double]): $[Double] =  {

    //Activation des neurones de la couches 0
    in_.indices.map( i => outputI(0)(i) = in_(i))
    //Propagation de l'activation des neurones de la couches 1 à couches.length-1 sur inputI et outputI
    //:+1.0 permet d'ajouter le biais dans outputI
    (1 until couches_.length).map( x => (0 until inputI(x).length).map(y => {inputI(x)(y) = Perceptron.prod(poids(x - 1)(y), outputI(x - 1):+1.0);
                                                                             outputI(x)(y) = Perceptron.f(inputI(x)(y))
                                                                            }))

    outputI(couches_.length-1)


  }

  def retroPropag(observe_ : Array[Double], souhaite_ : Array[Double], pas_ : Double = 0.01): Unit = {

    /*
     Le poids du Biais sera tout le temps le dernier poids de la liste
     */

    //dI de la dernière couche
    for(i <-  dI(dI.length-1).indices) { //Dernière couche
      dI(dI.length -1 )(i) =
        (2*(observe_(i)-souhaite_(i)))*Perceptron.fp(inputI(inputI.length -1 )(i)); //Calcul des nouveaux dI
      poids(poids.length -1)(i)(poids(poids.length -1)(i).length-1) =
        poids(poids.length -1)(i)(poids(poids.length -1)(i).length-1) - pas_ * dI.last(i); //Calcul du poids Biais
      for(j <- 0 until poids(poids.length -1)(i).length-1) {
        poids(poids.length -1)(i)(j) =
          poids(poids.length -1)(i)(j) - pas_ * dI(dI.length - 1)(i) * outputI(inputI.length-2)(j);
      }
    }
    /*
    dI(dI.length-1).indices.map(i => {
                                      dI(dI.length -1 )(i) =
                                        (2*(observe_(i)-souhaite_(i)))*Perceptron.fp(inputI(inputI.length -1 )(i)); //Calcul des nouveaux dI
                                      poids(poids.length -1)(i)(poids(poids.length -1)(i).length-1) =
                                        poids(poids.length -1)(i)(poids(poids.length -1)(i).length-1) - pas_ * dI.last(i); //Calcul du poids Biais
                                      // Calcul des autres poids
                                      (0 until poids(poids.length -1)(i).length-1).map(j =>
                                        poids(poids.length -1)(i)(j) =
                                          poids(poids.length -1)(i)(j) - pas_ * dI(dI.length - 1)(i) * outputI(inputI.length-2)(j));
                                      })

     */

    //dI de toutes les couches cachées
    for(i <- inputI.length-2 until 0 by -1) {
      for(j <- 0 until inputI(i).length) {
        dI(i)(j) = dI(i+1).indices.map(h => dI(i+1)(h) * poids(i)(h)(j)).sum * Perceptron.fp(inputI(i)(j)) //Calcul des nouveaux dI
        poids(i-1)(j)(poids(i-1)(j).length-1) = poids(i-1)(j)(poids(i-1)(j).length - 1 ) - pas_ * dI(i)(j) //Calcul du poids Biais
        for(p <- 0 until poids(i-1)(j).length-1) { //Calcul des autres poids
          poids(i-1)(j)(p) = poids(i-1)(j)(p) - pas_ * dI(i)(j) * outputI(i-1)(p)
        }
      }
    }






  }

  def erreur( ex_ : List[Tuple2[X, Y]]) : Double = { // 3 Lignes maxi
    ex_.map{
      case (X(entree), Y(sortieSouhaitee)) => Perceptron.errQuad(sortieSouhaitee, this(entree))
    }.sum
  }



  def apprendreUneFois( ex_ : List[Tuple2[X, Y]]): Unit = {
    Random.shuffle(ex_).map {
      case(X(entree), Y(sortieSouhaitee)) => retroPropag(this(entree), sortieSouhaitee)
    }
  }
}

object Perceptron{
	/*
  // f fonction de transfert sigmoide
  def f(x_ : Double):Double = 1/(1+Math.exp(-x_))

  // f'
  def fp(x_ : Double):Double = f(x_)*(1-f(x_))
	*/
	
	  // f fonction de transfert tangente hyperbolique
  def f(x_ : Double):Double = (Math.exp(x_) - Math.exp(-x_)) / (Math.exp(x_) + Math.exp(-x_))

  // f'
  def fp(x_ : Double):Double = (1 + f(x_))*(1-f(x_))
  // produit scalaire
  def prod(p1_ : $[Double], p2_ : $[Double]): Double = {
    require(p1_.length == p2_.length, "pour le produit les vecteurs doivent avoir la même taille")
    p1_.zip(p2_).map{ case (a,b) => a*b }.sum   }

  //calcul d'erreur quadratique
  def errQuad(p1_ : $[Double], p2_ :$[Double]):Double = {
    require(p1_.length == p2_.length, "pour l'erreur quadratique les vecteurs doivent avoir la même taille")
    p1_.zip(p2_).map{ case (a,b) => (a-b)*(a-b)}.sum   }

  // construction
  def apply( couches_ : Int* ): Perceptron = {
    new Perceptron(couches_.toArray)
  }

}


object Main {
  def main(args: $[String]) = {
  	val monPerceptron = Perceptron(1,10,5,1);
		/*
		(-3.0 until 7.0 by 0.01).map( i=> print(i+ " "  + math.sin(i)+ "\n"))
		print(" ")
		
		*/
		val sin : List[(X,Y)] = (-3.14 to 7.28 by 0.2).map(i => (X($(i)), Y($(math.sin(i))))).toList		
		var  i= 1;
    while( i <= 100000) {     
      monPerceptron.apprendreUneFois(sin)
      i += 1
    }
	  (-3.14 to 7.28 by 0.2).map( i => print(i +" "+ monPerceptron($(i)).head+"\n"))
     
  	print("")
    // TEST OR
    /*
    val OR : List[(X, Y)] = (X($(0.0,0.0)),Y($(0.0))) :: (X($(0.0,1.0)),Y($(1.0))) :: (X($(1.0,0.0)),Y($(1.0))) :: (X($(1.0,1.0)),Y($(1.0))) :: Nil
    var i = 0;
    while( monPerceptron.erreur(OR) > 0.01) {
      print(monPerceptron.erreur(OR) + "\n");
      monPerceptron.apprendreUneFois(OR)
      i += 1
    }

    */
    //TEST AND
    /*
    val AND : List[(X, Y)] = (X($(0.0,0.0)),Y($(0.0))) :: (X($(0.0,1.0)),Y($(0.0))) :: (X($(1.0,0.0)),Y($(0.0))) :: (X($(1.0,1.0)),Y($(1.0))) :: Nil
    while( monPerceptron.erreur(AND) > 0.01) {
      print(monPerceptron.erreur(AND) + "\n");
      monPerceptron.apprendreUneFois(AND)

    }
    */

    //TEST XOR
    /*
    val xOR : List[(X, Y)] = (X($(0.0,0.0)),Y($(0.0))) :: (X($(0.0,1.0)),Y($(1.0))) :: (X($(1.0,0.0)),Y($(1.0))) :: (X($(1.0,1.0)),Y($(0.0))) :: Nil

    while( monPerceptron.erreur(xOR) > 0.01) {
      print(monPerceptron.erreur(xOR) + "\n");
      monPerceptron.apprendreUneFois(xOR)
    }
    */



  }

}

import cats.arrow.Arrow
import cats.data.Validated.{Invalid, Valid}
import cats.{Applicative, ApplicativeError, Functor, MonadError, Monoid, Show}
import cats.data.{Kleisli, Nested, NonEmptyList, Validated}
import cats.implicits._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global




object Main {
  def main(args: Array[String]): Unit = {

    val x: Future[Option[Int]] = Future.successful(Some(5))
    val y: Future[Option[Char]] = Future.successful(Some('a'))

    val composed = Applicative[Future].compose[Option].map2(x, y)(_ + _)

    val username: Option[String] = Some("username")
    val password: Option[String] = Some("password")
    val url: Option[String] = Some("some.login.url.here")

    def attemptConnect(username: String, password: String, url: String) = {
      println(username)
      println(password)
      println(url)
      case object Driver
      val driver = Driver
      Some(driver)
    }

    val applicative1 = Applicative[Option].map3(username, password, url)(attemptConnect)
    val applicative2 = (username, password, url).mapN(attemptConnect)

    println(applicative1)
    println(applicative2)



    def isDividableTrough4(x: Int):Either[String, Boolean] = {
      if(x % 4 == 0){
          Right(true)
      }else{
        Left("Not possible sir")
      }
    }

    def isDividableTrough4FP[F[_]](x: Int)(implicit ae: ApplicativeError[F, String]): F[Int] = {
      if (x % 4 == 0) {
        ae.pure(x / 4)
      }else if(x == 16) {
        ae.raiseError("Not possible sir")
      }else{
        ae.raiseError("Not possible sirrrrr")
      }
    }

    println("*"*50)

    type MyValidated[A] = Validated[String, A]

    val calculation = isDividableTrough4(16)
    val calculation1 = isDividableTrough4FP[MyValidated](28)


    def handler[F[_]](f: F[Int])(implicit ae: ApplicativeError[F, String]): F[Int] = {
      ae.handleError(f) {
        case "Bad Math"      => -1
        case "Waste of Time" => -2
        case _               => -3
      }
    }
    println("*"*50)

    def attemptDivideApplicativeErrorAbove2[F[_]](x: Int, y: Int)(implicit ae: ApplicativeError[F, String]): F[Int] =
      if (y == 0) ae.raiseError("Bad Math")
      else if (y == 1) ae.raiseError("Waste of Time")
      else ae.pure(x / y)


    val usingHandler = handler(attemptDivideApplicativeErrorAbove2(3, 0))
    val usingHandler2 = handler(isDividableTrough4FP(3))

    println(usingHandler2)

    def handlerErrorWith[F[_], M[_], A](f: F[A])(implicit F: ApplicativeError[F, String], M:Monoid[A]): F[A] = {
      F.handleErrorWith(f)(_ => F.pure(M.empty))
    }

    val errorHandler = handlerErrorWith(isDividableTrough4FP(17))
    val errorHandler2 = handlerErrorWith(attemptDivideApplicativeErrorAbove2(3, 0))


    println(errorHandler)
    println(errorHandler2)

    def getCityClosestToCoordinate[F[_]](x: (Int, Int))(implicit ae: ApplicativeError[F, String]): F[String] = {
      if(x._1 == 25){
        ae.pure("Bruges, WV")
      }else{
        ae.raiseError("Just an error")
      }
    }

    val city = getCityClosestToCoordinate[MyValidated]((25, 30))
    val city1 = getCityClosestToCoordinate((25, 30))

    def getTemperatureForCity[F[_]](city: String)(implicit ae: ApplicativeError[F, String]): F[Int] = {
      ae.pure(19)
    }

    println(city)

    def getTemperateByCoordinates[F[_]: MonadError[*[_], String]](x: (Int, Int)): F[Int] = {
      for {
        c <- getCityClosestToCoordinate[F](x)
        t <- getTemperatureForCity[F](c)
      } yield t
    }

    println("*"*50)

    type MyEither[A] = Either[String, A]
    println(getTemperateByCoordinates[MyEither](25 -> 93))

    println("*"*50)

    def getTemperatureFromByCoordinatesAlternate[F[_]](x: (Int, Int))(implicit ae: MonadError[F, String]): F[Int] = {
      if(x._1 == 25 && x._2 == 70) ae.raiseError("Invalid coordinates") else for {
        c <- getCityClosestToCoordinate[F](x)
        t <- getTemperatureForCity[F](c)
      } yield t
    }

    println(getTemperatureFromByCoordinatesAlternate((25,70)))

    println("*"*50)

    def combine[F[_, _]: Arrow, A, B, C](fab: F[A, B], fac: F[A, C]): F[A, (B, C)] =
      Arrow[F].lift((a: A) => (a, a)) >>> (fab *** fac)


    val mean: List[Int] => Double =
      combine((_: List[Int]).sum, (_: List[Int]).size) >>> { case (x, y) => x.toDouble / y }

    val variance: List[Int] => Double =
    // Variance is mean of square minus square of mean
      combine(((_: List[Int]).map(x => x * x)) >>> mean, mean) >>> { case (x, y) => x - y * y }

    val meanAndVar: List[Int] => (Double, Double) = combine(mean, variance)

    val mean1 = meanAndVar(List(1, 2, 3, 4))

    println(mean1)

    println("*"*50)


    val headK = Kleisli((_: List[Int]).headOption)
    val lastK = Kleisli((_: List[Int]).lastOption)

    println(headK(List(1,2,3)))

    val headPlusLast = combine(headK, lastK) >>> Arrow[Kleisli[Option, *,*]].lift(((_: Int) + (_: Int)).tupled)

    val result = headPlusLast.run(List(2,3,4,6,8))

    val result2 = headPlusLast.run(Nil)

    println(result)
    println(result2)

    case class FancyFunction[A, B](run: A => (FancyFunction[A, B], B))

    println("-+-"*25)
    val eithers: List[Validated[NonEmptyList[String], Int]] = List(Valid(42), "dsqdqsd".invalidNel, "didqiqsdis".invalidNel, Valid(89))

    println(eithers.sequence: Validated[NonEmptyList[String], List[Int]])


    case class Name(value: String)
    case class Age(value: Int)
    case class Person(name: Name, age: Age)

    def parse(s: String): Either[NonEmptyList[String], Int] = {
      if (s.matches("-?[0-9]+")) Right(s.toInt)
      else Left(NonEmptyList.one(s"$s is not a valid integer."))
    }

    def validateAge(a: Int): Either[NonEmptyList[String], Age] = {
      if (a > 18) Right(Age(a))
      else Left(NonEmptyList.one(s"$a is not old enough"))
    }

    def validateName(n: String): Either[NonEmptyList[String], Name] = {
      if (n.length >= 8) Right(Name(n))
      else Left(NonEmptyList.one(s"$n Does not have enough characters"))
    }

    //This is sick
    def parsePerson(ageString: String, nameString: String) =
      for {
        age <- parse(ageString)
        person <- (validateName(nameString), validateAge(age)).parMapN(Person)
      } yield person

    //Same as above

    def parsePerson1(ageString: String, nameString: String) =
      for {
        age <- parse(ageString)
        person <- (validateName(nameString).toValidated, validateAge(age).toValidated).mapN(Person).toEither
      } yield person

    implicit val showPerson: Show[Person] = Show.show { person => person.name.value}

    println(parsePerson("22", "Berttttt").show)

    val twice: Int => Int =
      x => x * 2

    val countCats: Int => String =
      x => if (x == 1) "1 cat" else s"$x cats"

    val twiceAsManyCats: Int => String =
      twice.andThen(countCats) // equivalent to: countCats compose twice

    println(twiceAsManyCats(1))





    case class Car(name: "Bert", age: 22)


    object JSON {

    }


    val car1 = Car("Bert",22)

    trait Semigroup[A] {
      def combine(a: A, Y: A):A
    }

    trait Monoid[A] extends Semigroup[A] {
      def empty: A
    }





    trait Functor[F[_]] {
      def map[A, B](fa: F[A])(f: A => B): F[B]
    }

  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)

  def do10xFGeneral[F[_]](mappablde: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(mappablde)(_ * 10)

    trait Semigroupal[F[_]] {
      def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
    }

    trait Apply[F[_]] extends Semigroupal[F] with Functor[F] {
      def ap[A, B](fab: F[A => B], fa: F[A]): F[B]

      def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
        val myFunction: A => B => (A, B) = (a: A) => (b: B) => (a, b)

        //In order to change F[A] into a F[B] with a different data type, we need a functor.
        // So we extends functor
        val fab: F[B => (A, B)] = map(fa)(myFunction)

        ap(fab, fb)
      }

      def mapN[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
        map(product(fa, fb)) {
          case (a, b) => f(a, b)
        }
      }
    }

  trait Applicative[F[_]] extends Apply[F] {
    def pure[A](a: A): F[A]

    def ap[A,B](fa: F[A])(f: A => B): F[B] = {
      ap(pure(f), fa)
    }
  }

    trait FlatMap[F[_]] {
      def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
    }

  trait Monad[F[_]] extends Applicative[F[_]] with FlatMap[F] {
    def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
  }

    trait ApplicativeError[F[_], E] extends Applicative[F] {
      def raiseError[A](error: E): F[A]
    }









  }
}
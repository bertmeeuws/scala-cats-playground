import cats.{Applicative, ApplicativeError, Functor, MonadError, Monoid}
import cats.data.Nested
import cats.implicits._
import cats.data.Validated

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



  }
}
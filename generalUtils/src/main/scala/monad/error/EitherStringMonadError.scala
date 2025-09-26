package monad.error
import monad.Monad.{given, *}

object EitherStringMonadError {
    given eitherStringMonadError:MonadError[EitherM[String], String] = new MonadError[EitherM[String], String] {
        def pure[A](v: A): EitherM[String][A] = eitherStringMonad.pure(v)
        def bind[A, B](fa: EitherM[String][A])(f: A => EitherM[String][B]): EitherM[String][B] = 
            eitherStringMonad.bind(fa)(f)
        override def raiseError[A](e: String): EitherM[String][A] = Left(e)
        override def handleErrorWith[A](fa: EitherM[String][A])(f: String => EitherM[String][A]): EitherM[String][A] =fa match {
            case Left(err) => f(err)
            case Right(value) => Right(value)
        }
    }
}
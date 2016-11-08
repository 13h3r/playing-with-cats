package foozie

import cats._
import cats.data._
import cats.implicits._
import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.language.higherKinds

object SprayJsonInstances {
  implicit val jsObjectMonoid = new Monoid[JsObject] {
    override def combine(x: JsObject, y: JsObject): JsObject = {
      JsObject(x.fields ++ y.fields)
    }

    override def empty: JsObject = JsObject.empty
  }
}

case class FieldDependencyT[Field, F[_], Value](fields: Set[Field], value: F[Value]) {
  def apply(f: (Set[Field], F[Value]) => Value): Value = f(fields, value)
}

object FieldDependencyT extends FieldDependencyTInstances with FieldDependencyTFunctions

trait FieldDependencyTFunctions {
}

trait FieldDependencyTInstances {

  implicit def fieldDependencyTApplicative[Field, F[_]](implicit fap: Applicative[F]) = new Applicative[FieldDependencyT[Field, F, ?]] {
    override def pure[A](x: A): FieldDependencyT[Field, F, A] = {
      FieldDependencyT(Set.empty, fap.pure(x))
    }

    override def ap[A, B](ff: FieldDependencyT[Field, F, (A) => B])(fa: FieldDependencyT[Field, F, A]): FieldDependencyT[Field, F, B] = {
      FieldDependencyT(ff.fields ++ fa.fields, fap.ap(ff.value)(fa.value))
    }
  }

  implicit def fieldDependencyTMonoid[Field, F[_], T](implicit monoid: Monoid[F[T]]) = new Monoid[FieldDependencyT[Field, F, T]] {
    override def empty: FieldDependencyT[Field, F, T] = FieldDependencyT(Set.empty, monoid.empty)

    override def combine(x: FieldDependencyT[Field, F, T], y: FieldDependencyT[Field, F, T]): FieldDependencyT[Field, F, T] = {
      FieldDependencyT(x.fields ++ y.fields, x.value.combine(y.value))
    }
  }
}


object SimpleFDTest extends App {

  type Hash = String
  type HashReaderT[F[_], T] = ReaderT[F, Hash, T]

  object HashReaderT {
    def lift[F[_], A](x: F[A]): HashReaderT[F, A] = Kleisli.lift[F, Hash, A](x)
  }

  type RS = Map[String, Any]
  type CT[X] = FieldDependencyT[String, RS => ?, X]


  val name = new CT(Set("name"), rs => JsObject("name" -> rs("name").toString.toJson))
  val link = new CT(Set("link"), rs => JsObject("link" -> rs("link").toString.toJson))
  val hash = new HashReaderT[Id, JsObject](hash =>
    JsObject("hash" -> hash.toJson)
  )

  val item: HashReaderT[CT, JsObject] = List(
    HashReaderT.lift(name)
    , HashReaderT.lift(link)
    , hash.transform(Lambda[Id ~> CT](_.pure[CT]))
  ).combineAll


  val res = item.apply("HASH").apply {
    case (fields, result) =>
      println(fields)
      result(Map("name" -> "NAME", "link" -> "LINK"))
  }
  println(res)


}
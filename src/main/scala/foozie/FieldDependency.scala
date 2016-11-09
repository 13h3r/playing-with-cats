package foozie

import cats._
import cats.data._
import cats.implicits._
import spray.json.DefaultJsonProtocol._
import spray.json._
import SprayJsonInstances._
import FieldDependencyT._

import scala.language.higherKinds

object SprayJsonInstances {
  implicit val jsObjectMonoid = new Monoid[JsObject] {
    override def combine(x: JsObject, y: JsObject): JsObject = {
      JsObject(x.fields ++ y.fields)
    }

    override def empty: JsObject = JsObject.empty
  }
}

case class FieldDependencyT[Field, Value](fields: Set[Field], value: Value) {
  def apply[T](f: (Set[Field], Value) => T) = f(fields, value)
}

object FieldDependencyT extends FieldDependencyTInstances with FieldDependencyTFunctions

trait FieldDependencyTFunctions {
}

trait FieldDependencyTInstances {
  implicit def fieldDependencyTApplicative[Field] = new Applicative[FieldDependencyT[Field, ?]] {
    override def pure[A](x: A): FieldDependencyT[Field, A] = {
      FieldDependencyT(Set.empty, x)
    }

    override def ap[A, B](ff: FieldDependencyT[Field, (A) => B])(fa: FieldDependencyT[Field, A]): FieldDependencyT[Field, B] = {
      new FieldDependencyT(ff.fields ++ fa.fields, ff.value(fa.value))
    }
  }

  implicit def fieldDependencyTMonoid[Field, T](implicit monoid: Monoid[T]) = new Monoid[FieldDependencyT[Field, T]] {
    override def empty: FieldDependencyT[Field, T] = {
      monoid.empty.pure[FieldDependencyT[Field, ?]]
    }

    override def combine(x: FieldDependencyT[Field, T], y: FieldDependencyT[Field, T]): FieldDependencyT[Field, T] = {
      FieldDependencyT(x.fields ++ y.fields, x.value.combine(y.value))
    }
  }
}

object SimpleFDTest extends App {
  type Hash = String
  type HashReader[T] = Reader[Hash, T]

  object HashReader {
    def lift[A](x: A): HashReader[A] = Reader.apply(_ => x)
  }

  type RS[T] = Map[String, Any] => T
  type OneItem[T] = FieldDependencyT[String, T]
  object OneItem {
    def apply[T](deps: Set[String], value: T) = new FieldDependencyT[String, T](deps, value)
  }


  val name = OneItem[RS[JsObject]](Set("name"), rs => JsObject("name" -> rs("name").toString.toJson))
  val link = OneItem[RS[JsObject]](Set("link"), rs => JsObject("link" -> rs("link").toString.toJson))
  val hash = new HashReader(hash =>
    JsObject("hash" -> hash.toJson)
  )

  type Stack[T] = HashReader[OneItem[RS[T]]]

//  implicit val y = implicitly[Monoid[HashReader[OneItem[RS[JsObject]]]]]
  implicit val x = implicitly[Monoid[Stack[JsObject]]]
//  implicit val x: Applicative[Stack] = implicitly[Applicative[HashReader]].compose(implicitly[Applicative[OneItem]]).compose(implicitly[Applicative[RS]])

  val item = List[Stack[JsObject]](
    HashReader.lift(name)
    , HashReader.lift(link)
    , hash.map(_.pure[RS].pure[OneItem])
//    name, link
  ).combineAll

//  val res = item.apply("HASH").apply {
//    case (fields, result) =>
//      println(fields)
//      result(Map("name" -> "NAME", "link" -> "LINK"))
//  }
//  println(res)
}
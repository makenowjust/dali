package codes.quine.labo
package dali
package higher

trait TypeFunction1 { type Apply[_] }

sealed trait Const1[A] extends TypeFunction1 { type Apply[_] = A }

sealed trait Param1 extends TypeFunction1 { type Apply[A] = A }
sealed trait Rec1[F[_]] extends TypeFunction1 { type Apply[A] = F[A] }

sealed trait HList1 extends TypeFunction1 { type Apply[_] <: HList }
sealed trait HNil1 extends HList1 { type Apply[_] = HNil }
sealed trait :**:[H <: TypeFunction1, T <: HList1] extends HList1 { type Apply[A] = H#Apply[A] :*: T#Apply[A] }

sealed trait Coproduct1 extends TypeFunction1 { type Apply[_] <: Coproduct }
sealed trait CNil1 extends Coproduct1 { type Apply[_] = CNil }
sealed trait :++:[H <: TypeFunction1, T <: Coproduct1] extends Coproduct1 { type Apply[A] = H#Apply[A] :+: T#Apply[A] }

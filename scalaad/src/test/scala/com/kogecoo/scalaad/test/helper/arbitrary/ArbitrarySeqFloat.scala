package com.kogecoo.scalaad.test.helper.arbitrary

import com.kogecoo.scalaad.graph.{ContainerConst, Node, ScalarConst, Var}
import com.kogecoo.scalaad.test.helper.gen.{SeqFloatNodeGen, SeqFloatValueGen}
import com.kogecoo.scalaad.value.{ContainerValue, NonContainerValue, Value}
import org.scalacheck.Arbitrary


object ArbitrarySeqFloat {

    private[this] val nodeGen = new SeqFloatNodeGen

    private[this] val valueGen = new SeqFloatValueGen


    object Implicits {

        implicit def arbNode(implicit a: Arbitrary[Node[Seq, Float]]): Arbitrary[Node[Seq, Float]] = Arbitrary {
            nodeGen.genNode()
        }

        implicit def arbVar(implicit a: Arbitrary[Var[Seq, Float]]): Arbitrary[Var[Seq, Float]] = Arbitrary {
            nodeGen.genVar()
        }

        implicit def arbScalarConst(implicit a: Arbitrary[ScalarConst[Seq, Float]]): Arbitrary[ScalarConst[Seq, Float]] = Arbitrary {
            nodeGen.genScalarConst()
        }

        implicit def arbContainerConst(implicit a: Arbitrary[ContainerConst[Seq, Float]]): Arbitrary[ContainerConst[Seq, Float]] = Arbitrary {
            nodeGen.genContainerConst()
        }

        implicit def arbValue(implicit a: Arbitrary[Value[Seq, Float]]): Arbitrary[Value[Seq, Float]] = Arbitrary {
            valueGen.genValue()
        }

        implicit def arbNonContainerValue(implicit a: Arbitrary[Value[Seq, Float]]): Arbitrary[NonContainerValue[Seq, Float]] = Arbitrary {
            valueGen.genNonContainerValue()
        }

        implicit def arbContainerValue(implicit a: Arbitrary[Value[Seq, Float]]): Arbitrary[ContainerValue[Seq, Float]] = Arbitrary {
            valueGen.genContainerValue()
        }
    }

}

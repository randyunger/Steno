/**
 * Copyright 2009 The Apache Software Foundation
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.gravity.steno

import scalaz._, Scalaz._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import com.gravity.steno.grvannotation.BasicNoteTypes.{MsgNote, Note}


/**
 * Created by runger on 4/21/14.
 */

@RunWith(classOf[JUnitRunner])
abstract class BaseScalaTest extends FunSuite with ShouldMatchers

class FailureResult(val message: String, val exceptionOption: Option[Throwable]) extends Serializable

object FailureResult {
  def apply(message: String, exceptionOption: Option[Throwable]): FailureResult = new FailureResult(message, exceptionOption)

  def apply(ex: Throwable): FailureResult = FailureResult(ex.getMessage, Some(ex))

  def apply(msg: String, ex: Throwable): FailureResult = FailureResult(msg, Some(ex))

  def apply(msg: String): FailureResult = FailureResult(msg, None)

  implicit def failureResultCanBeNote[N <: FailureResult] = new grvannotation.CanBeNote[N] {
    override def toNote(n: N): Note = MsgNote(n.message)
  }
}

class AnnotationTest extends BaseScalaTest {

  test("Basics of Annotated") {
    import grvannotation._
    import BasicNoteTypes._

    Annotated("some value of any type", Seq(
      MsgNote("associated notes about that value"),
      new SimpleMessageMetaNote("Can stash strongly typed meta data too", meta = (42, "Anything I want", Map.empty), canContinue = true)
    ))
    //Helpers exist for displaying notes & retrieving meta

    //Can also instantiate like so
    val res = "some value".annotate("This method may be less intrusive with existing code") //TypeClass based, so can accept many parameter types - String, FailureResult, RecoFailure - easy to extend.
    //I use the .annotate method throughout the demo

    println(res)
  }

  test("What's lacking from ValidationNel") {

    val failSafe = 42

    // A process that makes 2 attempts and then has a failsafe backup
    def attempt1: ValidationNel[FailureResult, Int] = FailureResult("Live process failed").failNel[Int]
    def attempt2: ValidationNel[FailureResult, Int] = FailureResult("Backup process failed").failNel[Int]
    def process: ValidationNel[FailureResult, Int] = {
      val failures = (attempt1 |+| attempt2)
      println(failures)                               //Failure(NonEmptyList(FailureResult:	"Live process failed", FailureResult:	"Backup process failed"))

      // ValidationNel lets us aggregate our failures, but it's not easy to return them along with a success
      // Annotated is kinda like (ValidationNel[F,S], List[F]) on steroids
      failures.getOrElse(failSafe).successNel[FailureResult]
    }

    process
  }

  test("sequence of steps"){
    /*
    We've got a two-stage process - take a sqrt of a double, and invert it
     */
    def sqrt(d: Double): ValidationNel[FailureResult, Double] = math.sqrt(d) match {
      case nan if nan == Double.NaN => FailureResult(s"Unable to calculate sqrt for $nan").failNel
      case sq => sq.successNel
    }

    def invert(d: Double): ValidationNel[FailureResult, Double] = {
      d match {
        case nan if nan.equals(Double.NaN) => {
          FailureResult("Can't invert NaN").failNel
        }
        case dub => 1/dub match {
          case inf if inf.equals(Double.PositiveInfinity) => {
            FailureResult("Invert produced infinity").failNel
          }
          case inv => {
            inv.successNel
          }
        }
      }
    }

    /*
    These stages comprise a pipeline
     */
    def pipeline(d: Double): ValidationNel[FailureResult, Double] = {
      val sq = sqrt(d)
      sq match {
        case Failure(fs) => fs.fail
        case Success(dub) => invert(dub)
      }
      // If we had a recovery step here we would either
      // A) lose the informative FailureResults or
      // B) have to return them along with our Success value
      // Annotated is a strategy for B
    }

    println(pipeline(64))
    println("*"*10)
    println(pipeline(0))
    println(pipeline(-1))

  }

  test("Annotated with Stage"){
    import grvannotation._
    import com.gravity.steno.grvstage._
    import BasicNoteTypes._

    case object SqrtStage extends StageWithMeta {   //reference to SqMeta breaks unless case object extends. Can't do val sqrtStage = new StageWithMeta

      trait SqStageNote extends Note    // A trait to indicate this stages notes

      type StageNote = SqStageNote      //The type of notes contained in this stage

      type MetaType = SquareRootMeta            //The type of meta data

      case class SquareRootMeta(aDouble: Option[Double] = None, secondMetaType: Option[ValidationNel[FailureResult, Double]] = None)
      //This above format of meta data (Option[X]=None) makes it easy to attach only the meta data that is in scope at any time.
      //also allows us to retrieve it in a type-safe way

      def makeNote(message: String) = new MsgNote(message) with StageNote
      // You need to provide an implementation for each stage to construct a note with that Stages StageNote type. Unfortunately, this cannot be abstracted by the base class.
      // Fortunately, it can be copied & pasted in each implementation

      def makeMetaNote(message: String, meta: MetaType, continuable: Boolean = true) = new SimpleMessageMetaNote(message, meta, continuable) with StageNote
      // Provide an implementation to wrap your meta in a note.

      val noteCT = classTag[StageNote]  // An unfortunately exposed implementation detail. Copy and paste this line into each Stage implementation.
      // This is how we recover the type information lost due to erasure. It allows us to filter notes by stage.

      val label = "Square Root Stage"
    }

    case object InversionStage extends StageWithMeta {
      trait InversionStageNote extends Note
      type StageNote = InversionStageNote
      type MetaType = InversionStageMeta
      case class InversionStageMeta(meta1: Option[Double] = None, somethingElse: Option[Double] = None, stashTheResult: Option[ValidationNel[FailureResult, Double]] = None)
      def makeNote(message: String) = new MsgNote(message) with StageNote
      def makeMetaNote(message: String, meta: MetaType, continuable: Boolean = true) = new SimpleMessageMetaNote(message, meta, continuable) with StageNote
      val noteCT = classTag[StageNote]
      val label = "Inverse stage"
    }

    //Also have an IntelliJ LiveTemplate to generate a StageWithMeta

    def sqrt(d: Double): Annotated[ValidationNel[FailureResult, Double]] = {
      import SqrtStage._

      //Import your stage for convenient implicits and access to makeNote and makeMetaNote as well as that stages Note types and aliases
      //Import brings in
      // def makeNote
      // def makeMetaNote
      // typeclasses specialized by stage - allows .annotate

      val result: grvannotation.Annotated[Validation[NonEmptyList[FailureResult], Double]] = math.sqrt(d) match {
        case nan if nan.equals(Double.NaN) => {
          FailureResult(s"Unable to calculate sqrt for $nan").failNotes
          // failNotes turns any value with a CanBeNote typeclass into a Annotated[Failure[NonEmptyList[F], X]]
          // where F is the type being operated on and X is defined on a different code path
          // or is supplied via type parameter like x.failNotes[SuccessType]
          // This is analogous to scalaz's .failNel
        }
        case sq => sq.successNotes(s"Square root of $d is $sq").annotate("successfully got the sqrt")
        //successNotes is similarly sugar for Annotated[Failure[NonEmptyList[X], S]] where S is being operated on.
        // equivalent to sq.successNel[FailureResult].annotate(s"Square root of $d is $sq")
      }
      result.annotate(makeMetaNote("result of sqrt stage", SquareRootMeta(aDouble = d.some, secondMetaType = result.value.some)))
      //.annotate is overloaded in multiple places for convenience. This is your goto.
      //.annotate can also be used to stick another note on the Seq without transforming the value
    }

    def invert(d: Double): Annotated[ValidationNel[FailureResult, Double]] = {
      import InversionStage._
      val notes = scala.collection.mutable.ArrayBuffer.empty[Note]
      // Another pattern for aggregating notes. This is useful when you're combining sub-steps that each return Annotated
      // ArrayBuffer is useful for accumulating

      notes += makeMetaNote("About to invert", InversionStageMeta(meta1 = d.some))
      val result = d match {
        case nan if nan.equals(Double.NaN) => {
          notes += makeMetaNote("Input to invert was a nan", InversionStageMeta(somethingElse = nan.some), continuable = false)
          FailureResult("Can't invert NaN").failNel
        }
        case dub => 1/dub match {
          case inf if inf.equals(Double.PositiveInfinity) => {
            notes += makeMetaNote("Invert produced infinity. Did we operate against 0?", InversionStageMeta(somethingElse = inf.some), continuable = false)
            FailureResult("Invert produced infinity").failNel
          }
          case inv => {
            notes += makeMetaNote("Inversion was successful", InversionStageMeta(somethingElse = inv.some), continuable = true)
            inv.successNel
          }
        }
      }
      result.annotate(notes).annotate(makeMetaNote("Result of invert stage", InversionStageMeta(stashTheResult = result.some)))
    }

    def compute(d: Double): Annotated[ValidationNel[FailureResult, Double]] = {
      val sq = sqrt(d)

      sq.flatMap(s =>{
        s match {
          case Success(d) => invert(d)
          case f@Failure(_) => Annotated(f)
        }
      })
      //Can flatmap to combine notes lists
    }

    val computePipeline = Pipeline(SqrtStage, InversionStage)
    //Defining a pipeline gives us access to methods that can group and render our notes for us.

    val compute64 = compute(64)
    println("Success case")
//    computePipeline.print(compute64.notes)
    //Print the notes in a tabulated form. See com.gravity.utilities.Tabulator

    val compute0 = compute(0)
    println("Failure case")
//    computePipeline.print(compute0.notes)

    val notesDescribingSeriousFailures = computePipeline.getNotContinuable(compute0.notes)
    //Find the notes that were created with canContinue = false

    println("Print the notes that associated with the failure")
//    computePipeline.print(notesDescribingSeriousFailures)

    val inputThatCausedTheFailure: Seq[Double] = InversionStage.retrieve(compute0.notes)(_.meta1)
    val doubleAtTimeOfFailure: Seq[Double] = InversionStage.retrieve(compute0.notes)(_.somethingElse)
    //We can recover values stashed as meta in a typesafe way via retrieve

    println(s"The input which caused trouble: $inputThatCausedTheFailure")
    println(s"The resulting double: $doubleAtTimeOfFailure")

    doubleAtTimeOfFailure should equal(Seq(Double.PositiveInfinity))
  }
}

//Todos:
// Flesh out API for non-staged and non-meta use
// Interface that allows you to apply notes to a stage or pipeline a single time, and then call .print or .retrieve etc
// Make it impossible to nest (Annotated[Annotated[V]])


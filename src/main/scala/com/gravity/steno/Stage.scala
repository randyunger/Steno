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

import scala.reflect.ClassTag

/**
 * Created by runger on 5/13/14.
 */
object grvstage {
  //  import com.gravity.utilities.Tabulator._
  import grvannotation.BasicNoteTypes._
  import grvannotation._

  /**
   * A Stage is way of grouping together a Seq of notes. Defining a stage provides methods for filtering, grouping, rendering, and printing.
   */
  trait Stage {

    //Give us these four
    def label: String
    type StageNote <: Note
    def noteCT: ClassTag[StageNote]
    def makeNote(message: String): StageNote

    //And you get these
    def containsNote(note: Note): Boolean = noteCT.runtimeClass.isInstance(note)  //You might think we could note.isInstanceOf[StageNote] // but we can't. (StageNote seems to refer to Stage#StageNote which matches all stages)
    def filter(notes: Seq[Note]): Seq[StageNote] = notes.filter(note => this.containsNote(note)).map(_.asInstanceOf[StageNote])

    //helpers
    def classTag[T: ClassTag] = scala.reflect.classTag[T]
    implicit def stageRef = this

    implicit def stringCanBeNote = new CanBeNote[String] {
      override def toNote(msg: String): Note = makeNote(msg)
    }

    //    implicit def failureResultCanBeNote[T <: FailureResult] = new CanBeNote[T] {
    //      override def toNote(f: T): Note = makeNote(f.message)
    //    }

    //todo: Shorthand for defining a Stage??
    //  def config5[A: ClassTag, B: ClassTag, C: ClassTag, D: ClassTag, E: ClassTag](label: String) = StageConfig(label, List(classTag[A], classTag[B], classTag[C], classTag[D], classTag[E]), None)
  }

  trait StageWithMeta extends Stage {
    //Give us these two

    /**
     * The type of this Stage's Meta. Should be a case class like
     * case class StageMeta(name: Option[String] = None, context: Option[(URL, Timestamp)])
     */
    type MetaType <: Product //<: Meta //todo: interesting things with a Meta trait?

    /**
     * Define a method that wraps your MetaType in
     * @param message A message describing the meta data
     * @param meta The meta data to be stashed in this note
     * @param continuable False indicates that an unrecoverable error is indicated by this note
     * @return a Note instance with meta data suitable for stashing in an Annotated instance
     */
    def makeMetaNote(message: String, meta: MetaType, continuable: Boolean): BasicMetaNote[MetaType] with BasicMessageNote with StageNote

    /**
     * @param notes The notes resulting from a process that returns Annotated
     * @return The BasicMessageMetaNotes for this stage, untyped
     */
    def metaNotes(notes: Seq[Note]): Seq[StageNote with BasicMessageMetaNote[_]] = {                                   //vs BasicMetaNote? Clutters the api.
      notes.filter(note => this.containsNote(note) && note.isInstanceOf[BasicMessageMetaNote[_]]).map(_.asInstanceOf[StageNote with BasicMessageMetaNote[_]])
    }

    /**
     * Finds the meta notes for this stage and casts the meta to the appropriate type, with the message and canContinue as additional Tuple members
     * @param notes The notes resulting from a process that returns Annotated
     * @return A Tuple3 with a properly typed MetaType instance, Message, and CanContinue
     */
    def metaTuple(notes: Seq[Note]): Seq[(MetaType, String, Boolean)] = {
      metaNotes(notes).map(metaNote => (metaNote.meta.asInstanceOf[MetaType], metaNote.message, metaNote.canContinue))
    }

    /**
     * Finds the meta notes for this stage and casts the meta to the appropriate type, without the message nor canContinue
     * @param notes The notes resulting from a process that returns Annotated
     * @return A Seq of properly type MetaType instances
     */
    def meta(notes: Seq[Note]): Seq[MetaType] = {
      metaNotes(notes).map(metaNote => metaNote.meta.asInstanceOf[MetaType])
    }

    /**
     * Find the instances of MetaType that have a value of Some for the field supplied by f
     * @param notes The notes resulting from a process that returns Annotated
     * @param f indicates a field of MetaType to be sought
     * @tparam T The type of the field sought
     * @return A Seq of only those instances of MetaType where the sought field is defined
     */
    def retrieve[T](notes: Seq[Note])(f: MetaType => Option[T]) = {
      meta(notes).flatMap(meta => f(meta))
    }

    def getNotContinuable(notes: Seq[Note]) = {
      metaNotes(notes).filterNot(_.canContinue)
    }

    //todo Wrap up these defs that take SEeq[notes] somehwere else?
    implicit def stageMetaRef = this
  }

  //  implicit val tabTriples: CanBeTabulated[(String, String, String)] = new CanBeTabulated[(String, String, String)] {
  //    property("Stage")(_._1)
  //    property("Note")(_._2)
  //    property("Meta")(_._3)
  //  }
  //
  //  def print(stages: Traversable[RenderedStage]) = {
  //    val out = stages.flatMap(stage => (stage.label, "", "") +: stage.metaNotes.map{ case RenderedMetaNote(message, meta) => ("", message, meta)} )
  //    out.tablePrint()
  //  }


  //  def baseRenderPF(renderMeta: PartialFunction[Any, String]): PartialFunction[Note, RenderedMetaNote] = {
  //    case n: BasicMessageMetaNote[_] => RenderedMetaNote(n.message, grvstrings.renderCaseClassOfOptionsRecursive(n.meta, withClassnameWrapper = false)(renderMeta)) //n.meta.toString)
  //    case n: BasicMetaNote[_] => RenderedMetaNote("", n.meta.toString)
  //    case n: BasicMessageNote => RenderedMetaNote(n.message, "")
  //    //todo: Make it easier to override renderPF with Stage-specific meta to allow Shows
  //  }

  case class RenderedMetaNote(message: String, meta: String)
  case class RenderedStage(label: String, metaNotes: Seq[RenderedMetaNote])
  case class NotesForStage(stageLabel: String, notes: Seq[Note])


  //  trait StageWithRenderMeta extends StageWithMeta {
  //    def renderMeta: PartialFunction[Note, RenderedMetaNote]
  //  }

  val idPF: PartialFunction[Any, String] = {
    case x => x.toString
  }

  case class Pipeline(stages: Stage*){

    def group(notes: Seq[Note]) = stages.map(stage => (stage.label, stage.filter(notes)))

    //    def render(notes: Seq[Note])(renderPF: PartialFunction[Any, String]) = {  //perhaps PFs can be extracted from stages and traversed or sequenced?
    //      stages.map {  //todo simple stage =>
    //        case stage: Stage => {
    //          val label = stage.label
    //          val stageNotes = stage.filter(notes)
    //          val renderedNotes = stageNotes.map(baseRenderPF(renderPF))
    //          RenderedStage(label, renderedNotes)
    //        }
    //      }
    //    }

    //    def printPF(notes: Seq[Note])(renderPF: PartialFunction[Any, String] = idPF) = {
    //      grvstage.print(render(notes)(renderPF))
    //    }
    //
    //    def print(notes: Seq[Note]) = {
    //      grvstage.print(render(notes)(idPF))
    //    }

    def stagesWithMeta = stages.collect {
      case st:StageWithMeta => st
    }

    def getNotContinuable(notes: Seq[Note]) = {
      stagesWithMeta.flatMap(stage => stage.getNotContinuable(notes))
    }
  }
}
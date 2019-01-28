package org.example
import java.net.URL
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

import org.opalj.br.analyses.{BasicReport, DefaultOneStepAnalysis, Project}

//import scala.collection.JavaConverters._

object Main extends DefaultOneStepAnalysis {

  override def description = "Counts the number of public methods."

  def doAnalyze(
    project: Project[URL],
    parameters: Seq[String],
    isInterrupted: () ⇒ Boolean): BasicReport = {

    val nativeMethods =
      for {
        classFile ← project.allClassFiles.par
        method ← classFile.methods
        if method.isPublic
      } yield method.toJava

    val libs = for {
      lf <- project.allLibraryClassFiles.par
    } yield lf

    val libCount = libs.toVector.size

    val packs = for {
      pf <- project.packages
    } yield pf

    val seppacks = for {
      p <- packs
      np <- p.split("/")
    } yield np


    val packcount = packs.size

//Element mit der größten Häufigkeit finden
val list = new ListBuffer[Int]
    val strings = seppacks.toVector
    val freq = strings.groupBy(identity).mapValues(_.size)
    var elem = {strings(0)}
    var max = 0
    for (i <- strings) {
      if (max < freq(i)) {
        max = freq(i)
        elem = i
      }
    }

     //TODO: Element mit der größten Häufigkeit an Java Word2Vec übergeben
    //TODO: Vektor von Word2Vec Ergebnis erhalten und speichern
    //TODO: Trennen der Methodenbezeichner
    //TODO: rausfiltern von public, static, void, private und allen Datentypen

    //Vektor mit allen Methoden
      val methhelp = for {
      cf <- project.allClassFiles.par
      mt <- cf.methods
      if !mt.isSynthetic
      if !mt.isConstructor
    } yield mt.name


/*
    val met = for {
      m <- methhelp
      mpas <-  m.split("(")
    } yield mpas


    val methods = met2.filter(_== "public")
    val methods1 = methods.filter(_== "private")
    val methods2 = methods1.filter (_== "boolean")
    val methods3 = methods2.filter (_== "Int")
    val methods4 = methods3.filter (_== "protected")
    val methods5 = methods4.filter (_== "void")

*/

      // Anzahl der Methoden
      val mc = methhelp.toVector.size

      /*Enter here
      val tester = for {
        ot <- Vector ("test", "heLlo", "there").toList
        bt <- ot.toLowerCase
      } yield bt


      val goodOnes = for {
        ob <- Vector("TESt", "HAllo", "thEre").toList
        bb <- ob.toLowerCase
      } yield bb
  */


    //derzeit noch manuell von Word2Vec zu übernehmen
      val goodMOnesIn = "analyze, apply, events, latest, running, part, here, attachments, most, check, provided, exactly, whether, could, created, does, case, these, eg, task-local, main, link, historic, component, assignee, marks, wide, send, added, recorded, known, #setassigneestring, defined, enabled, nothing, end-user, updated, claimed, associate, newly, made, performed, instance<li>, another, already, subscribed, processes, #claim}, related, inside, delegates, payload, successfully, also, correlated, started, being, delegated, difference, identitylink}s, event, persistent, instances, message, start, executed, sent, specified, comments, unclaim, non, instead, stored, claims, called, information, owner, happens, subscription, saved, schema-name, associated, build, deletes, drops, operation, store, runtime, referenced, deleting, activitieventdispatcher}, reason, operations, checks, existing, intermediate, boundary, transfers, single, finds, group, subprocess"
    val goodMOnes = goodMOnesIn.split(", ").toList
    //TODO: typische Feldnamen finden und ergänzen auch bei der Fallprüfung
    val generaltypicalFieldNamesIn = "task, name, parameter, static, entity, state, container, manager, counter, util, config, handler, filter, parameters, files"
    val generaltypicalFieldNames = generaltypicalFieldNamesIn.split(", ").toList

    println(goodMOnes)

    var goodCount = 0

      val equalsA = for {
        mt <- methhelp
        if mt.contains("get")
      } yield mt

      val EqualsACount = equalsA.toVector.size

    val equalsB = for {
      mz <- methhelp
      if mz.contains("set")
    } yield mz

    val EqualsBCount = equalsB.toVector.size

    val equalsCa = for {
      mu <- methhelp
      m <- mu.split("(?=\\p{Upper})")
    } yield m.toLowerCase

    val equalsC = for {
      m <- equalsCa
      if goodMOnes.contains(m)
    } yield m

    println("this are the matching method names" + equalsC)

    val equalsD = for {
      mz <- equalsC
      if mz.contains("new")
    } yield mz

    val EqualsDCount = equalsD.toVector.size

    val EqualsCCount = equalsC.toVector.size

    goodCount = EqualsACount + EqualsBCount + EqualsCCount + EqualsDCount

    val methodScore = goodCount.toFloat/mc.toFloat


      //TODO: Felder untersuchen

      //erstellt einen Vektor mit alen Feldnamen
      val fields = for {
        fn <- project.allFields
      } yield fn.name

    //trennt die Wörter bei Großbuchstaben
    val sepfields = for {
      f <- fields
      fn <- f.split("(?=\\p{Upper})")
    } yield fn

    // macht alles zu kleinen Buchstaben
    val smallfields = for {
      f <- sepfields
    } yield f.toLowerCase

    //liefert alle Strings der Felder ohne
    val PartA = for {
      fd <- smallfields
      if fd.contains("val")
    } yield fd

    val CleanFieldA = for {
      word <- PartA
    } yield  word.substring(4)

    val CleanFieldB = for {
      fd <- smallfields
      if !fd.contains("val" )
    } yield fd


    println("my fields are with vals: " + CleanFieldA + " and normals: " + CleanFieldB)

    var goodFieldCount = 0

    val checkFieldAa = for {
      fa <- CleanFieldA
      if goodMOnes.contains(fa)
    } yield fa

    val checkFieldAb = for {
      fb <- CleanFieldA
      if generaltypicalFieldNames.contains(fb)
    } yield fb

    val checkFieldACount = checkFieldAa.toVector.size + checkFieldAb.toVector.size

    val checkFieldBa = for {
      fa <- CleanFieldB
      if goodMOnes.contains(fa)
    } yield fa

    val checkFieldBb = for {
      fb <- CleanFieldB
      if generaltypicalFieldNames.contains(fb)
    } yield fb

    val checkFieldBCount = checkFieldBa.toVector.size + checkFieldBb.toVector.size

    //Anzahl der übereinstimmenden Felderbezeichner
    goodFieldCount = checkFieldACount + checkFieldBCount

    //Anzahl der Felder in dem Projekt
    val fieldCount = fields.toVector.size

    println("number of matching fields: " + goodFieldCount)

    val FieldScore = goodFieldCount.toFloat/fieldCount.toFloat


    val combinedValue = (FieldScore + methodScore) / 2
    println("results in the following Field Score: " + FieldScore)


      println ("number of fields " + fieldCount)
      println("number of methods " + mc + " which are: " + methhelp)
      println ("number of Libraries " + libCount)
      println ("number of packages " + packcount + "which are the following: " + packs + " seperated looks like: " + seppacks)
      println(max)
      println("The most frequent element is: " + elem)
      println ("matching methods " + goodCount)
      println ("method Score is "+ methodScore)
      println ("methods to check are " + equalsC)
    println("general score = " + combinedValue)
      //TODO: Trennung der Feldnamen bei Großbuchstaben

      //TODO: Vektor mit guten Feldnamen der goodFieldList heißt

      /* val goodFields = for {
         field <- fields.toVector
         if field.equals(good)
       } yield field

         val goodFieldNameCount = goodFields.size

       println("number of fields and methods with same name " + goodFieldNameCount)

       val fieldRelation = goodFieldNameCount.toFloat/fieldCount.toFloat

   */

      //TODO: Kombination der Relationen
      // val CombinedValue = Word2VecUptrainingExample.run().asScala



      /*
           def prepareVector(lv: Vector[Int]): Vector[Vector[Int]] = {
           val mv = new ArrayBuffer[Vector[Int]]()

           def filter(methode: methods[Int]): match = methode match {
             case get :: word = word - get; if goodConstructors.contains(word) --> goodConstructors++ // in richtige Syntax
             case set :: word = word - set; if goodConstructors.contains(word) --> goodConstructors++ //in richtige Syntax
             }
             case y :: ys => {
               mv += Vector(y)
               go(ys)
             }
             case Nil => None
           }

           go(lv.toList)

           mv.toVector
         }
          */

      val publicMethodsCount = nativeMethods.size
      val r = nativeMethods.mkString(s"$publicMethodsCount public methods found:\n\t", "\n\t", "\n")
    println("results in the following Field Score: " + FieldScore)
    println ("method Score is "+ methodScore)
    println("general score = " + combinedValue)
      BasicReport(r + "Method Score: " + methodScore + "Field Score: " + FieldScore + "combined Value: " + combinedValue)

    }

  }
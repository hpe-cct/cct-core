name := "cct-core"

description := "HPE Cognitive Computing Toolkit core"

organizationName := "Hewlett Packard Labs"

organizationHomepage := Some(url("http://www.labs.hpe.com"))

version := "5.0.1"

organization := "com.hpe.cct"

scalaVersion := "2.11.7"

// add scala module dependencies when needed (for Scala 2.11 and newer) in a robust way
// this mechanism supports cross-version publishing
// taken from: http://github.com/scala/scala-module-dependency-sample
libraryDependencies := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    // if scala 2.11+ is used, add dependency on needed scala modules
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      libraryDependencies.value ++ Seq(
        "org.scala-lang.modules" %% "scala-xml" % "1.0.4",
        "org.scala-lang.modules" %% "scala-swing" % "1.0.2",
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "com.hpe.cct" %% "hdf-java" % "2.11.0-hpe.3")
    case _ =>
      libraryDependencies.value ++ Seq(
        "org.scala-lang" % "scala-swing" % scalaVersion.value,
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "com.hpe.cct" %% "hdf-java" % "2.11.0-hpe.3")
  }
}

parallelExecution in Test := false

// Updated from 2.1.4 in prep for move to scala 2.11
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.14"

libraryDependencies += "org.jogamp.jogl" % "jogl-all-main" % "2.3.2"

libraryDependencies += "org.jogamp.gluegen" % "gluegen-rt-main" % "2.3.2"

libraryDependencies += "org.jogamp.jocl" % "jocl-main" % "2.3.2"

// Was 1.9.1.3.  Important compareTo bug fixed in 1.13.0.0. East->West layouts still broken as of 2.3.0.5
libraryDependencies += "org.tinyjee.jgraphx" % "jgraphx" % "3.4.1.3"

libraryDependencies += "org.swinglabs.swingx" % "swingx-core" % "1.6.5-1"

libraryDependencies += "jfree" % "jfreechart" % "1.0.13"

libraryDependencies += "jfree" % "jcommon" % "1.0.16"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"

libraryDependencies += "gov.nist.math" % "jama" % "1.0.3"

libraryDependencies += "io.github.bchandle" %% "im-scala-swing" % "1.0"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.7"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

libraryDependencies += "junit" % "junit" % "4.7" % "test"

resolvers ++= Seq(Resolver.bintrayRepo("bchandle", "maven"),
                  Resolver.bintrayRepo("hpe-cct", "maven"))

licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html"))

bintrayRepository := "maven"

bintrayOrganization := Some("hpe-cct")

// The following line allows sbt's "publish-local" command to
// find 'javac' without the user having to add "$JAVA_HOME/bin"
// to the PATH environment variable.  This was a problem discovered 
// on Windows for this project, which has java sources.  The
// java.home system property of the JVM points to the jre, which
// is one diretory below the jdk home (hence the "..").

javaHome := Some(new File(System.getProperty("java.home"),".."))

// When sbt doc is run, copy image files used within Scaladoc to the target dir.
// A first unsuccessful attempt at this created a 'copyDocImagesTask' and linked it to
// the doc task by 'triggeredBy'.  The problem was that this ran in parallel (and hence
// raced) with the publishDoc task which depends on doc alone.  The following approach
// performs the dir copy action as a side effect of a routine that maps docRoot -> docRoot
// that is run at the tail end of doc.

doc in Compile <<= (doc in Compile) map { docRoot =>
  println("Copying doc images")
  val sourceDir = file("src/main/resources/doc-images")
  val targetDir = docRoot / "doc-images"
  IO.copyDirectory(sourceDir, targetDir)
  docRoot
}

// Sbt adds jars found in ./lib to the classpath, but we'd prefer to organize the
// hdf5 jars in a subdirectory ./lib/hdf5.  The following accomplishes this:

// I'm leaving this here (but commented out) for reference, just in case 
// another library needs to be loaded in a similar manner in the future.
// HDF5 no longer lives in the libcog lib directory.
//  -Ben

//unmanagedJars in Compile <++= baseDirectory map { base =>
//  val baseDirectories = (base) +++ (base / "hdf5")
//  val customJars = (baseDirectories ** "*.jar")
//  customJars.classpath
//}

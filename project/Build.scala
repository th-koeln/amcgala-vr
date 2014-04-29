import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import sbt._
import sbt.Keys._


object VR extends Build {

  lazy val formatSettings = SbtScalariform.scalariformSettings ++ Seq(
    ScalariformKeys.preferences in Compile := formattingPreferences,
    ScalariformKeys.preferences in Test := formattingPreferences
  )

  import scalariform.formatter.preferences._

  def formattingPreferences =
    FormattingPreferences()
      .setPreference(RewriteArrowSymbols, true)
      .setPreference(AlignParameters, true)
      .setPreference(AlignSingleLineCaseStatements, true)
      .setPreference(DoubleIndentClassDeclaration, true)
      .setPreference(PlaceScaladocAsterisksBeneathSecondAsterisk, true)

  lazy val projectSettings = Defaults.defaultSettings ++ Seq(
    version := "0.1.0",
    scalaVersion := "2.10.4",
    organization := "org.amcgala",
    connectInput := true,
    name := "VR Framework",
    offline := false,
    scalacOptions := Seq(
      "-Xfuture",
      "-deprecation",
      "-feature",
      "-Xlint",
      "-optimise",
      "-Xexperimental"
    )
  )


  lazy val root = Project(id = "vr", base = file("."), settings = projectSettings ++ formatSettings)
}



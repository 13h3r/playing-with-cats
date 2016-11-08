scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.7.2"
  ,"io.spray" %% "spray-json" % "1.3.2"

)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.2")

addCompilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full)
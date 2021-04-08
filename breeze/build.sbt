libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze" % "1.0",
  "org.scalanlp" %% "breeze-natives" % "1.0"
)

initialCommands in console :=
    """
      |import breeze.linalg._
      |import com.kogecoo.scalaad.breeze.BreezeRule.Implicits._
""".stripMargin

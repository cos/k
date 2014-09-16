import AssemblyKeys._ 

assemblySettings

organization := "org.kframework.k"

name := "k"

version := "3.5-SNAPSHOT"

scalaVersion := "2.10.4"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  "Runtime Verification" at "http://office.runtimeverification.com:8888/repository/internal",
  "Runtime Verification Snapshots" at "http://office.runtimeverification.com:8888/repository/snapshots"
)

libraryDependencies ++= Seq(
    "com.google.guava" % "guava" % "[18.0,)",
	"com.google.inject" % "guice" % "[3.0,)",
	"com.google.inject.extensions" % "guice-multibindings" % "[3.0,)",
	"net.sf.jung" % "jung-api" % "[2.0.1,)",
	"org.apache.commons" % "commons-lang3" % "[3.3.2,)",
	"org.apache.commons" % "commons-collections4" % "[4.0,)",
	"commons-io" % "commons-io" % "2.4",	
	"net.sf.jung" % "jung-visualization" % "[2.0.1,)",
	"net.sf.jung" % "jung-algorithms" % "[2.0.1,)",
	"org.kframework.dependencies" % "jcommander" % "[1.35-custom,)",
	"net.sf.jung" % "jung-graph-impl" % "[2.0.1,)",
	"org.kframework.mpfr_java" % "mpfr_java" % "1.0-SNAPSHOT",
	"org.kframework.mpfr_java" % "mpfr_java" % "1.0-SNAPSHOT", //<classifier>${native.classifier}</classifier>,
	"net.sf.jung" % "jung-io" % "[2.0.1,)",
	"org.fusesource.jansi" % "jansi" % "[1.11,)",
	"jline" % "jline" % "0.9.94",
	"org.pcollections" % "pcollections" % "[2.1.2,)",
	"com.googlecode.java-diff-utils" % "diffutils" % "[1.3.0,)",
	"uk.com.robust-it" % "cloning" % "[1.9.0,)",
	"junit" % "junit" % "[4.11,)",
	"org.mockito" % "mockito-all" % "[1.9.5,)",
	"org.kframework.dependencies" % "strategoxt" % "[1.0,)",
	"com.microsoft.z3" % "com.microsoft.z3" % "[4.3.2.5a45711f22d9,)",
	"com.microsoft.z3" % "libz3java" % "[4.3.2.5a45711f22d9,)", // <type>${native.library.type}</type><classifier>${native.classifier}</classifier>
	"com.microsoft.z3" % "libz3" % "[4.3.2.5a45711f22d9,)",  //<type>${native.library.type} </type><classifier>${native.classifier}</classifier>
	"org.kframework.dependencies" % "maude" % "[2.6,)",
	"org.kframework.dependencies" % "gappa" % "[1.0,)", // <type>${native.exe.type}</type><classifier>${native.os.classifier}</classifier>
	"org.kframework.dependencies" % "sdf2table" % "[1.0,)", //<type>${native.exe.type}</type><classifier>${native.os.classifier}</classifier>
	"org.kframework.dependencies" % "strj" % "[1.0,)", // <scope>provided</scope>
	"org.kframework.dependencies" % "implodePT" % "[1.0,)") // <type>${native.exe.type}</type><classifier>${native.os.classifier}</classifier><scope>provided</scope>

net.virtualvoid.sbt.graph.Plugin.graphSettings

unmanagedSourceDirectories in Compile += baseDirectory.value / "target" / "generated-sources" / "javacc"

unmanagedSourceDirectories in Compile += baseDirectory.value / "target" / "generated-sources" / "sdf"

unmanagedResourceDirectories in Compile += baseDirectory.value / "src" / "main" / "sdf" / "syntax"

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

classDirectory in Compile := target.value / "classes"

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case PathList(ps @ _*) if ps.contains("hawtjni-runtime") => MergeStrategy.first
    case PathList(ps @ _*) if ps.contains("junit") => MergeStrategy.first
    case PathList(ps @ _*) if ps.contains("hamcrest") => MergeStrategy.first
    case PathList(ps @ _*) if ps.contains("objenesis") => MergeStrategy.first
    case PathList(ps @ _*) if ps.contains("strategoxt") => MergeStrategy.first
    case PathList(ps @ _*) if ps.contains("run.class") => MergeStrategy.first
    case PathList(ps @ _*) if ps.contains("start.class") => MergeStrategy.first
    case x => old(x)
  }
}

mainClass := Some("org.kframework.main.Main")

lazy val releaseDirectory = settingKey[File]("An example task")

releaseDirectory := target.value / "release" / "k"

lazy val copyBin = taskKey[Unit]("Copy bin directory")

copyBin := {
	IO.copyDirectory(sourceDirectory.value / "main" / "scripts" / "bin", releaseDirectory.value / "bin")	
}

outputPath in assembly := releaseDirectory.value / "lib" / "java" / (name.value + "-" + version.value + ".jar")


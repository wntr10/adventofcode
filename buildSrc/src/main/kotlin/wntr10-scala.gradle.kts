plugins {
    scala
    `java-library`
}

dependencies {
    implementation("org.scala-lang:scala3-library_3:3.2.1")
}

tasks.withType<ScalaCompile>() {
    targetCompatibility = ""
    scalaCompileOptions.additionalParameters = listOf("-target:jvm-1.8")
}

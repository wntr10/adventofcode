plugins {
    scala
    `java-library`
}

dependencies {
    implementation("org.scala-lang:scala-library:2.12.17")
}

tasks.withType<ScalaCompile>() {
    targetCompatibility = ""
    scalaCompileOptions.additionalParameters = listOf("-target:jvm-1.8")
}

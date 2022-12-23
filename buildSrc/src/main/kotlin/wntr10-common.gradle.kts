plugins {
    id("wntr10-java")
}

repositories {
    mavenCentral()
}

tasks.test {
    useJUnitPlatform()
}
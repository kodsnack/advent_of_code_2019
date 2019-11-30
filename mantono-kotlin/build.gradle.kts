fun version(artifact: String): String {
	val key = "version.${artifact.toLowerCase()}"
	return project.ext[key]?.toString()
		?: throw IllegalStateException("No version found for artifact '$artifact'")
}

fun projectName(): String = project.name.replace("{", "").replace("}", "")

plugins {
	id("application") apply true
	id("org.jetbrains.kotlin.jvm") version "1.3.50" apply true
	id("java") apply true
	id("maven") apply true
	id("idea") apply true
}

application {
	mainClassName = "com.mantono.MainKt"
}

group = "com.mantono"
version = "0.1.0"
description = "{{project_description}}"

defaultTasks = mutableListOf("test")

repositories {
	mavenLocal()
	jcenter()
	mavenCentral()
}

dependencies {
	implementation("org.jetbrains.kotlin", "kotlin-stdlib-jdk8")
	implementation("org.jetbrains.kotlinx", "kotlinx-coroutines-jdk8")
	
	// Logging
	implementation("io.github.microutils", "kotlin-logging", "1.6.20")
	// Enable for applications
	// runtime("ch.qos.logback", "logback-classic", "1.2.3")

	// Junit
	testCompile("org.junit.jupiter", "junit-jupiter-api", version("junit"))
	testRuntime("org.junit.jupiter", "junit-jupiter-engine", version("junit"))
}

tasks {
	test {
		useJUnitPlatform()

		// Show test results.
		testLogging {
			events("passed", "skipped", "failed")
		}
		reports {
			junitXml.isEnabled = false
			html.isEnabled = true
		}
	}

	compileKotlin {
		sourceCompatibility = version("jvm")
		kotlinOptions {
			jvmTarget = version("jvm")
		}
	}


	wrapper {
		description = "Generates gradlew[.bat] scripts for faster execution"
		gradleVersion = version("gradle")
	}
}
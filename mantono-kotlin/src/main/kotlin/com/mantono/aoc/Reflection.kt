package com.mantono.aoc

import org.reflections.Reflections
import org.reflections.scanners.MethodAnnotationsScanner
import org.reflections.util.ClasspathHelper
import org.reflections.util.ConfigurationBuilder
import java.lang.reflect.Method

fun findFunction(day: Int): Method {
    val config = ConfigurationBuilder()
        .setUrls(ClasspathHelper.forPackage("com.mantono.aoc"))
        .setScanners(MethodAnnotationsScanner())

    val reflections = Reflections(config)
    return reflections.getMethodsAnnotatedWith(AoC::class.java).asSequence()
        .filter { it.getAnnotation(AoC::class.java).day == day }
        .sortedBy { it.getAnnotation(AoC::class.java).part }
        .last()
}
/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */
package ch.epfl.scala.debugadapter.sbtplugin;

import sbt.testing.AnnotatedFingerprint;

import java.io.Serializable;

public final class AnnotatedFingerscan implements AnnotatedFingerprint, Serializable {
    private final boolean isModule;
    private final String annotationName;

    public AnnotatedFingerscan(final AnnotatedFingerprint print) {
        isModule = print.isModule();
        annotationName = print.annotationName();
    }

    public boolean isModule() {
        return isModule;
    }

    public String annotationName() {
        return annotationName;
    }
}
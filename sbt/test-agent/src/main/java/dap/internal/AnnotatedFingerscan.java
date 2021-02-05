package ch.epfl.scala.debug.internal;

import sbt.testing.AnnotatedFingerprint;

import java.io.Serializable;

final class AnnotatedFingerscan implements AnnotatedFingerprint, Serializable {
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
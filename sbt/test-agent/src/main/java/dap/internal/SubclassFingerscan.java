package ch.epfl.scala.debug.internal;

import sbt.testing.SubclassFingerprint;

import java.io.Serializable;

final class SubclassFingerscan implements SubclassFingerprint, Serializable {
    private final boolean isModule;
    private final String superclassName;
    private final boolean requireNoArgConstructor;

    SubclassFingerscan(final SubclassFingerprint print) {
        isModule = print.isModule();
        superclassName = print.superclassName();
        requireNoArgConstructor = print.requireNoArgConstructor();
    }

    public boolean isModule() {
        return isModule;
    }

    public String superclassName() {
        return superclassName;
    }

    public boolean requireNoArgConstructor() {
        return requireNoArgConstructor;
    }
}
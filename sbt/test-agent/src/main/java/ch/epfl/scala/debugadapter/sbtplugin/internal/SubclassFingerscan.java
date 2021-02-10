/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package ch.epfl.scala.debugadapter.sbtplugin.internal;

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
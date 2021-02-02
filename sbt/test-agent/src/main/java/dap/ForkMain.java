/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package dap;

import sbt.testing.*;

import java.io.ObjectInputStream;
import java.io.Serializable;
import java.net.InetAddress;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public final class ForkMain {
  // main
  // ----------------------------------------------------------------------------------------------------------------

  public static void main(final String[] args) throws Exception {
    ClassLoader classLoader = new Run().getClass().getClassLoader();
    try {
      main(args, classLoader);
    } finally {
      System.exit(0);
    }
  }

  public static void main(final String[] args, ClassLoader classLoader) throws Exception {
    final Socket socket = new Socket(InetAddress.getByName(null), Integer.valueOf(args[0]));
    final ObjectInputStream is = new ObjectInputStream(socket.getInputStream());
    // os = new ObjectOutputStream(socket.getOutputStream());
    // Must flush the header that the constructor writes, otherwise the ObjectInputStream on the
    // other end may block indefinitely
    // os.flush();
    try {
      new Run().run(is, classLoader);
    } finally {
      is.close();
    }
  }

  // ----------------------------------------------------------------------------------------------------------------

  private static final class Run {

    private void run(
        final ObjectInputStream is, ClassLoader classLoader) {
      try {
        runTests(is, classLoader);
      } catch (final RunAborted e) {
        internalError(e);
      } catch (final Throwable t) {
        try {
          logError("Uncaught exception when running tests: " + t.toString());
        } catch (final Throwable t2) {
          internalError(t2);
        }
      }
    }

    private boolean matches(final Fingerprint f1, final Fingerprint f2) {
      if (f1 instanceof SubclassFingerprint && f2 instanceof SubclassFingerprint) {
        final SubclassFingerprint sf1 = (SubclassFingerprint) f1;
        final SubclassFingerprint sf2 = (SubclassFingerprint) f2;
        return sf1.isModule() == sf2.isModule()
            && sf1.superclassName().equals(sf2.superclassName());
      } else if (f1 instanceof AnnotatedFingerprint && f2 instanceof AnnotatedFingerprint) {
        final AnnotatedFingerprint af1 = (AnnotatedFingerprint) f1;
        final AnnotatedFingerprint af2 = (AnnotatedFingerprint) f2;
        return af1.isModule() == af2.isModule()
            && af1.annotationName().equals(af2.annotationName());
      }
      return false;
    }

    class RunAborted extends RuntimeException {
      RunAborted(final Exception e) {
        super(e);
      }
    }

    private void logDebug(final String message) {
      System.out.println(message);
    }

    private void logInfo(final String message) {
      System.out.println(message);
    }

    private void logWarn(final String message) {
      System.out.println(message);
    }

    private void logError(final String message) {
      System.err.println(message);
    }

    private sbt.testing.Logger remoteLogger(final boolean ansiCodesSupported) {
      return new sbt.testing.Logger() {
        public boolean ansiCodesSupported() {
          return ansiCodesSupported;
        }

        public void error(final String s) {
          logError(s);
        }

        public void warn(final String s) {
          logWarn(s);
        }

        public void info(final String s) {
          logInfo(s);
        }

        public void debug(final String s) {
          logDebug(s);
        }

        public void trace(final Throwable t) {
          logDebug(t.getMessage());
        }
      };
    }

    private ExecutorService executorService(
            final ForkConfiguration config) {
      if (config.isParallel()) {
        final int nbThreads = Runtime.getRuntime().availableProcessors();
        logDebug("Create a test executor with a thread pool of " + nbThreads + " threads.");
        // more options later...
        // TODO we might want to configure the blocking queue with size #proc
        return Executors.newFixedThreadPool(nbThreads);
      } else {
        logDebug("Create a single-thread test executor");
        return Executors.newSingleThreadExecutor();
      }
    }

    private void runTests(
        final ObjectInputStream is, ClassLoader classLoader)
        throws Exception {
      final ForkConfiguration config = (ForkConfiguration) is.readObject();
      final ExecutorService executor = executorService(config);
      final TaskDef[] tests = (TaskDef[]) is.readObject();
      final int nFrameworks = is.readInt();
      final sbt.testing.Logger[] loggers = {remoteLogger(config.isAnsiCodesSupported())};

      for (int i = 0; i < nFrameworks; i++) {
        final String[] implClassNames = (String[]) is.readObject();
        final String[] frameworkArgs = (String[]) is.readObject();
        final String[] remoteFrameworkArgs = (String[]) is.readObject();

        Framework framework = null;
        for (final String implClassName : implClassNames) {
          try {
            final Object rawFramework =
                Class.forName(implClassName).getDeclaredConstructor().newInstance();
            if (rawFramework instanceof Framework) framework = (Framework) rawFramework;
            else framework = new FrameworkWrapper((org.scalatools.testing.Framework) rawFramework);
            break;
          } catch (final ClassNotFoundException e) {
            logDebug("Framework implementation '" + implClassName + "' not present.");
          }
        }

        if (framework == null) continue;

        final LinkedHashSet<TaskDef> filteredTests = new LinkedHashSet<>();
        for (final Fingerprint testFingerprint : framework.fingerprints()) {
          for (final TaskDef test : tests) {
            // TODO: To pass in correct explicitlySpecified and selectors
            if (matches(testFingerprint, test.fingerprint()))
              filteredTests.add(
                  new TaskDef(
                      test.fullyQualifiedName(),
                      test.fingerprint(),
                      test.explicitlySpecified(),
                      test.selectors()));
          }
        }
        final Runner runner = framework.runner(frameworkArgs, remoteFrameworkArgs, classLoader);
        final sbt.testing.Task[] tasks = runner.tasks(filteredTests.toArray(new TaskDef[filteredTests.size()]));
        logDebug(
            "Runner for "
                + framework.getClass().getName()
                + " produced "
                + tasks.length
                + " initial tasks for "
                + filteredTests.size()
                + " tests.");

        Thread callDoneOnShutdown = new Thread(runner::done);
        Runtime.getRuntime().addShutdownHook(callDoneOnShutdown);

        runTestTasks(executor, tasks, loggers);

        runner.done();

        Runtime.getRuntime().removeShutdownHook(callDoneOnShutdown);
      }
      is.readObject();
    }

    private void runTestTasks(
        final ExecutorService executor,
        final sbt.testing.Task[] tasks,
        final sbt.testing.Logger[] loggers) {
      if (tasks.length > 0) {
        final List<Future<sbt.testing.Task[]>> futureNestedTasks = new ArrayList<>();
        for (final sbt.testing.Task task : tasks) {
          futureNestedTasks.add(runTest(executor, task, loggers));
        }

        // Note: this could be optimized further, we could have a callback once a test finishes that
        // executes immediately the nested tasks
        //       At the moment, I'm especially interested in JUnit, which doesn't have nested tasks.
        final List<sbt.testing.Task> nestedTasks = new ArrayList<>();
        for (final Future<sbt.testing.Task[]> futureNestedTask : futureNestedTasks) {
          try {
            nestedTasks.addAll(Arrays.asList(futureNestedTask.get()));
          } catch (final Exception e) {
            logError("Failed to execute task " + futureNestedTask);
          }
        }
        runTestTasks(executor, nestedTasks.toArray(new sbt.testing.Task[nestedTasks.size()]), loggers);
      }
    }

    private Future<sbt.testing.Task[]> runTest(
        final ExecutorService executor,
        final sbt.testing.Task task,
        final sbt.testing.Logger[] loggers) {
      return executor.submit(
          () -> {
            sbt.testing.Task[] nestedTasks;
            final TaskDef taskDef = task.taskDef();
            try {
              final EventHandler handler =
                  new EventHandler() {
                    public void handle(final Event e) { }
                  };
              logDebug("  Running " + taskDef);
              nestedTasks = task.execute(handler, loggers);
            } catch (final Throwable t) {
              nestedTasks = new sbt.testing.Task[0];
            }
            return nestedTasks;
          });
    }

    private void internalError(final Throwable t) {
      System.err.println("Internal error when running tests: " + t.toString());
    }
  }
}

val args = CommandLine.arguments();
val arg = List.hd args;
CM.make "../sources1.cm";

Sample.parse(arg);

val _ = OS.Process.exit(OS.Process.success);
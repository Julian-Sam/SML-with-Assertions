val args = CommandLine.arguments();
val arg = List.hd args;
CM.make "../sources.cm";

Sample.parse(arg);

val _ = OS.Process.exit(OS.Process.success);
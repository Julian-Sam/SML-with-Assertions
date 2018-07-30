val args = CommandLine.arguments();
val arg = List.hd args;
CM.make "../sources.cm";

Sample.parse_print(arg);

val _ = OS.Process.exit(OS.Process.success);
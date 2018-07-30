val args = CommandLine.arguments();
val arg = List.hd args;
CM.make "../sources.cm";


val _ = OS.Process.exit(OS.Process.success);
val args = CommandLine.arguments();
CM.make (List.nth(args,1));
AssertEngine.parse(List.nth(args,0));
val _ = OS.Process.exit(OS.Process.success);
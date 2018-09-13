val args = CommandLine.arguments();
val _ = print (List.nth(args,0));
CM.make (List.nth(args,0));
val _ = OS.Process.exit(OS.Process.success);
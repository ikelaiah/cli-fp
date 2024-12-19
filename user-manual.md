## Error Messages and Invalid Usage

When using the CLI framework, you might encounter various error messages that help you understand what went wrong and how to fix it. Here are the common scenarios:

### Invalid Command Usage

1. **Using an Unknown Command**
   ```bash
   $ myapp invalidcommand
   Error: Unknown command "invalidcommand"
   Usage: myapp <command> [options]

   Commands:
     repo              Repository management
     config            Configuration management

   Use --help for more information.
   ```

2. **Using an Unknown Subcommand**
   ```bash
   $ myapp repo invalid
   Error: Unknown subcommand "invalid" for repo

   Available subcommands:
     init              Initialize a new repository
     clone             Clone an existing repository

   Use "myapp repo --help" for more information.
   ```

3. **Using Invalid Flags**
   ```bash
   $ myapp repo clone -z
   Error: Unknown parameter "-z"
   Usage: myapp repo clone [options]

   Clone a repository

   Options:
     -u, --url               Repository URL (required)
     -p, --path              Target path
   ```

4. **Missing Required Parameters**
   ```bash
   $ myapp repo clone
   Error: Required parameter "--url" not provided
   Usage: myapp repo clone [options]

   Clone a repository

   Options:
     -u, --url               Repository URL (required)
     -p, --path              Target path
   ```

### Getting Help

- If you see an error message, the framework will automatically show relevant help information
- You can always use `-h` or `--help` with any command or subcommand to see detailed usage information
- Use `--help-complete` to see the complete reference for all commands and options

### Best Practices

1. **Check Command Syntax**
   - Always verify the correct command name and structure
   - Use tab completion if available in your shell

2. **Verify Parameters**
   - Make sure all required parameters are provided
   - Check that parameter values are in the correct format

3. **Use Help When Needed**
   - When in doubt, append `--help` to see command-specific help
   - Use `--help-complete` to explore all available commands 
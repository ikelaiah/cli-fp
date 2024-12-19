## Error Handling

### Invalid Commands and Flags

The framework implements robust error handling for invalid commands, subcommands, and flags:

1. **Unknown Commands**
   - When a non-existent command is provided, the framework displays an error message and shows a brief help with available commands
   - Example:
     ```
     Error: Unknown command "invalid"
     Usage: myapp <command> [options]

     Commands:
       repo              Repository management
       config            Configuration management
     ```

2. **Unknown Subcommands**
   - When an invalid subcommand is provided, the framework:
     - Shows an error message
     - Lists available subcommands for the parent command
     - Provides guidance on how to get help
   - Example:
     ```
     Error: Unknown subcommand "invalid" for repo

     Available subcommands:
       init              Initialize a new repository
       clone             Clone an existing repository

     Use "myapp repo --help" for more information.
     ```

3. **Invalid Flags**
   - The framework validates all provided flags against the command's defined parameters
   - Unknown flags trigger an error and display the command's help
   - Example:
     ```
     Error: Unknown parameter "-z"
     Usage: myapp repo clone [options]

     Clone a repository

     Options:
       -u, --url               Repository URL (required)
       -p, --path              Target path
     ```

4. **Required Parameters**
   - The framework checks for required parameters
   - Missing required parameters trigger an error with helpful guidance
   - Example:
     ```
     Error: Required parameter "--url" not provided
     Usage: myapp repo clone [options]
     ```

### Error Handling Flow

1. Command Validation
   ```mermaid
   flowchart TD
     A[Start] --> B{Valid Command?}
     B -->|No| C[Show Error & Brief Help]
     B -->|Yes| D{Valid Subcommand?}
     D -->|No| E[Show Error & Available Subcommands]
     D -->|Yes| F{Valid Flags?}
     F -->|No| G[Show Error & Command Help]
     F -->|Yes| H{Required Params?}
     H -->|Missing| I[Show Error & Command Help]
     H -->|All Present| J[Execute Command]
   ```

This error handling system ensures that users:
- Get immediate feedback when something is wrong
- See relevant help information for their current context
- Understand what valid options are available
- Know how to get more detailed help when needed 
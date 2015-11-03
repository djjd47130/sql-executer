# Introduction #

SQL Executer is a tool to execute large SQL Server script files which contain `GO` statements. Since ADO does not support the `GO` statement (because it's specific to Microsoft tools), this tool was started to provide a simple light-weight solution to do so within a Delphi project. It's especially useful when deploying mass updates to a SQL Server database.

**NOTE** - This tool is not yet considered stable, although it does essentially work

# Details #

SQL Executer can be installed into your IDE as a component. This makes it easier to work with existing database components, specifically the `TADOConnection` which needs to be assigned to the `Connection` property.

To use this tool:
  1. Assign a connected `TADOConnection` component to the `Connection` property
  1. Load SQL script into the `SQL` property
  1. Run SQL script by calling the `Execute` function


# Notes #

  1. Cannot handle `GO` statement inside of a comment block
  1. "GO" statements must be alone on their own line with nothing else
  1. Script is not case sensitive, `GO` may be `go`
  1. Since MS tools support changing `GO` to your own custom keyword, this tool also supports it via the `SplitWord` property


# To Do #

  1. Implement `PRINT` statements (not ones which may be in a Stored Procedure)
  1. Implement `USE` statements to switch the database
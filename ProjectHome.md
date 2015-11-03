# SQL Script Executer #
## Use to execute large SQL scripts which contain `GO` statements using ADO. ##

This component can be installed into the Delphi IDE.

**Main Component**:
  * `TSQLExec`

**To Use**:

  1. Assign `TADOConnection` to the `Connection` property
  1. Load SQL script to the `SQL` property (for example `SQL.LoadFromFile(Filename)`)
  1. Execute SQL script by calling `Execute` function

**Features**:

  * Split a large SQL script file into individual blocks to be executed
  * Change keyword `GO` to a different custom keyword (as supported by MS tools)
  * Use transaction mode to be able to rollback changes on script errors
  * Iterate through each parsed SQL script block
  * Monitor current position to be able to implement a progress bar
  * Caching mechanism to only parse when it needs to (via Invalidation)
  * Custom exception handlers to catch specific exceptions and related data

**To Do**:

  * Detect `PRINT` statements to trigger `OnPrint` event
  * Detect `GO` statements inside of a comment block (`/* GO */`)
  * Detect `USE` statements to switch current database

**Sample Usage**:

```
procedure TForm1.Button1Click(Sender: TObject);
begin
  SQLExec1.SQL.LoadFromFile('C:\MyScriptFile.sql');
  SQLExec1.Connection:= ADOConnection1;
  SQLExec1.Execute;
end;
```
# SQL Executer
### by Jerry Dodge

_This code uses the Mozilla Public License 1.1_

Use the SQL Executer component to run a large SQL script file which contains "GO" statements using ADO.

**Main Unit**: `SQLExec.pas`

**Main Component**:
 `TSQLExec`

This component can be installed into the Delphi IDE by adding it to a package and registering it.

**How To Use**:

1. Assign `TADOConnection` to the `Connection` property
  
2. Load SQL script to the `SQL` property (for example `SQL.LoadFromFile(Filename)`)
  
3. Execute SQL script by calling `Execute` function

**Features**:

* Split a large SQL script file into individual blocks to be executed
  
* Change keyword `GO` to a different custom keyword (as supported by MS tools)
  
* Use transaction mode to be able to rollback changes on script errors
  
* Iterate through each parsed SQL script block
  
* Monitor current position to be able to implement a progress bar
  
* Caching mechanism to only parse when it needs to (via Invalidation)
  
* Custom exception handlers to catch specific exceptions and related data

**Sample Usage**:

```
procedure TForm1.Button1Click(Sender: TObject);
begin
  SQLExec1.SQL.LoadFromFile('C:\MyScriptFile.sql');
  SQLExec1.Connection:= ADOConnection1;
  SQLExec1.Execute;
end;
```

Package also includes fully featured application to demonstrate the usage of this component.

1. Requires ChromeTabs library (Not Included)
  - https://github.com/norgepaul/TChromeTabs
2. Requires SynEdit library (Not Included)
  - https://github.com/SynEdit/SynEdit

**Application Features**:

1. Execute single script on multiple databases at once
2. Error reporting with exact script which failed
3. Connect to multiple servers at once
4. Full syntax highlighting with editing capabilities
5. Supports Open With and Recent SQL files (Jump List)



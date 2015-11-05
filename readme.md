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

**Sample Application**

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

**Background**

This project originally started about a year ago when I was tasked with enhancing how our customers' SQL Server Database gets updated. We have a script file over 38,000 lines of SQL text, and over 2,000 "GO" statements. ADO does not support "GO", it's specific to MS tools. But it does have an advantage - anywhere a GO statement appears, it makes sure the previous code finishes executing before moving onto the next segment.

So, I wrote this utility to parse out the SQL Script and split it at each "GO" statement, and run each segment one by one. This was then used in our server update application, but I cannot share that application here. Therefore, I have just written this sample application in the past few days to demonstrate its usage. This sample application then quickly took off and I'm using it already for production.

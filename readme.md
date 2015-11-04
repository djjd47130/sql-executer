# SQL Executer
### by Jerry Dodge

_This code uses the Mozilla Public License 1.1_

Main Unit: SQLExec.pas

Use the SQL Executer component to run a large SQL script file which contains "GO" statements using ADO.

This component can be installed into the Delphi IDE by adding it to a package and registering it.

Package also includes fully featured application to demonstrate the usage of this component.

1. Requires ChromeTabs library (Not Included)
  - https://github.com/norgepaul/TChromeTabs
2. Requires SynEdit library (Not Included)
  - https://github.com/SynEdit/SynEdit

Instructions:

1. Assign TADOConnection to the Connection property
2. Load SQL script to the SQL property
3. Execute SQL script by calling Execute function

Features:

1. Change the "GO" keyword to any other keyword of your choice instead of just "GO"
2. Iterate through all SQL blocks before executing them
3. Report and handle script errors more effectively (Each block is executed on its own)
4. Code is self-documented to help you understand how it works

Known Issues:

1. Cannot handle "GO" statement inside of a comment block
    - This has been fixed, introducing support of comment blocks
    - Comment blocks are expected to be initiated on a line of its own
2. "GO" statements must be alone on their own line with nothing else
3. Cannot handle "PRINT" statement because it clashes with that inside of stored procedures
4. Unclear how to handle "USE" statements
5. Occasional access violation if one block has a script error


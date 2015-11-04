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

SQL Executer
by Jerry Dodge

This code uses the Mozilla Public License 1.1

Use the SQL Executer component to run a large SQL script file which contains "GO" statements.

This component can be installed into the Delphi IDE by adding it to a package and registering it.


Instructions:
1 - Assign TADOConnection to the Connection property
2 - Load SQL script to the SQL property
3 - Execute SQL script by calling Execute function


Known Issues:
1 - Cannot handle "GO" statement inside of a comment block
    - This has been fixed, introducing support of comment blocks
2 - "GO" statements must be alone on their own line with nothing else
3 - Cannot handle "PRINT" statement because it clashes with that inside of stored procedures
4 - Unclear how to handle "USE" statements
5 - Occasional access violation if one block has a script error


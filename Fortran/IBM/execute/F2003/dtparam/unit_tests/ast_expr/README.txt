****** DTP unit tests using AST *********
#The following sequence of test cases are basic test cases
001 -expressions with addition in derived type definition
002 -expressions with subtraction in derived type defintion
003 -expressions with multiplication in derived type definition
004 -expressions with division in derived type definition
005 -expression with exponentiation in derived type definition
006 -large expressions in derived type defintion
007 -intialized derived type parameters and expressions
008 -expressions in derived type definition. write/read to module file
009 -expressions in derived type definition with multiple read/write's to module file
010 -expressions in 1D-array bounds in derived type defintions
011 -expressions in multiple dimension array bounds in derived type defintions
012 -s/a
013 -expressions in derived type definition with extends keyword
014 -s/a
015 -expressions in derived type definition with extends keyword and array bounds 
016 -expressions with unary operator + in derived type definition -FAILS 323551
017 -expressions with unary operator - in derived type definition

#The next sequence of testcases, test different types of components within the derived type definition 
018 -real component
018a - w/ module file
019 -complex component
019a - w/ module file
020 -logical component
020a - w/ module file
021 -character component
021a - w/ module file
022 -derived type component
022a - w/ module file
023 -derived type components
023a - w/ module file FAILS 323604
024 -derived type components w/ read/write from module file
024a - w/ module file FALS 323604

#The next sequence on test cases test with initalization expressions
025 -integer components and initialization expressions
025a - w/ module file
026 -s/a
026a - w/ module file
027 -real components and initalization expressions FAILS 323718
027a - w/ module file FAILS 323718
028 -complex components and intialization expressions FAILS 323758
028a - w/ module file FAILS 323758
028b - w/ module file FAILS 323758
029 -character components and intialization expressions FAILS 323718
029a - w/ module file FAILS 323718
030 -derived types and intialization expressions.FAILS 324074
030a - w/ module file FAILS 324074
031 -allocatable/pointer component in derived type FAILS 323758	
031a - w/ module file FAILS 323758

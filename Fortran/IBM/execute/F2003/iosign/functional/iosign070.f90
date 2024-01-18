!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Alberto Alvarez-Mesquida
!*  DATE                       : 02/20/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing SIGN specifier with the OPEN, READ and
!*  WRITE stmts for files with sequential access, list-directed input/output,
!*  using G edit descriptor in FORMAT stmt. Testing complex variables.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program iosign070

  complex :: q
  complex(4) :: q4
  complex(8) :: q8
  complex(16) :: q16

  write (*,*) "\nTest 1\n"

  open (1,file="iosign070.in",access='sequential',sign='suppress')
  read (1,*) q,q4,q8,q16
  close(1)

10 format(4(G10.4,X),2(G12.5,X),2(G14.6,X))

  write (*,10,sign='processor_defined') q,q4,q8,q16

  write (*,*) "\nTest 2\n"

  open (1,file="iosign070.in",access='sequential',sign='processor_defined')
  read (1,*) q,q4,q8,q16
  close(1)

  write (*,10,sign='plus') q,q4,q8,q16

  write (*,*) "\nTest 3\n"

  open (1,file="iosign070.in",access='sequential',sign='plus')
  read (1,*) q,q4,q8,q16
  close(1)

  write (*,10,sign='suppress') q,q4,q8,q16

end program iosign070


!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing SIGN specifier with the OPEN, READ and
!*  WRITE stmts for internal files, printing using E edit descriptor.
!*  Testing complex variables.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program iosign091

  complex :: q
  complex(4) :: q4
  complex(8) :: q8
  complex(16) :: q16
  character(120) :: str

  write (*,*) "\nTest 1\n"

  open (1,file="iosign091.in",access='sequential',sign='suppress')
  read (1,*) q,q4,q8,q16
  close(1)

10 format(4(E10.4,X),2(E12.5,X),2(E14.6,X))

  write (str,10,sign='processor_defined') q,q4,q8,q16
  write (*,*) str

  write (*,*) "\nTest 2\n"

  open (1,file="iosign091.in",access='sequential',sign='processor_defined')
  read (1,*) q,q4,q8,q16
  close(1)

  write (str,10,sign='plus') q,q4,q8,q16
  write (*,*) str

  write (*,*) "\nTest 3\n"

  open (1,file="iosign091.in",access='sequential',sign='plus')
  read (1,*) q,q4,q8,q16
  close(1)

  write (str,10,sign='suppress') q,q4,q8,q16
  write (*,*) str

end program iosign091

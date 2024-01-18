!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing SIGN specifier with the OPEN, READ and
!*  WRITE stmts for files with stream access, list-directed input/output,
!*  using F edit descriptor in FORMAT stmt. Testing complex variables.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program iosign067

  complex :: q
  complex(4) :: q4
  complex(8) :: q8
  complex(16) :: q16

  write (*,*) "\nTest 1\n"

5 format(8(F5.2))

  open (1,file="iosign067.in",form='formatted',access='stream',sign='suppress')
  read (1,5) q,q4,q8,q16
  close(1)

10 format(4(F5.2,X),2(F6.3,X),2(F7.4,X))

  write (*,10,sign='processor_defined') q,q4,q8,q16

  write (*,*) "\nTest 2\n"

  open (1,file="iosign067.in",form='formatted',access='stream', &
        sign='processor_defined')
  read (1,5) q,q4,q8,q16
  close(1)

  write (*,10,sign='plus') q,q4,q8,q16

  write (*,*) "\nTest 3\n"

  open (1,file="iosign067.in",form='formatted',access='stream',sign='plus')
  read (1,5) q,q4,q8,q16
  close(1)

  write (*,10,sign='suppress') q,q4,q8,q16

end program iosign067


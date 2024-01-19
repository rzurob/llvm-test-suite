!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing SIGN specifier with the OPEN, READ and
!*  WRITE stmts for files with sequential access, list-directed input/output.
!*  Testing integer variables.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program iosign058

  integer(2) :: i2
  integer :: i
  integer(4) :: i4
  integer(8) :: i8

  write (*,*) "\nTest 1\n"

  open (1,file="iosign058.in",access='sequential',sign='suppress')
  read (1,*) i2,i,i4,i8
  close(1)

  write (*,*,sign='suppress') i2,i,i4,i8

  write (*,*) "\nTest 2\n"

  open (1,file="iosign058.in",access='sequential',sign='processor_defined')
  read (1,*) i2,i,i4,i8
  close(1)

  write (*,*,sign='plus') i2,i,i4,i8

  write (*,*) "\nTest 3\n"

  open (1,file="iosign058.in",access='sequential',sign='plus')
  read (1,*) i2,i,i4,i8
  close(1)

  write (*,*,sign='processor_defined') i2,i,i4,i8

end program iosign058


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
!*  WRITE stmts for internal files. Testing integer variables.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program iosign086

  integer(2) :: i2
  integer :: i
  integer(4) :: i4
  integer(8) :: i8
  character(20) :: str

  write (*,*) "\nTest 1\n"

  open (1,file="iosign086.in",access='sequential',sign='suppress')
  read (1,*) i2,i,i4,i8
  close(1)

  write (str,*,sign='plus') i2,i,i4,i8
  write (*,*) str 

  write (*,*) "\nTest 2\n"

  open (1,file="iosign086.in",access='sequential',sign='processor_defined')
  read (1,*) i2,i,i4,i8
  close(1)

  write (str,*,sign='plus') i2,i,i4,i8
  write (*,*) str 

  write (*,*) "\nTest 3\n"

  open (1,file="iosign086.in",access='sequential',sign='plus')
  read (1,*) i2,i,i4,i8
  close(1)

  write (str,*,sign='suppress') i2,i,i4,i8
  write (*,*) str 

end program iosign086


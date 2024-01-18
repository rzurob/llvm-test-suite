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
!*  WRITE stmts for files with sequential access, namelist input/output.
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

program iosign072

  integer(2) :: i2
  integer :: i
  integer(4) :: i4
  integer(8) :: i8
  namelist /myvals/ i2,i,i4,i8

  write (*,*) "\nTest 1\n"

  open (1,file="iosign072.in",access='sequential',sign='suppress')
  read (1,nml=myvals)
  close(1)

  write (*,nml=myvals,sign='suppress')

  write (*,*) "\nTest 2\n"

  open (1,file="iosign072.in",access='sequential',sign='processor_defined')
  read (1,nml=myvals)
  close(1)

  write (*,nml=myvals,sign='plus')

  write (*,*) "\nTest 3\n"

  open (1,file="iosign072.in",access='sequential',sign='plus')
  read (1,nml=myvals)
  close(1)

  write (*,nml=myvals,sign='processor_defined')

end program iosign072


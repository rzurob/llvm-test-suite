!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing SIGN specifier with the OPEN, READ and
!*  WRITE stmts for files with stream access, namelist input/output.
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

program iosign073

  integer(2) :: i2
  integer :: i
  integer(4) :: i4
  integer(8) :: i8
  namelist /myvals/ i2,i,i4,i8

  write (*,*) "\nTest 1\n"

  open (1,file="iosign073.in",form='formatted',access='stream',sign='suppress')
  read (1,nml=myvals)
  close(1)

  write (*,nml=myvals,sign='suppress')

  write (*,*) "\nTest 2\n"

  open (1,file="iosign073.in",form='formatted',access='stream', &
        sign='processor_defined')
  read (1,nml=myvals)
  close(1)

  write (*,nml=myvals,sign='plus')

  write (*,*) "\nTest 3\n"

  open (1,file="iosign073.in",form='formatted',access='stream',sign='plus')
  read (1,nml=myvals)
  close(1)

  write (*,nml=myvals,sign='processor_defined')

end program iosign073

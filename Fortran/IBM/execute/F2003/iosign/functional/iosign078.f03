!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing SIGN specifier with the OPEN, READ and
!*  WRITE stmts for files with sequential access, namelist input/output,
!*  using G edit descriptor in FORMAT stmt. Testing real variables.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program iosign078

  real :: r
  real(4) :: r4
  real(8) :: r8
  real(16) :: r16
  namelist /myvals/ r,r4,r8,r16

  write (*,*) "\nTest 1\n"

  open (1,file="iosign078.in",access='sequential',sign='suppress')
  read (1,nml=myvals)
  close(1)

  write (*,nml=myvals,sign='processor_defined')

  write (*,*) "\nTest 2\n"

  open (1,file="iosign078.in",access='sequential',sign='processor_defined')
  read (1,nml=myvals)
  close(1)

  write (*,nml=myvals,sign='plus')

  write (*,*) "\nTest 3\n"

  open (1,file="iosign078.in",access='sequential',sign='plus')
  read (1,nml=myvals)
  close(1)

  write (*,nml=myvals,sign='suppress')

end program iosign078

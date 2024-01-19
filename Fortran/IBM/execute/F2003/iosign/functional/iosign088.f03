!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing SIGN specifier with the OPEN, READ and
!*  WRITE stmts for internal files, printing using E edit descriptor.
!*  Testing real variables.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program iosign088

  real :: r
  real(4) :: r4
  real(8) :: r8
  real(16) :: r16
  character(60) :: str

  write (*,*) "\nTest 1\n"

5 format(4(E10.4))

  open (1,file="iosign088.in",access='sequential',sign='suppress')
  read (1,5) r,r4,r8,r16
  close(1)

10 format(2(E10.4,X),E12.5,X,E14.6)

  write (str,10,sign='processor_defined') r,r4,r8,r16
  write (*,*) str

  write (*,*) "\nTest 2\n"

  open (1,file="iosign088.in",access='sequential',sign='processor_defined')
  read (1,5) r,r4,r8,r16
  close(1)

  write (str,10,sign='plus') r,r4,r8,r16
  write (*,*) str

  write (*,*) "\nTest 3\n"

  open (1,file="iosign088.in",access='sequential',sign='plus')
  read (1,5) r,r4,r8,r16
  close(1)

  write (str,10,sign='suppress') r,r4,r8,r16
  write (*,*) str

end program iosign088


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
!*  WRITE stmts for files with stream access, namelist input/output,
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

program iosign079

  real :: r
  real(4) :: r4
  real(8) :: r8
  real(16) :: r16
  namelist /myvals/ r,r4,r8,r16

  write (*,*) "\nTest 1\n"

5 format(4(E10.4))

  open (1,file="iosign079.in",access='stream',form='formatted',sign='suppress')
  read (1,nml=myvals)
  close(1)

  write (*,nml=myvals,sign='processor_defined')

  write (*,*) "\nTest 2\n"

  open (1,file="iosign079.in",access='stream',form='formatted',&
          sign='processor_defined')
  read (1,nml=myvals)
  close(1)

  write (*,nml=myvals,sign='plus')

  write (*,*) "\nTest 3\n"

  open (1,file="iosign079.in",access='stream',form='formatted',sign='plus')
  read (1,nml=myvals)
  close(1)

  write (*,nml=myvals,sign='suppress')

end program iosign079


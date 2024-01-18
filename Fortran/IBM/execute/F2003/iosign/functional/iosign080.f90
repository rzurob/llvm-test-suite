!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing SIGN specifier with the OPEN, READ and
!*  WRITE stmts for files with sequential access, namelist input/output,
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

program iosign080

  complex :: q
  complex(4) :: q4
  complex(8) :: q8
  complex(16) :: q16
  namelist /myvals/ q,q4,q8,q16

  write (*,*) "\nTest 1\n"

  open (1,file="iosign080.in",access='sequential',sign='suppress')
  read (1,nml=myvals)
  close(1)

  write (*,nml=myvals,sign='processor_defined')

  write (*,*) "\nTest 2\n"

  open (1,file="iosign080.in",access='sequential',sign='processor_defined')
  read (1,nml=myvals)
  close(1)

  write (*,nml=myvals,sign='plus')

  write (*,*) "\nTest 3\n"

  open (1,file="iosign080.in",access='sequential',sign='plus')
  read (1,nml=myvals)
  close(1)

  write (*,nml=myvals,sign='suppress')

end program iosign080


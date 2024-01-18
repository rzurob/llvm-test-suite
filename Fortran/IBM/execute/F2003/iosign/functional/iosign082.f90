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
!*  WRITE stmts for files with sequential access, namelist input/output,
!*  using E edit descriptor in FORMAT stmt. Testing complex variables.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program iosign082

  complex :: q
  complex(4) :: q4
  complex(8) :: q8
  complex(16) :: q16
  namelist /myvals/ q,q4,q8,q16

  write (*,*) "\nTest 1\n"

  open (1,file="iosign082.in",access='sequential',sign='suppress')
  read (1,nml=myvals)
  close(1)

  write (*,nml=myvals,sign='processor_defined')

  write (*,*) "\nTest 2\n"

  open (1,file="iosign082.in",access='sequential',sign='processor_defined')
  read (1,nml=myvals)
  close(1)

  write (*,nml=myvals,sign='plus')

  write (*,*) "\nTest 3\n"

  open (1,file="iosign082.in",access='sequential',sign='plus')
  read (1,nml=myvals)
  close(1)

  write (*,nml=myvals,sign='suppress')

end program iosign082


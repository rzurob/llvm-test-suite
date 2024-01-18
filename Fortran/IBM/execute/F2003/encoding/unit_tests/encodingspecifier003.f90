!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : encodingspecifier003
!*
!*  PROGRAMMER                 : Thomas Tewoldemedhin
!*  DATE                       : April 28, 2007
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ENCODING= specifier in I/O statements
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : diagnostic testing of the ENCODING= specifier
!*                               in I/O statements at run time with 
!*                               unformatted I/O
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  real :: num
  
  character(20) :: form_mode = 'unformatted'  
  
  !ENCODING= specifier not allowed in unformatted I/O
  
  open(UNIT=2,FILE='real4.dat', FORM=form_mode, ENCODING='default')
  
  close(2)

  open(UNIT=2,FILE='real4.dat', ENCODING='default', FORM = form_mode) 

end program

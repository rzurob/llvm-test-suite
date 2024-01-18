!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: roundspecifier004.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : roundspecifier004
!*
!*  PROGRAMMER                 : Michael Selvanayagam
!*  DATE                       : Dec. 20, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND= specifier in I/O statements
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : diagnostic testing of the ROUND= specifier
!*                               in I/O statements at run time with 
!*                               unformatted I/O
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  real :: num
  
  character(20) :: form_mode = 'unformatted'  
  character(20) :: format_descrip = '(f7.4 , rc )' 
  
  !ROUND= specifier not allowed in unformatted I/O
  
  open(UNIT=2,FILE='real4.dat', FORM=form_mode, ROUND='down')
  
  read(UNIT=2, FMT='(f7.4)',ROUND='nearest') num
  	
  write(2,FMT='(f7.4)',ROUND='zero') num
  
  close(2)
  
  
  open(UNIT=2,FILE='real4.dat', ROUND='down', FORM = form_mode) 
  
  read(2,format_descrip) num
  
  write(2,format_descrip) num
  
  print format_descrip, num

end program


 
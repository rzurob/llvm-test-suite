!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : encodingspecifier009
!*
!*  PROGRAMMER                 : Thomas Tewoldemedhin
!*  DATE                       : April 30, 2007
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ENCODING= specifier in I/O statements
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : functional testing of ENCODING= specifier in 
!*                               INQUIRE statement and OPEN statement
!*                               with specifers set at compile and run time with
!*                               external files
!*
!234567890093456789009345678900934567890093456789009345678900934567890

  character(20) :: which_mode, encoding_spec
  
  open(UNIT=2,FILE='encodingspecifier.dat', ENCODING='DEFAULT')
  inquire(2, ENCODING=which_mode)
  if(which_mode .ne. 'DEFAULT') error stop 1
  close(2)
  
  open(UNIT=2,FILE='encodingspecifier.dat', ENCODING='DEFAULT') 
  inquire(2, ENCODING=which_mode)
  if(which_mode .ne. 'DEFAULT') error stop 2
  close(2)
 
  open(UNIT=2,FILE='encodingspecifier.dat')
  inquire(2, ENCODING=which_mode)
  if(which_mode .ne. 'DEFAULT') error stop 3
  close(2)
  
  open(UNIT=2,FILE='encodingspecifier.dat', form='unformatted')
  inquire(2, ENCODING=which_mode)
  if(which_mode .ne. 'UNDEFINED') error stop 4
  close(2)
  
  encoding_spec='DEFAULT'
  open(UNIT=2,FILE='encodingspecifier.dat', ENCODING=encoding_spec)
  inquire(2, ENCODING=which_mode)
  if(which_mode .ne. 'DEFAULT') error stop 5
  close(2)
  
  encoding_spec='DEFAULT'
  open(UNIT=2,FILE='encodingspecifier.dat', ENCODING=encoding_spec)  
  inquire(2, ENCODING=which_mode)
  if(which_mode .ne. 'DEFAULT') error stop 6
  close(2)
  
  inquire(2, ENCODING=which_mode)
  if(which_mode .ne. 'UNKNOWN') error stop 7

end

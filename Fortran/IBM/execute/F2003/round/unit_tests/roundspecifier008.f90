!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: roundspecifier008.f
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
!*  TEST CASE TITLE            : roundspecifier008
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
!*  DESCRIPTION                : diagnostic testing of ROUND= specifier
!*                               with IOSTAT values.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
  real :: num
  integer :: iostat_num
  
  character(20) :: form_mode = 'unformatted'
  character(20) :: round_mode= 'processor_defined'
  character(20) :: wrong_round_mode='updownzero'
  character(20) :: fmt_mode='(f7.4)'
  
  open(UNIT=2, FILE='real4.dat', FORM=form_mode, ROUND=round_mode, IOSTAT=iostat_num)
  if(iostat_num .ne. 222) error stop 1
  
  open(UNIT=2, FILE='real4.dat', ROUND=wrong_round_mode, IOSTAT=iostat_num)  
  if(iostat_num .ne. 223) error stop 2
  
  open(UNIT=2, FILE='real4.dat', ROUND=round_mode, IOSTAT=iostat_num) 
  read(UNIT=2, FMT=fmt_mode,ROUND=wrong_round_mode, IOSTAT=iostat_num) num
  if(iostat_num .ne. 223) error stop 3
  	
  write(2,FMT=fmt_mode,ROUND=wrong_round_mode, IOSTAT=iostat_num) num
  if(iostat_num .ne. 223) error stop 4

end program

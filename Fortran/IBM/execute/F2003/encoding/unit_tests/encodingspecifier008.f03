!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : April 30, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : ENCODING= specifier in I/O statements
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : diagnostic testing of ENCODING= specifier
!*                               with IOSTAT values.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
  real :: num
  integer :: iostat_num

  character(20) :: form_mode = 'unformatted'
  character(20) :: encoding_mode= 'default'
  character(20) :: wrong_encoding_mode='utf-16'
  character(20) :: fmt_mode='(f7.4)'

  open(UNIT=2, FILE='real4.dat', FORM=form_mode, ENCODING=encoding_mode, IOSTAT=iostat_num)
  if(iostat_num .ne. 235) error stop 1

  open(UNIT=2, FILE='real4.dat', ENCODING=wrong_encoding_mode, IOSTAT=iostat_num)
  if(iostat_num .ne. 236) error stop 2

  open(UNIT=2, FILE='real4.dat', ENCODING=encoding_mode, IOSTAT=iostat_num)
  if(iostat_num .ne. 0) error stop 3

end program

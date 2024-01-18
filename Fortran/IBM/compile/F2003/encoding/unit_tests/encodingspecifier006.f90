!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : April 29, 2007
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ENCODING= specifier in I/O statements
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : diagnostic testing of incorrect use of
!*                               ENCODING= specifier in I/O statements at
!*                               compile time
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  real :: num

  character(20) :: encoding_mode = 'default'

  !incorrect character expression in the ENCODING= specifier
  !in open statement

  open (UNIT=2, FILE='real4.dat', ENCODING='updown') !incorrect specifier value

  open (UNIT=2, FILE='real4.dat', ENCODING=encoding_mode)

  read (UNIT=2,FMT='(f7.4)', ENCODING='UTF-8') num !encoding used in wrong stmt

  write (UNIT=2, FMT='(f7.4)', ENCODING='default') num ! encoding used in wrong stmt

  wait(2,ENCODING='default')!encoding used in wrong stmt

  backspace(2,ENCODING='default')!encoding used in wrong stmt

  flush(2,ENCODING='default')

  close(2,ENCODING='default')


end program

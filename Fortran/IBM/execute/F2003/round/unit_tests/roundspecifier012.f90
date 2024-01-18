!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 20, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND= specifier in I/O statements
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : functional testing of ROUND= specifier in
!*                               INQUIRE statement and OPEN statement
!*                               with specifers set at compile and run time with
!*                               external files
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  character(20) :: which_mode, round_spec

  open(UNIT=2,FILE='roundspecifier.dat', ROUND='UP')
  inquire(2, ROUND=which_mode)
  if(which_mode .ne. 'UP') error stop 1
  close(2)

  open(UNIT=2,FILE='roundspecifier.dat', ROUND='DOWN')
  inquire(2, ROUND=which_mode)
  if(which_mode .ne. 'DOWN') error stop 2
  close(2)

  open(UNIT=2,FILE='roundspecifier.dat', ROUND='ZERO')
  inquire(2, ROUND=which_mode)
  if(which_mode .ne. 'ZERO') error stop 3
  close(2)

  open(UNIT=2,FILE='roundspecifier.dat', ROUND='NEAREST')
  inquire(2, ROUND=which_mode)
  if(which_mode .ne. 'NEAREST') error stop 4
  close(2)

  open(UNIT=2,FILE='roundspecifier.dat', ROUND='COMPATIBLE')
  inquire(2, ROUND=which_mode)
  if(which_mode .ne. 'COMPATIBLE') error stop 5
  close(2)

  open(UNIT=2,FILE='roundspecifier.dat', ROUND='PROCESSOR_DEFINED')
  inquire(2, ROUND=which_mode)
  if(which_mode .ne. 'PROCESSOR_DEFINED') error stop 6
  close(2)

  open(UNIT=2,FILE='roundspecifier.dat')
  inquire(2, ROUND=which_mode)
  if(which_mode .ne. 'PROCESSOR_DEFINED') error stop 7
  close(2)

  round_spec='UP'
  open(UNIT=2,FILE='roundspecifier.dat', ROUND=round_spec)
  inquire(2, ROUND=which_mode)
  if(which_mode .ne. 'UP') error stop 8
  close(2)


  round_spec='DOWN'
  open(UNIT=2,FILE='roundspecifier.dat', ROUND=round_spec)
  inquire(2, ROUND=which_mode)
  if(which_mode .ne. 'DOWN') error stop 9
  close(2)

  round_spec='ZERO'
  open(UNIT=2,FILE='roundspecifier.dat', ROUND=round_spec)
  inquire(2, ROUND=which_mode)
  if(which_mode .ne. 'ZERO') error stop 10
  close(2)

  round_spec='NEAREST'
  open(UNIT=2,FILE='roundspecifier.dat', ROUND=round_spec)
  inquire(2, ROUND=which_mode)
  if(which_mode .ne. 'NEAREST') error stop 11
  close(2)

  round_spec='COMPATIBLE'
  open(UNIT=2,FILE='roundspecifier.dat', ROUND=round_spec)
  inquire(2, ROUND=which_mode)
  if(which_mode .ne. 'COMPATIBLE') error stop 12
  close(2)

  round_spec='processor_defined'
  open(UNIT=2,FILE='roundspecifier.dat', ROUND=round_spec)
  inquire(2, ROUND=which_mode)
  if(which_mode .ne. 'PROCESSOR_DEFINED') error stop 13
  close(2)

end








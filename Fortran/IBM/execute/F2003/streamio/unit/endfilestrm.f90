! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: endfilestrm.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*
!*  ===================================================================
!*
!*                             :
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  DIAGNOSES TESTED           : ENDFILE statement
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890


      Program endfilestrm

      integer iostat, pos, size, old, int1, int2, int3

      open(11, status='scratch', access='stream', form='formatted', iostat=iostat)

      if (iostat <> 0) error stop 1

      write(11, fmt='($, 2I3/I4)', iostat=iostat) 123, 456, 7890
      if (iostat <> 0) error stop 2

      inquire(11, size=old)

      rewind 11
      read(11, fmt='(2I3/I4)', iostat=iostat, advance='no') int1, int2, int3
      if (iostat <> 0) error stop 3
      if (int1 <> 123) error stop 4
      if (int2 <> 456) error stop 5
      if (int3 <> 7890) error stop 6

      inquire(11, pos=pos)
      if (pos <> 12) error stop 7

      endfile 11
      inquire(11, pos=pos, size=size)
      if (pos <> 12) error stop 8
      if (size <> old) error stop 9

      rewind 11
      read(11, *, iostat=iostat) int1
      if (iostat <> 0) error stop 10
      if (int1 <> 123456) error stop 11

      read(11, '(I2)', advance='no', iostat=iostat) int2
      if (iostat <> 0) error stop 12
      if (int2 <> 78) error stop 13

      inquire(11, pos=pos, size=size)
      if (pos <> 10) error stop 14
      if (size <> old) error stop 15

      endfile 11
      inquire(11, pos=pos, size=size)
      if (pos <> 10) error stop 16
      if (size <> 9) error stop 17

      write(11, '(I4)', iostat=iostat) 6666
      if (iostat <> 0) error stop 18

      backspace 11
      read(11, *, iostat=iostat) int3
      if (iostat <> 0) error stop 19
      if (int3 <> 786666) error stop 20

      inquire(11, pos=pos, size=size)
      ! comment until defect 248991 is fixed
      ! if (pos <> 15) error stop 21
      if (size <> 14) error stop 22

      End Program endfilestrm

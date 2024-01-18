! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: poscdestrm01.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*                             :
!*  PROGRAMMER                 : Daniel Chen
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  DIAGNOSES TESTED           : The usage of the control edit
!*                             : descriptors '/', 'r/' and '$' with
!*                             : stream formatted access.
!*                             :
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890


      Program poscdestrm01

      integer iostat, pos, size, int1, int2, int3
      real r1, r2

      open(11, status='scratch', access='stream', form='formatted', iostat=iostat)

      if (iostat <> 0) error stop 1

      write(11, fmt='($, 2I3/I4)', iostat=iostat) 123, 456, 7890
      if (iostat <> 0) error stop 2

      write(11, fmt='(F4.1, F7.4)', iostat=iostat) 23.3, 45.6789
      if (iostat <> 0) error stop 3

      rewind 11
      read(11, fmt='(2I3/I5)', iostat=iostat, advance='no') int1, int2, int3
      if (iostat <> 0) error stop 4
      if (int1 <> 123) error stop 5
      if (int2 <> 456) error stop 6
      if (int3 <> 78902) error stop 7

      inquire(11, pos=pos)
      if (pos <> 13) error stop 8

      open(11, access='stream', form='formatted', position='asis', iostat=iostat)
      if (iostat <> 0) error stop 9

      inquire(11, pos=pos, size=size)
      if (pos <> 13) error stop 10

      write(11, pos=size+1, fmt='($, E8.3)', iostat=iostat) 1.236E-2
      if (iostat <> 0) error stop 11

      write(11, '($, I1)', iostat=iostat) 3
      if (iostat <> 0) error stop 12

      backspace 11
      backspace 11

      read(11, fmt='(F9.2/E9.3)', iostat=iostat) r1, r2
      if (iostat <> 0) error stop 13
      if (r1 <> 789023.34) error stop 14
      if (r2 <> 1.24E-14) error stop 15

      backspace 11
      read(11, *, iostat=iostat) r2
      if (iostat <> 0) error stop 16

      ! comment until defect 248991 is fixed.
      ! inquire(11, pos=pos)
      ! if (pos <> 34) error stop 17
      if (r2 <> 1.24E-14) error stop 18

      End Program poscdestrm01

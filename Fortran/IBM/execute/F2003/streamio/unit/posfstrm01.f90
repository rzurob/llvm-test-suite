! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: posfstrm01.f
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
!*  DIAGNOSES TESTED           : POS change with formatted
!*                             : stream access when Pad is set to yes.
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


      Program posfstrm01

      integer iostat, pos, size
      character*2 c1, c2
      character*4 ch1, ch2

      open(11, status='scratch', access='stream', form='formatted', iostat=iostat)
      if (iostat <> 0) error stop 1

      write(11, '(A2/A2)', iostat=iostat) 'ab', 'cd'
      if (iostat <> 0) error stop 2

      inquire(11, pos=pos)
      if (pos <> 7) error stop 3

      read(11, *, iostat=iostat) ch1
      if (iostat <> -1) error stop 4

      read(11, '(A4)', pos=4, advance='no', iostat=iostat) ch1
      if (iostat <> -1) error stop 5

      inquire(11, pos=pos)
      if (pos <> 7) error stop 6

      read(11, '(A4)', pos=4, iostat=iostat) ch1
      if (iostat <> -1) error stop 7

      inquire(11, pos=pos)
      if (pos <> 7) error stop 8

      write(11, '($, A4)', iostat=iostat) 'efgh'
      if (iostat <> 0) error stop 9

      inquire(11, pos=pos)
      if (pos <> 11) error stop 10

      read(11, '(A4)', iostat=iostat) ch1
      if (iostat <> -1) error stop 11

      inquire(11, pos=pos, size=size)
      if (pos <> 11) error stop 12
      if (size <> 10) error stop 13

      read(11, '(A4)', advance='no', iostat=iostat) ch1
      if(iostat <> -1) error stop 14

      backspace (11, iostat=iostat)
      if (iostat <> 0) error stop 15
      inquire(11, pos=pos)
      if (pos <> 7) error stop 16

      read(11, '(A4)', advance='no', iostat=iostat) ch1
      if (iostat <> 0) error stop 17
      inquire(11, pos=pos)
      if (pos <> 11) error stop 18

      read(11, '(A4)', pos=9, iostat=iostat) ch1
      if (iostat <> -1) error stop 19

      inquire(11, pos=pos)
      if (pos <> 11) error stop 20

      read(11, '(A4)', pos=9, advance='no', iostat=iostat) ch1
      if (iostat <> -1) error stop 21

      inquire(11, pos=pos)
      if (pos <> 11) error stop 22

      write(11, '(A1)') 'x'
      inquire(11, pos=pos, size=size)
      if(pos <> 13) error stop 23
      if (size <> 12) error stop 24

      End Program posfstrm01

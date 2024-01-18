! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: posfstrm03.f
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
!*  DIAGNOSES TESTED           : Writing an empty record with no record
!*                             : marker has no effect.
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890


      Program posfstrm03

      integer iostat, pos, size, old
      character*7 ch2
      character*4 ch1

      open(11, status='new', access='stream', form='formatted', iostat=iostat)
      if (iostat <> 0) error stop 1

      write(11, '($, A2/A2)', pos=4, iostat=iostat) 'ab', 'cd'
      if (iostat <> 0) error stop 2

      inquire(11, pos=pos, size=old)
      if (pos <> 9) error stop 3

      write(11, '($, A4)', iostat=iostat)
      if (iostat <> 0) error stop 4

      inquire(11, pos=pos, size=size)
      if (pos <> 9) error stop 5
      if (size <> old) error stop 6

      write(11, '(A2)', pos=12, iostat=iostat) 'ef'
      if (iostat <> 0) error stop 7

      inquire(11, pos=pos, size=size)
      if (pos <> 15) error stop 8
      if (size <> 14) error stop 9

      write(11, '(A3)', pos=1, iostat=iostat) '123'
      if (iostat <> 0) error stop 10

      inquire(11, pos=pos, size=size)
      if (pos <> 5) error stop 11
      if (size <> 14) error stop 12

      write(11, '(A3)', pos=9, advance='no', iostat=iostat) 'xxx'
      if (iostat <> 0) error stop 13

      rewind 11
      read(11, *, iostat=iostat) ch1
      if (iostat <> 0) error stop 14
      if (ch1 <> '123') error stop 15

      read(11, *, iostat=iostat) ch1
      if (iostat <> 0) error stop 16
      if (ch1 <> 'b') error stop 17

      read(11, *, iostat=iostat) ch2
      if (iostat <> 0) error stop 18
      if (ch2 <> 'cdxxxef') error stop 19

      End Program posfstrm03

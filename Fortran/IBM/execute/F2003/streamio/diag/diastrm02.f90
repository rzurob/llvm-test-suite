! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: diastrm02.f
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
!*  DIAGNOSES TESTED           : The file storage units of a file connected
!*                             : for unformatted stream access can only be
!*                             : read or written by unformatted stream
!*                             : input/output statements.
!*                             : The file stroage units of a file connected
!*                             : for formatted stream access can only be
!*                             : read or written by formatted stream access
!*                             : input/output statements.
!*                             : Recoving from recoverable error conditions
!*                             : is also tested.
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

      Program diastrm02

      integer iostat
      character*2 c

      open(11, status='scratch', access='stream', iostat=iostat)
      if (iostat <> 0) error stop 1

      write(11, fmt='(A5)', iostat=iostat ) "abcde"
      if (iostat <> 20) error stop 2

!     recovering tests
      write(11, iostat=iostat) "1234"
      if (iostat <> 0) error stop 3

      read(11, fmt='(A2)', iostat=iostat) c
      if (iostat <> 20) error stop 4

!     recovering tests
      read(11, pos=2, iostat=iostat) c
      if (iostat <> 0) error stop 5
      if (c <> '23') error stop 6

      open(12, status='scratch', access='stream', form='formatted', iostat=iostat)
      if (iostat <> 0) error stop 7

      write(12, iostat=iostat) "abcde"
      if (iostat <> 19) error stop 8

!     recovering tests
      write(12, *, iostat=iostat) "12345"
      if (iostat <> 0) error stop 9

      rewind(12)
      read(12, iostat=iostat) c
      if (iostat <> 19) error stop 10

!     recovering tests
      read(12, *, iostat=iostat) c
      if (iostat <> 0) error stop 11
      if (c <> '12') error stop 12

      End Program diastrm02

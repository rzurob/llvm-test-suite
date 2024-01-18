! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/diastrm01.sh diastrm01
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
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
!*  DIAGNOSES TESTED           : The RECL specifier must not appear when
!*                             : a file is being connected for stream
!*                             : access via the OPEN statement.
!*                             : The REC specifier must not appear in
!*                             : input/output statements that specify a
!*                             : unit connected for stream access.
!*                             : This test case also tests if the program
!*                             : can be recovered from the above recoverable
!*                             : error conditions.
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

      Program diastrm01

      integer iostat
      character*2 c
      character*6 :: access = "stream"

      open(11, status='scratch', access=access, recl=5, form='unformatted')

!     While iostat is not specified in OPEN statement, the compiler
!     is trying to recover by ignoring the RECL specifier and still
!     connecting the file to unit 11.

      write(11, rec=2, iostat=iostat ) "abcde"
      if (iostat <> 22) error stop 1

!     Recovering tests
      write(11, iostat=iostat) "12345"
      if (iostat <> 0) error stop 2

      read(11, rec=1, iostat=iostat) c
      if (iostat <> 22) error stop 3

!     Recovering tests
      read(11, pos=2, iostat=iostat) c
      if (iostat <> 0) error stop 4
      if (c <> "23") error stop 5

      open(12, status='scratch', access=access, recl=5, form='unformatted', iostat=iostat)

      if (iostat <> 191) error stop 6
!     While iostat is specified in OPEN statement, the compiler
!     is NOT trying to recover. The connection to unit 12 is NOT established.

      write(12, rec=2, iostat=iostat) "abcde"
      if (iostat <> 18) error stop 7
      End Program diastrm01

! *********************************************************************
!*
!*  ===================================================================
!*
!*                             : that cannot be positioned.
!*
!*  DIAGNOSES TESTED           : Stream I/O statements can access the
!*                             : files that cannot be positioned.
!*                             : The POS specifier must not be used
!*                             : on data transfer statements with files
!*                             : that cannot be positioned.
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program nonseekstrm

      integer iostat

      open(11, file='/dev/tty', access='stream', form='formatted', iostat=iostat)
      if (iostat <> 0) error stop 1


      write(11, '(A3)', iostat=iostat) "abc"
      if (iostat <> 0) error stop 3

      write(11, *, pos=1, iostat=iostat) "123"
      if (iostat <> 197) error stop 4

      open(12, file='/dev/null', access='stream', form='unformatted', iostat=iostat)
      if (iostat <> 0) error stop 5

      write(12, iostat=iostat) "abc"
      if (iostat <> 0) error stop 6

      End Program nonseekstrm
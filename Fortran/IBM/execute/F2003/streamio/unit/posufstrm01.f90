! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: posufstrm01.f
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
!*  DIAGNOSES TESTED           : The POS specifier can not be less
!*                             : than one.
!*                             : BACKSPACE can not be referred in a
!*                             : unit connected for unformatted stream
!*                             : access.
!*                             : Data transfer statements with
!*                             : unformatted stream access.
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890


      Program posufstrm01

      integer iostat, pos, size, number, i
      character*2 c
      i = 0

      open(11, status='scratch', access='stream', iostat=iostat)
      if (iostat <> 0) error stop 1

      write(11, pos=i, iostat=iostat) 12, 34
      if (iostat <> 195) error stop 2

      write(11, iostat=iostat) 12, 34
      if (iostat <> 0) error stop 3
      inquire(11, pos=pos, size=size)
      if (pos <> 9) error stop 4
      if (size <> 8) error stop 5

      write(11, pos=13, iostat=iostat)
      if (iostat <> 0) error stop 6
      inquire(11, pos=pos, size=size)
      if (pos <> 13) error stop 7
      if (size <> 8) error stop 8


      write(11, pos=5, iostat=iostat)
      if (iostat <> 0) error stop 6
      inquire(11, pos=pos, size=size)
      if (pos <> 5) error stop 9
      if (size <> 8) error stop 10

      read(11, iostat=iostat) number
      if (iostat <> 0) error stop 11
      if (number <> 34) error stop 12

      read(11, iostat=iostat) number
      if (iostat <> -1) error stop 13
      read(11, pos=7, iostat=iostat) number
      if (iostat <> -1) error stop 14

      write(11, pos=13, iostat=iostat) 56
      if (iostat <> 0) error stop 15
      inquire(11, pos=pos, size=size)
      if (pos <> 17) error stop 16
      if (size <> 16) error stop 17

      read(11, pos=9, iostat=iostat) number
      if (iostat <> 0) error stop 18
      if (number <> 0) error stop 19

      write(11, pos=9, iostat=iostat) 99
      if (iostat <> 0) error stop 20

      backspace(11, iostat=iostat)
      if (iostat <> 194) error stop 21

      rewind(11)
      read(11, pos=9, iostat=iostat) number
      if (iostat <> 0) error stop 22
      if (number <> 99) error stop 23

      read(11, pos=1, iostat=iostat) number
      if (iostat <> 0) error stop 24
      if (number <> 12) error stop 25

      inquire(11, size=size)
      if (size <> 16) error stop 26

      End Program posufstrm01

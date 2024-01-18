!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: async03.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Stream Access I/O
!*
!*  PROGRAMMER                 : Rafik Zurob
!*  DATE                       : April 2003
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Asynchronous Stream IO
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      character*0 c0
      character*3 c3
      character*4 c4
      character*6 c6
      integer pos
      integer id1, id2
      character*36 c36

      open(11, access='stream', status='scratch', action='readwrite', asynch='yes')

      do i = 1, 400
        write(11) '1234567890abcdefghijklmnopqrstuvwxyz'
      end do
      rewind 11

      ! (no pos, no pos)
      write(11, id=id1) '!@#'
      write(11, id=id2) '$%^&'
      wait(11, id=id1)
      wait(11, id=id2)
      inquire(11, pos=pos)
      read(11, pos=1) c3
      read(11) c4

      if (c3 /= "!@#") error stop 1
      if (c4 /= "$%^&") error stop 2
      if (pos /= 8) error stop 3

      ! (pos, no pos)
      write(11, id=id1, pos=18) 'HIJKLM'
      write(11, id=id2) 'NOPQ'
      wait(11, id=id1)
      wait(11, id=id2)
      inquire(11, pos=pos)
      read(11, pos=18) c6
      read(11) c4

      if (c6 /= 'HIJKLM') error stop 4
      if (c4 /= 'NOPQ') error stop 5
      if (pos /= 28) error stop 6

      ! (no pos, pos), spans buffers,  non-overlapping writes
      write(11, id=id1) 'RSTUVW'
      write(11, id=id2, pos=12457) '!@#$'
      wait(11, id=id1)
      wait(11, id=id2)
      inquire(11, pos=pos)
      read(11, pos=28) c6
      read(11, pos=12457) c4

      if (c6 /= 'RSTUVW') error stop 7
      if (c4 /= '!@#$') error stop 8
      if (pos /= 12461) error stop 9

      ! (pos, pos), spans buffers,  non-overlapping writes
      write(11, id=id1, pos=12) 'BCDEFG'
      write(11, id=id2, pos=14397) 'WXYZ'
      wait(11, id=id1)
      wait(11, id=id2)
      inquire(11, pos=pos)
      read(11, pos=12) c6
      read(11, pos=14397) c4

      if (c6 /= 'BCDEFG') error stop 10
      if (c4 /= 'WXYZ') error stop 11
      if (pos /= 14401) error stop 12

      read(11, pos=10)
      inquire(11, pos=pos)
      if (pos /= 10) error stop 13

      read(11, pos=11) c0
      inquire(11, pos=pos)
      if (pos /= 11) error stop 14

      end

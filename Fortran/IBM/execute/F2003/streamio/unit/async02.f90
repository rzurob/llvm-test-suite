!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: async02.f
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
      read(11, id=id1) c3
      read(11, id=id2) c4
      wait(11, id=id1)
      wait(11, id=id2)
      inquire(11, pos=pos)

      if (c3 /= "123") error stop 1
      if (c4 /= "4567") error stop 2
      if (pos /= 8) error stop 3

      ! (pos, no pos)
      read(11, id=id1, pos=1) c6
      read(11, id=id2) c4
      wait(11, id=id1)
      wait(11, id=id2)
      inquire(11, pos=pos)

      if (c6 /= "123456") error stop 4
      if (c4 /= "7890") error stop 5
      if (pos /= 11) error stop 6

      ! (no pos, pos), spans buffers,  non-overlapping reads
      read(11, id=id1) c6
      read(11, id=id2, pos=12457) c4
      wait(11, id=id1)
      wait(11, id=id2)
      inquire(11, pos=pos)

      if (c6 /= "abcdef") error stop 7
      if (c4 /= "1234") error stop 8
      if (pos /= 12461) error stop 9

      ! (pos, pos), spans buffers,  non-overlapping reads
      read(11, id=id1, pos=12) c6
      read(11, id=id2, pos=14397) c4
      wait(11, id=id1)
      wait(11, id=id2)
      inquire(11, pos=pos)

      if (c6 /= "bcdefg") error stop 10
      if (c4 /= "wxyz") error stop 11
      if (pos /= 14401) error stop 12

      read(11, pos=10)
      inquire(11, pos=pos)
      if (pos /= 10) error stop 13

      read(11, pos=11) c0
      inquire(11, pos=pos)
      if (pos /= 11) error stop 14

      ! (no pos, pos), overlapping reads
      read(11, id=id1) c6
      read(11, id=id2, pos=15) c4
      wait(11, id=id1)
      wait(11, id=id2)
      inquire(11, pos=pos)

      if (c6 /= "abcdef") error stop 15
      if (c4 /= "efgh") error stop 8
      if (pos /= 19) error stop 9

      ! (pos, pos), overlapping reads
      read(11, id=id1, pos=12) c6
      read(11, id=id2, pos=10) c4
      wait(11, id=id1)
      wait(11, id=id2)
      inquire(11, pos=pos)

      if (c6 /= "bcdefg") error stop 10
      if (c4 /= "0abc") error stop 11
      if (pos /= 14) error stop 12

      end

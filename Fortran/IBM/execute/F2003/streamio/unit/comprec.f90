!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: comprec.f
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
!*  DESCRIPTION                : Basic testing of ADVANCE
!*                               The advance specifier and $ descriptor
!*                               wrt complete and incomplete records.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      integer ipos, size
      character*1 c1
      character*3 c3
      character*4 c4

      open(11, access='stream', form='formatted', status='scratch')

      ! Advancing write.  We should be positioned after the record marker.
      write(11, '(A)') "abcd"
      inquire(11, pos=ipos)
      if (ipos /= 6) error stop 1

      rewind 11
      inquire(11, pos=ipos)
      if (ipos /= 1) error stop 2

      ! Advancing read.  We should be positioned after the record marker.
      read(11, '(A)') c4
      inquire(11, pos=ipos)
      if (ipos /= 6) error stop 3
      if (c4 /= 'abcd') error stop 4

      close(11)


      open(11, access='stream', form='formatted', &
           action='readwrite', status='scratch')

      ! Non-advancing write using the ADVANCE= specifier.
      write(11, '(A3)', advance='no') 'abc'
      inquire(11, pos=ipos)
      if (ipos /= 4) error stop 1

      rewind 11
      inquire(11, pos=ipos)
      if (ipos /= 1) error stop 2

      ! Advancing read.  We're reading an incomplete record.
      read(11, '(A)') c3
      inquire(11, pos=ipos)
      if (ipos /= 4) error stop 3
      if (c3 /= 'abc') error stop 4

      close(11)


      open(11, access='stream', form='formatted', status='scratch')

      ! Non-advancing write using the $ descriptor
      write(11, '($, A3)') "abc"
      inquire(11, pos=ipos, size=size)
      if (ipos /= 4) error stop 1
      if (size /= 3) error stop 2

      ! Advancing read.  We're reading an incomplete record.
      read(11, '(A1)', pos=3) c1
      inquire(11, pos=ipos, size=size)
      if (ipos /= 4) error stop 3
      if (size /= 3) error stop 4

      ! Append to the incomplete record using an advancing write.
      ! We will now have a complete record.
      write(11, '(A1)') "x"
      inquire(11, pos=ipos, size=size)
      if (ipos /= 6) error stop 6
      if (size /= 5) error stop 7

      read(11, '(A1)', pos=4) c1
      if (c1 /= 'x') error stop 8

      end

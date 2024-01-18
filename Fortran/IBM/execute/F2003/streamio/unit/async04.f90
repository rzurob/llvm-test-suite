!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: async04.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************

!*  ===================================================================
!*
!*  DATE                       : April 2003
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Asynchronous Stream IO
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      character*36, dimension(400) :: c
      integer pos
      character*4 c4
      open(11, access='stream', status='scratch', action='readwrite', asynch='yes')

      c = '1234567890abcdefghijklmnopqrstuvwxyz'

      write(11, pos=14401) "4567"
      rewind 11

      ! (no pos, no pos)
      write(11, id=id1) c
      write(11, id=id2) "$%^&"
      wait(11, id=id1)
      wait(11, id=id2)
      inquire(11, pos=pos)
      read(11, pos=14401) c4

      if (c4 /= "$%^&") error stop 1
      if (pos /= 14405) error stop 2

      end

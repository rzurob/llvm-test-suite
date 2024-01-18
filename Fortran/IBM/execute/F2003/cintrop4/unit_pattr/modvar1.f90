!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/vfloop.sh
! %COMPOPTS: -qdebug=intmsg
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!***********************************************************************

      module modvar1
        bind(c)            :: j1  ! ok
        bind(c, name='k2') :: j2  ! ok
        integer, bind(c)            :: j3  ! ok
        integer, bind(c, name='k4') :: j4  ! ok
      end module

      program p1
        use modvar1
        integer, bind(c, name='k5') :: j5  ! err: bind(C) on non-module var
      end program

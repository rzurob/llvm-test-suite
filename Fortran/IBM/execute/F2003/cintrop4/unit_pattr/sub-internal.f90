!*********************************************************************
!***********************************************************************

      program x
      contains
        ! BIND(C) w/out NAME=
        subroutine s1 () BIND(C)  ! err: BIND(C) on internal sub
          return
        end subroutine

        ! BIND(C) w/ NAME= and non-default name
        subroutine sn1 () BIND(C, name='S1')  ! err: BIND(C) on internal sub
          return
        end subroutine

        ! BIND(C) w/ NAME= and default name
        subroutine sd1 () BIND(C, name='sd1')  ! err: BIND(C) on internal sub
          return
        end subroutine
      end

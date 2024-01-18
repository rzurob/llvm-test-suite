!*********************************************************************
!***********************************************************************

      !!! BIND(C) w/out NAME=
      subroutine s1 BIND(C)  ! err: sub w/out parens, no NAME=
        return
      end subroutine

      subroutine s2 () BIND(C)  ! ok: sub w/ parens and 0 args, no NAME=
        return
      end subroutine

      subroutine s3 (a) BIND(C)  ! ok: sub w/ parens and 1 arg, no NAME=
        return
      end subroutine

      subroutine s4 (a, b) BIND(C)  ! ok: sub w/ parens and 2 args, no NAME=
        return
      end subroutine

      !!! BIND(C) w/ NAME= and non-default name
      subroutine sn1 BIND(C, name='S1')  ! err: sub w/out parens, NAME=non-default
        return
      end subroutine

      subroutine sn2 () BIND(C, name='S2')  ! ok: sub w/ parens and 0 args, NAME=non-default
        return
      end subroutine

      subroutine sn3 (a) BIND(C, name='S3')  ! ok: sub w/ parens and 1 arg, NAME=non-default
        return
      end subroutine

      subroutine sn4 (a, b) BIND(C, name='S4')  ! ok: sub w/ parens and 2 args, NAME=non-default
        return
      end subroutine

      !!! BIND(C) w/ NAME= and default name
      subroutine sd1 BIND(C, name='sd1')  ! err: sub w/out parens, NAME=default
        return
      end subroutine

      subroutine sd2 () BIND(C, name='sd2')  ! ok: sub w/ parens and 0 args, NAME=default
        return
      end subroutine

      subroutine sd3 (a) BIND(C, name='sd3')  ! ok: sub w/ parens and 1 arg, NAME=default
        return
      end subroutine

      subroutine sd4 (a, b) BIND(C, name='sd4')  ! ok: sub w/ parens and 2 args, NAME=default
        return
      end subroutine

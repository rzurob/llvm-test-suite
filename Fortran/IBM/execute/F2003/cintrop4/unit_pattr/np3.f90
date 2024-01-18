!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/vfloop.sh
! %COMPOPTS: -qdebug=intmsg -c
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!***********************************************************************

      !!! No Parentheses: verify that grammar requires parentheses when
      !!! suffix is present.
      !!! Set 3: subroutine with bind(c) suffix.

      subroutine s1 () BIND(C)  ! ok: parens between subroutine and bind-no-name
      end subroutine

      subroutine sn1 () BIND(C, name='S1')  ! ok: parens btw sub & bind-NAME=
      end subroutine

      ! in -qfixed, the `BIND' in these becomes absorbed into the sub-name:
      subroutine s2 BIND(C)  ! err: no-parens btw sub and bind-no-name
      end subroutine

      subroutine sn2 BIND(C, name='S2')  ! err: no-parens btw sub & bind-NAME=
      end subroutine

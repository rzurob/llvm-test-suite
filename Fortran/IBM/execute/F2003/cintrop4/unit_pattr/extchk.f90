!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/vfloop.sh -cf $TR_SRC/c-f-pair.sh
! %COMPOPTS: -qdebug=intmsg
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!***********************************************************************

      !?? manually tested with module procedures and without: ok.
      !?? manually tested f2f: ok.
      !?? todo: create a 2nd version that is parameterized.
      ! Must vary on:
      ! * module, external procedures.
      ! * subroutines, functions.
      ! * num args (1,2).
      ! * type of args (integer, real, char).
      ! * for functions, return type (integer, real, ??char).
      ! * -qextchk (on, off).
      ! * don't bother with running the resulting executable so don't
      !   add print stms either.
      ! * call from and to for each of (f2f, c2f, f2c).

      module extchk
      contains
        subroutine f1_f (j)  bind(C, name='f1_fB')
          integer j
        end subroutine

        subroutine f2_f (j)  bind(C, name='f2_fB')
          integer j
        end subroutine
      end module

      subroutine f1c_via_f ()
        interface
          function f1_c (j)  bind(C, name='f1_cB')
            real j
          end function
        end interface

        real rv
        rv = f1_c (3.0)
      end subroutine

      subroutine f2f_via_f ()
        interface
          function f2_fLocalName (j)  bind(C, name='f2_fB')
            real j
          end function
        end interface

        real rv
        rv = f2_fLocalName (3.0)
      end subroutine

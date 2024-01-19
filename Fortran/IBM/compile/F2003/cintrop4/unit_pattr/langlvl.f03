!*********************************************************************
!***********************************************************************

      !!! BIND(C) is disallowed with older language levels.
      !!! Test error messages for BIND(C) when -qlanglvl=95std .

      module langlvl
        !! module variable bind(c), without NAME=
        bind(c)    j1 ! err: langlvl disallows BIND(C) modvar
        bind(c) :: j2 ! err: langlvl disallows BIND(C) modvar

        !! module variable bind(c), with NAME=
        bind(c, name='j3B') :: j3 ! err: langlvl BIND(C) modvar

      contains
        !! module SUBROUTINE/FUNCTION/ENTRY bind(c), without NAME=
        subroutine ms () bind(c)  ! err: langlvl disallows BIND(C) mod-sub
        entry mse () bind(c)  ! err: langlvl disallows BIND(C) mod-sub-entry
        end subroutine
        function mf() bind(c)  ! err: langlvl disallows BIND(C) mod-fn
          mf = 1.0
        entry mfe() bind(c)  ! err: langlvl disallows BIND(C) mod-fn-entry
          mfe = 1.0
        end function

        !! module SUBROUTINE/FUNCTION/ENTRY bind(c), with NAME=
        subroutine ms2 () bind(c, name='ms2B')  ! err: langlvl BIND(C) mod-sub
        entry ms2e () bind(c, name='ms2eB') ! err: langlvl BIND(C) mod-sub-entry
        end subroutine
        function mf2() bind(c, name='mf2B')  ! err: langlvl BIND(C) mod-fn
          mf2 = 1.0
        entry mf2e() bind(c, name='mf2eB')  ! err: langlvl BIND(C) mod-fn-entry
          mf2e = 1.0
        end function
      end module

      !! external SUBROUTINE/FUNCTION/ENTRY bind(c), without NAME=
      subroutine s () bind(c)  ! err: langlvl disallows BIND(C) subroutine
      entry se () bind(c)  ! err: langlvl disallows BIND(C) subroutine-entry
      end subroutine
      function f() bind(c)  ! err: langlvl disallows BIND(C) function
        f = 1.0
      entry fe() bind(c)  ! err: langlvl disallows BIND(C) function-entry
        fe = 1.0
      end function

      !! external SUBROUTINE/FUNCTION/ENTRY bind(c), with NAME=
      subroutine s2 () bind(c, name='s2B')  ! err: langlvl BIND(C) subroutine
      entry se2 () bind(c, name='se2B')  ! err: langlvl BIND(C) subroutine-entry
      end subroutine
      function f2() bind(c, name='f2B')  ! err: langlvl BIND(C) function
        f2 = 1.0
      entry fe2() bind(c, name='fe2B')  ! err: langlvl BIND(C) function-entry
        fe2 = 1.0
      end function

      program p
        common /c1/ k1
        common /c2/ k2
        common /c3/ k3
        common /c4/ k4

        !! common block bind(c), without NAME=
        bind(c)    /c1/  ! err: langlvl disallows BIND(C) common block
        bind(c) :: /c2/  ! err: langlvl disallows BIND(C) common block

        !! common block bind(c), with NAME=
        bind(c, name='c3B')    /c3/  ! err: langlvl BIND(C) common block
        bind(c, name='c4B') :: /c4/  ! err: langlvl BIND(C) common block
      end

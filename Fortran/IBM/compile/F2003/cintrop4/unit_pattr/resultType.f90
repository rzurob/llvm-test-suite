!*********************************************************************
!***********************************************************************

      ! fn non-interop by IMPLICIT keyword
      !
      function f1 ()  bind(C, name='f1_b')  ! 1 err: f non-interop
        implicit character*2 (f,r)
        character*2 g2, g3r

        ! function-entry-result non-interop by IMPLICIT keyword
        ! where fn-entry is also implicitly non-interop
        !
        entry f1e2 () bind(C, name='f1e2_b') result(r1) ! 1 err: fer non-interop
        r1 = 'aa'

        ! function-entry-result non-interop by IMPLICIT keyword
        ! where fn-entry isn't implicitly non-interop
        !
        entry g1 () bind(C, name='g1_b') result(r2)  ! 1 err: fer non-interop
          r2 = 'aa'

        ! function-entry-result non-interop by expl dcl
        !
        entry g3 () bind(C, name='g3_b') result(g3r)  ! 1 err: fer non-interop
          g3r = 'aa'

        ! fn-entry non-interop by IMPLICIT keyword
        !
        entry f1e1 () bind(C, name='f1e1_b')  ! 1 err: fe non-interop
          f1e = 'aa'

        ! fn-entry non-interop by expl dcl
        !
        entry g2 () bind(C, name='g2_b')  ! 1 err: fe non-interop
          g2 = 'aa'
      end function

      ! fn non-interop by expl dcl
      !
      function f2 ()  bind(C, name='f2_b')  ! 1 err: f non-interop
        character*2 f2
        f2 = 'aa'
      end function

      ! fn-result non-interop by IMPLICIT keyword
      ! where fn-entry is also implicitly non-interop
      !
      function f5 ()  bind(C, name='f5_b') result(f5r)  ! 1 err: fr non-interop
        implicit character*2 (f)
        f5r = 'aa'
      end function

      ! fn-result non-interop by expl dcl
      ! where fn-entry isn't implicitly non-interop
      !
      function f4 ()  bind(C, name='f4_b') result(r1)  ! 1 err: fr non-interop
        implicit character*2 (r)
        r1 = 'aa'
      end function

      ! fn-result non-interop by expl dcl
      !
      function f3 ()  bind(C, name='f3_b') result(r1)  ! 1 err: fr non-interop
        character*2 r1
        r1 = 'aa'
      end function

      program p1

        ! fn-result in interface dcl non-interop by expl dcl
        !
        interface
          function fA ()  bind(C, name='fA_b') result(r1)  ! 1 err: fri non-interop
            character*2 r1
          end function
        end interface

      end

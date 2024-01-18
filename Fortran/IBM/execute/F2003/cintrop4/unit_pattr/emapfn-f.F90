      !!! Implement int-function-no-args in C and Fortran and call:
      !!!   F -> F-impl
      !!!   F -> C-impl
      !!!   C -> F-impl
      !!!   C -> C-impl  (for completeness -- it's not really significant)
      !
      ! Abbreviations:
      !   * `f1_c' = 1st function, implemented in C.
      !   * `f1c_via_f' = 1st function (implemented in C),
      !     called from Fortran procedure.
      !
      ! Compile with -WF,-DMOD to test module functions;
      ! compile without it to test external functions.
      ! Must provide -WF,-DFTYPE=integer*2 (or other type definition)
      ! during compilation.

#ifndef FTYPE
#  define FTYPE real*8
#endif

#ifdef MOD
      module emapfn
        !! int-function-no-args implemented in C:
        !!
        interface
          function f1_c ()  bind(C, name='f1_cB')
            FTYPE f1_c
          end function
        end interface

      contains
#endif

        !! int-function-no-args implemented in Fortran:
        !!
        function f1_f ()  bind(C, name='f1_fB')

          FTYPE f1_f

          ! NB: The '(A)' edit descriptor prevents output of a leading blank.
          ! Only used where convenient.

          print '(A)', 'f1_f entered'
          f1_f = 4
        end function

#ifdef MOD
      end module
#endif


      subroutine f1f_via_f
#ifdef MOD
        use emapfn
#else
        interface
          function f1_f ()  bind(C, name='f1_fB')
            FTYPE f1_f
          end function
        end interface
#endif

        ! In Fortran, cannot use directly in print stm a function that prints.
        FTYPE rv  ! Return Value

        print '(A)', ''
        print '(A)', 'F calling f1_f'
        rv = f1_f ()
        print*, 'rv =',  rv
      end subroutine


      subroutine f1c_via_f
#ifdef MOD
        use emapfn
#else
        interface
          function f1_c ()  bind(C, name='f1_cB')
            FTYPE f1_c
          end function
        end interface
#endif

        print '(A)', ''
        print '(A)', 'F calling f1_c'
        print*, 'rv =',  f1_c ()
      end subroutine

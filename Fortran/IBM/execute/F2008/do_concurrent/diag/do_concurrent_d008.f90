!*******************************************************************************
!*
!============================================================================
!*  XL Fortran Test Case                                IBM INTERNAL USE ONLY
!*
!============================================================================
!*
!*  TEST CASE NAME             : do_concurrent_d008.f
!*
!*  PROGRAMMER                 : Bernard Kan
!*  DATE                       : 2015-03-20
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED :
!*  ADAPTED FROM               :
!*
!*  DESCRIPTION                : 
!*    - A reference to a nonpure procedure appears inside a do concurrent
!*      construct
!*    - the procedure could be internal; or
!*      - external: outside the program, in a c program, in a module or submodule
!=============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890
module m
    interface 
      module integer function modp2(i, j)
        integer, intent(in) :: i, j
      end function
  
      module subroutine mods2()
      end subroutine
    end interface

    contains

      integer function modp(i, j)
        integer, intent(in) :: i, j
        print *, "in procedure modp"
        modp = i+j
      end

      subroutine mods()
        print *, "in procedure mods"
      end

end module

submodule (m) subm
    contains

      integer function modp2(i, j)
        integer, intent(in) :: i, j
        print *, "in procedure modp2"
        modp2 = i+j
      end

      subroutine mods2()
        print *, "in procedure mods2"
      end

end submodule subm

program prog
    use m

    integer lxmax, lymax; parameter (lxmax = 2222, lymax = 2222)
    dimension co(0:lxmax, 0:lymax), o(0:lxmax, 0:lymax)
    integer i, j

      interface

        function c_func(i, j) bind(c)
          import
          integer, intent(in) :: i, j
        end

        subroutine c_sub() bind(c)
          import
        end

      end interface 

      DO CONCURRENT (j = 0:ly, i = 0:lx/8-1)
          co(i, j) = B'00111111'; o(i, j) = 0
          tmp = p(i,j)
          tmp2 = modp(i,j)
          tmp3 = modp2(i,j)
          tmp4 = c_func(i,j)
          tmp5 = internalp(i,j)
      END DO

      DO CONCURRENT (j = 0:ly, i = 0:lx/8-1)
          co(i, j) = B'00111111'; o(i, j) = 0
          call s()
          call mods()
          call mods2()
          call c_sub()
          call internals()
      END DO

      ! nested case
      DO CONCURRENT (j = 0:ly)
        DO CONCURRENT (i = 0:lx/8-1)
          co(i, j) = B'00111111'; o(i, j) = 0
          tmp = p(i,j)
          tmp2 = modp(i,j)
          tmp3 = modp2(i,j)
          tmp4 = c_func(i,j)
          tmp5 = internalp(i,j)
        END DO
      END DO

      DO CONCURRENT (j = 0:ly)
        DO CONCURRENT (i = 0:lx/8-1)
          co(i, j) = B'00111111'; o(i, j) = 0
          call s()
          call mods()
          call mods2()
          call c_sub()
          call internals()
        END DO
      END DO

    contains

      integer function internalp(i, j)
        integer, intent(in) :: i, j
        print *, "in procedure internalp"
        internalp = i+j
      end

      subroutine internals()
        print *, "in procedure internals"
      end

end program

      integer function p(i, j)
        integer, intent(in) :: i, j
        print *, "in subroutine p"
        p = i+j
      end

      subroutine s()
        print *, "in subroutine s"
      end

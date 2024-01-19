!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-09-30
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 Omit FUNCTION and SUBROUTINE Keywords
!*  REFERENCE                  : Feature Number 376084
!*  REQUIRED COMPILER OPTIONS  : noopt
!*
!*  DESCRIPTION
!*
!*  Two modules, some module functions and subroutines are terminated by "END",
!*  the rest are terminated by "END SUBROUTINE" or "END FUNCTION"
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789
      module mod
        integer, allocatable :: i
      contains

        function x(r)
          logical, allocatable :: x
          real, allocatable :: r

          allocate(x)
          if (allocated(r)) then
            x = r > 3.0
          else
            x = .false.
          endif
        end function

        subroutine sub1
          integer :: b_from_mod =1
          print *, b_from_mod
        end

      end module

      module mod2
        use mod
      contains
        function y(r)
          use mod
          logical, allocatable :: y
          real, allocatable :: r

          allocate(y)
          y = x(r)

          if (.not. y .and. allocated(i)) then
            y = (i == 0)
          endif
        end

        subroutine sub2
          integer ::  b_from_mod2 =2
          print *, "here is b from mod: ", b_from_mod
          print *, "here is b from itself: ",  b_from_mod2
        end subroutine

      end module

      program main
        use mod2

        implicit none

        logical, allocatable :: l
        real, allocatable :: r
        integer, allocatable :: status

        allocate(status)

        allocate(l)
        l = y(r)
        print *, l

        print *, allocated(i)  ! False
        allocate(i)
        i = 3
        print *, i

        deallocate(l, stat=status)
        print *, "status", status

        l = y(r)
        if (allocated(l)) then
          print *, l
        else
          print *, "not allocated"
        endif

        deallocate(l, stat=status)
        print *, "status", status

        allocate(r, stat=status)
        print *, "status", status
        r = 3.0
        print *, r

        l = y(r)
        if (allocated(l)) then
          print *, l
        else
          print *, "not allocated"
        endif

        print *, allocated(i)  ! F
        i = 3
        print *, i

        deallocate(l, stat=status)
        print *, "status", status

        l = y(r)
        if (allocated(l)) then
          print *, l
        else
          print *, "not allocated"
        endif

        deallocate(l, stat=status)
        print *, "status", status


        l = x(r)
        if (allocated(l)) then
          print *, l
        else
          print *, "not allocated"
        endif
      end program



!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/12/2005
!*
!*  DESCRIPTION                : CLASS keyword (randomly apply statement
!                               labels)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

9   module m
10    type base
11        integer id
12        contains
13        procedure :: print => printBase
14    end type

20    contains
25        subroutine printBase (b)
21            class (base), intent(in) :: b

22            print *, b%id
30        end subroutine
18  end module

program fclass015
1  use m
5    type (base):: b1 = base(10)
4    class (base), allocatable :: b2

3    call b1%print

50    allocate (b2, source=base(200))

52    select type (b2)
100        type is (base)
101              if (b2%id /= 200) error stop 1_4
102          class default
103              error stop 2_4
55    end select
7  end

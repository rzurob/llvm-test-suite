! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/class/fclass015.f
! opt variations: -qnol -qnodeferredlp

!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 04/12/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : CLASS keyword (randomly apply statement
!                               labels)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

9   module m
10  type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
11    integer(k1)    id
12        contains
13        procedure :: print => printBase
14  end type

20    contains
25        subroutine printBase (b)
21            class (base(*,4)), intent(in) :: b

22            print *, b%id
30        end subroutine
18  end module

program fclass015
1  use m
5    type (base(20,4)):: b1 = base(20,4)(10)
4    class (base(:,4)), allocatable :: b2

3    call b1%print

50    allocate (b2, source=base(20,4)(200))

52    select type (b2)
100        type is (base(*,4))
101              if (b2%id /= 200) error stop 1_4
102          class default
103              error stop 2_4
55    end select
7  end

!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn024a.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 03/26/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : data pointer assignment (non-poly pointer array
!*                               points to poly-pointer arrays of different
!*                               dynamic types; rank-two arrays)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4 :: id = 0
    end type

    type, extends (base) :: child
        character*15 :: name = ''
    end type
end module

module m1
use m
    type, extends(child) :: gen3
        logical*2 :: flag = .false.
    end type

    class (gen3), allocatable, target :: g1_m (:,:)
end module

program fpAssgn024a
use m1
    class (base), pointer :: b_ptr (:,:)
    type (base), pointer :: b1 (:,:)
    type (gen3), target :: g1(3,3)

    allocate (g1_m(10,10))

    !! transfer g1%base to b1 through b_ptr
    b_ptr => g1
    b1 => b_ptr

    if ((size(b1) /= 9) .or. (.not. associated (b1, g1%base))) error stop 1_4

    b1 = reshape ((/(base (id=i), i=1,9)/), (/3,3/))
    g1%name = 'g1'
    g1%flag = .true.

    if (any (g1%id /= reshape ((/(i, i=1,9)/), (/3,3/)))) error stop 2_4

    !! transfer g1_m(::2,:)%base to b1 through b_ptr
    b_ptr => g1_m(::2,:)

    b1 => b_ptr

    if ((size(b1) /= 50) .or. (.not. associated (b1,  g1_m(::2,:)%base))) error stop 3_4

    b1(:,::2) = reshape ((/(base (id=i),i=1,25)/), (/5, 5/))


    if (any (g1_m(2::2,:)%id /= 0) .or. any(g1_m(::2,2::2)%id /=0)) error stop 4_4

    k = 1

    !! this loop shouldn't be reordered or to be optimized by compiler, except
    !! the unrolling of the inner loop
    do i = 1,10,2
        do j = 1, 10, 2
            if (g1_m(j,i)%id /= k) error stop 5_4

            k = k +1
        end do
    enddo

end

! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn006a4.f
! opt variations: -qnock -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

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
!*  DATE                       : 04/28/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : data pointer assignment (use the defined
!                               assignment to map the rank-one array into
!                               multi-dimentional array)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2)    ! (20,4,1)
        integer, kind             :: k2
        character(kind=k2,len=n1)    name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(*,4,1)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

module m1
use m
    type container(k3,n2)    ! (4,20)
        integer, kind              :: k3
        integer, len               :: n2
        class(base(:,k3)), pointer :: data(:,:) => null()
    end type

    interface assignment(=)
        module procedure assignArray2Container
    end interface

    contains

    subroutine assignArray2Container (co, b1)
        type (container(4,*)), intent(out) :: co
        class(base(*,4)), intent(in) :: b1(:)

        class (base(:,4)), pointer :: localData(:,:)

        i1 = size(b1)/2

        allocate (localData(i1,2), source=reshape (b1, (/i1,2/)))

        co%data => localData
    end subroutine
end module

program fpAssgn006a4
use m1
    class (base(:,4)), pointer :: b1(:)
    type (container(4,20)) co1

    allocate (b1(12), source=(/(child(20,4,1)(i, &
        name='xlftest '//char(ichar('0')+mod(i,10))), i=1,12)/))

    !! invoke the defined assignment by the next statement
    co1 = b1

    if (.not. associated (co1%data)) error stop 1_4

    if (any(shape(co1%data) /= (/6,2/))) error stop 2_4

    do j = 1, 2
        do i = 1, 6
            call co1%data(i,j)%print()
        end do
    end do

    deallocate (co1%data)
end

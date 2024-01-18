! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qnodeferredlp -qreuse=self /tstdev/F2003/misc/d318769.f
! opt variations: -qck -qnok -qnol -qdefaultpv -qdeferredlp -qreuse=none

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/18/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 318769)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id(10)
        character(n1) :: name
        procedure(printBase), pointer :: print
    end type

    type container(k2,n2)    ! (4,20)
        integer, kind                   :: k2
        integer, len                    :: n2
        class(base(k2,n2)), allocatable :: b(:)
    end type

    contains

    recursive subroutine printBase (b)
        class (base(4,*)), intent(in) :: b

        print *, b%id
        print *, b%name
    end subroutine

    subroutine printCo (co)
        type(container(4,20)), value :: co

        if (allocated(co%b)) then
            print *, lbound(co%b), ubound(co%b)

            do i = lbound(co%b,1), ubound(co%b,1)
                if (associated(co%b(i)%print)) then
                    call co%b(i)%print
                else
                    print *, 'print function not available'
                end if
            end do
        else
            print *, "component is not allocated"
        end if
    end subroutine
end module

module m1
use m
    type, extends(base) :: child(n3,k3)    ! (4,20,20,8)
        integer, kind         :: k3
        integer, len          :: n3
        real(k3), allocatable :: data
    end type

    contains

    recursive subroutine printChild (b)
        class(child(4,*,*,8)), intent(in) :: b

        call b%base%print

        if (allocated(b%data)) then
            write (*, "(f10.2)") b%data
        else
            print *, 'data is not allocated'
        end if
    end subroutine
end module

use m1
    interface
        recursive subroutine printBaseVal (b)
        import
            class (base(4,*)), intent(in) :: b
        end subroutine
    end interface

    real(8), pointer :: d

    integer, allocatable :: i1(:)
    class(base(4,20)), allocatable :: b1(:)

    call printCo(container(4,20)((/(child(4,20,20,8)((/(j*10-10+i, i=10,1,-1)/), &
            'xlftest'//char(ichar('0')+j-1), null(), null(d)), j=1,10)/)))


    allocate (d, source=1.2d0)
    allocate (i1(-1:8), source=(/(i, i=10,1,-1)/))

    allocate (b1(-1:8), source=(/(child(4,20,20,8)(j*10+i1, 'xlftest',&
            printBaseVal, d*j), j=0,9)/))

    call printCo(container(4,20)(b1))
end

recursive subroutine printBaseVal (b)
use m1
    class (base(4,*)), intent(in) :: b

    select type (b)
        type is (base(4,*))
            call printBase(b)

        class is (child(4,*,*,8))
            call printChild(b)

        class default
            stop 10
    end select
end subroutine

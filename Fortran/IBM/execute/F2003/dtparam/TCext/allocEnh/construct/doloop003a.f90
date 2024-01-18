! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qnodefaultpv -qdeferredlp /tstdev/F2003/allocEnh/construct/doloop003a.f
! opt variations: -qnock -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/21/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the intrinsic assignment in a do-loop for
!                               derived type pointer that is bound-remapped by a
!                               data pointer assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      id
    end type

    type, extends(base) :: child    ! (20,4)
        character(:), allocatable :: name
    end type

    contains

    subroutine assgnVal (b1, intVal)
        class(base(:,4)), allocatable, intent(out) :: b1
        integer, intent(in) :: intVal

        allocate(child(20,4) :: b1)

        b1%id = intVal

        select type (x => b1)
            type is (base(*,4))

            type is (child(*,4))
                x%name = 'test ' // achar(65+x%id)

            class default
                stop 21
        end select
    end subroutine
end module

program doloop003a
use m

    type container(k2,n2)    ! (4,20)
        integer, kind                  :: k2
        integer, len                   :: n2
        class(base(:,k2)), allocatable :: data
    end type

    type(container(4,20)), target :: co1(120)

    type(container(4,:)), pointer :: co2(:,:)

    co2 (0:11, 0:4) => co1(::2)

    do i = 0, 11
        do j = 0, 4
            call assgnVal(co2(i,j)%data, i+j)
        end do
    end do

    !! verify result
    index1 = 1

    do j = 0, 4
        do i = 0, 11
            if (.not. allocated(co1(index1)%data)) error stop 1_4

            if (allocated(co1(index1+1)%data)) error stop 2_4

            if (co1(index1)%data%id /= i+j) error stop 3_4

            select type (x => co1(index1)%data)
                type is (child(*,4))
                    if (x%name /= 'test ' // achar(65+i+j)) error stop 4_4
                    if (x%name%len /= 6) error stop 5_4

                class default
                    error stop 6_4
            end select

            index1 = index1 + 2
        end do
    end do
end

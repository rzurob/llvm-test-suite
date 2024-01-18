!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/15/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: parent type name is renamed via use
!                               statement.  3 generations; also test assumed
!                               type parameters.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k, l)
        integer, kind :: k
        integer, len  :: l

        character(len=l) name
        integer(kind=k) id
    end type
end module

module m1
use m, newBase => base
    type, extends(newBase) :: child (k2, l2)
        integer, kind :: k2
        integer, len :: l2

        complex(kind=k2) cx
        real (max(k, k2)) data(min(l, l2):max(l, l2))
    end type

    type (child(k=4, l=20, k2=8, l2=10)) c1_m

    interface isBaseValCorrect
        module procedure isBaseValCorrect4
        module procedure isBaseValCorrect8
    end interface

    interface isChildCorrect
        module procedure isChildCorrect48
        module procedure isChildCorrect88
    end interface

    logical(4) precision_r8, precision_x6

    external precision_r8, precision_x6

    contains

    logical function isBaseValCorrect4 (b, name, id)
        type (newBase(4,*)), intent(in) :: b
        character(*), intent(in) :: name
        integer(4), intent(in) :: id

        isBaseValCorrect4 = (len(b%name) == len(name))
        isBaseValCorrect4 = isBaseValCorrect4 .and. &
                    ((b%name == name) .and. (b%id == id))
    end function

    logical function isBaseValCorrect8 (b, name, id)
        type (newBase(8,*)), intent(in) :: b
        character(*), intent(in) :: name
        integer(8), intent(in) :: id

        isBaseValCorrect8 = (len(b%name) == len(name))
        isBaseValCorrect8 = isBaseValCorrect8 .and. &
                    ((b%name == name) .and. (b%id == id))
    end function

    logical function isChildCorrect48 (c, name, id, cx, data)
        type (child(4, *, 8, *)), intent(in) :: c
        character(*), intent(in) :: name
        integer(4), intent(in) :: id
        complex(8), intent(in) :: cx
        real(8), intent(in) :: data(min(c%l, c%l2):max(c%l, c%l2))

        isChildCorrect48 = isBaseValCorrect(c%newbase, name, id) .and. &
                    precision_x6 (c%cx, cx)

        do i = lbound(data,1), ubound(data, 1)
            isChildCorrect48 = isChildCorrect48 .and. &
                            precision_r8 (c%data(i), data(i))
        end do
    end function

    logical function isChildCorrect88 (c, name, id, cx, data)
        type (child(8, *, 8, *)), intent(in) :: c
        character(*), intent(in) :: name
        integer(8), intent(in) :: id
        complex(8), intent(in) :: cx
        real(8), intent(in) :: data(min(c%l, c%l2):max(c%l, c%l2))

        isChildCorrect88 = isBaseValCorrect(c%newbase, name, id) .and. &
                    precision_x6 (c%cx, cx)

        do i = lbound(data,1), ubound(data, 1)
            isChildCorrect88 = isChildCorrect88 .and. &
                            precision_r8 (c%data(i), data(i))
        end do
    end function
end module

program dtparamExtends026
use m1
    character(20) c1_mName
    real(8) c1_mData(11)

    !! assign values to c1_m
    c1_mName = 'xlftest c1_m'
    c1_mData = (/(i*2.12d0, i=1, 11)/)

    c1_m%newbase = newBase(4, 20)(c1_mName, 100_4)

    c1_m%cx = (4.25d1, 3.21d0)
    c1_m%data = c1_mData

    if (.not. isChildCorrect(c1_m, c1_mName, 100_4, (4.25d1, 3.21d0),&
                            c1_mData))  error stop 1_4

    !! test 3rd generation
    call testExtend
end

subroutine testExtend
use m1, newChild => child
    type, extends(newChild) :: gen3 (k3, l3)
        integer, kind :: k3 = 4
        integer, len :: l3

        logical (k3) :: flag(l3) = .true.
    end type

    type (gen3(k=8,k2=8, k3=8, l=10, l2=10, l3=10)) g3

    character(10) g3Name
    integer(8) g3ID
    complex(8) g3Cx
    real(8) g3Data(1)

    g3Name = 'xlftest g3'
    g3ID = 2_8**33_8
    g3Cx = (1.42d0, 3.4d-1)
    g3Data = 3.43d0

    g3%newchild = newChild (k=8, k2=8, l=10, l2=10) (g3Name, g3ID, g3Cx, g3Data)


    !! verify the results
    if (.not. isChildCorrect (g3%newchild, g3Name, g3ID, g3Cx, g3Data)) &
                        error stop 10_4

    if (.not. all(g3%flag(1:10))) error stop 11_4
end subroutine

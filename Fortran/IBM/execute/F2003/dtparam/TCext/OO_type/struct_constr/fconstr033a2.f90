! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr033a2.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (unlimited
!                               poly-allocatable rank-one array as the
!                               component; use the real data type as the
!                               data-source in structure constructor)
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

program fconstr033a2
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), allocatable :: data(:)
    end type

    class (*), allocatable :: x1(:)
    class (*), pointer :: x2(:)

    real, target :: r1(10)

    allocate (x1(-1:2), source=(/(i*1.1d0, i=-1,2)/))

    x2 => r1

    r1 = (/(j*2.1, j=1,10)/)

    call foo (base(4,20)(x1), base(4,20)(x2))

    contains

!    associate (y1 => base(4,20)(x1), y2 => base(4,20)(x2))
    subroutine foo (y1, y2)
        type(base(4,*)), intent(in) :: y1, y2
        if ((.not. allocated (y1%data)) .or. (.not. allocated(y2%data))) &
                    error stop 1_4

        if ((lbound(y1%data,1) /= -1) .or. (ubound(y1%data,1) /= 2)) error stop 2_4

        if ((lbound(y2%data,1) /= 1) .or. (ubound(y2%data,1) /= 10)) error stop 3_4
        select type (z1 => y1%data)
            type is (double precision)
                write (*, '(d10.2)') z1
            class default
                print *, 'wrong'
        end select

        select type (z2 => y2%data)
            type is (real)
                write (*, '(e11.3)') z2
            class default
                print *, 'wrong'
        end select
    end subroutine
end

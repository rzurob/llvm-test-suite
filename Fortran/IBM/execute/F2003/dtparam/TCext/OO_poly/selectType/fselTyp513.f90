! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/selectType/fselTyp513.f
! opt variations: -qnol

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/31/2005
!*
!*  DESCRIPTION                : select type (IO and private component with
!                               select type)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        private
        integer(k1)      i
    end type

    contains

    subroutine printBase (b)
        class(base(*,4)), intent(in) :: b

        select type (b)
            type is (base(*,4))
                print *, b
        end select
    end subroutine

    subroutine setVal (b, i)
        class (base(*,4)), intent(inout) :: b
        integer, intent(in) :: i

        b%i = i
    end subroutine
end module

program fselTyp513
use m
    type(base(20,4)) :: b1

    call setVal (b1, 100)

    call printBase (b1)
end

! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qreuse=self /tstdev/OO_poly/misc/fmisc001.f
! opt variations: -qck -qnol -qdefaultpv -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items; mostly from defect (defect
!                               283645)
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      id
    end type

    type bigger(n2,k2)    ! (20,4)
        integer, kind     :: k2
        integer, len      :: n2
        type(base(n2,k2)) :: b1
        character(n2)     :: name
    end type
end module

program fmisc001
use m
    interface createData
        function createBase (i)
        use m
            type (base(20,4)) createBase
            integer*4, intent(in) :: i
        end function

        function createBigger (i, c)
        use m
            type (bigger(20,4)) createBigger
            integer*4, intent(in) :: i
            character(*), intent(in) :: c
        end function
    end interface

    print *, createData (10, 'test')
end

function createBase (i)
use m
    type (base(20,4)) createBase
    integer*4, intent(in) :: i

    createBase%id = i
end function

function createBigger (i, c)
use m
    type (bigger(20,4)) createBigger
    integer*4, intent(in) :: i
    character(*), intent(in) :: c

    createBigger%b1%id = i
    createBigger%name = c
end function


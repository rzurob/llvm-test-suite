! GB DTP extension using:
! ftcx_dtp -qck -ql /tstdev/OO_poly/misc/fmisc007a1.f
! opt variations: -qnock -qnol

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fmisc007a1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/29/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (test the type for
!                               associate name)
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
    type base1(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      id
    end type

    type base2(k2,n2)    ! (1,20)
        integer, kind             :: k2
        integer, len              :: n2
        character(kind=k2,len=n2)    name
    end type
end module

program fmisc007a1
use m

    interface makeData
        type (base1(20,4)) function makeBase1 (i)
        use m
            integer(4),intent(in) :: i
        end function

        type (base2(1,20)) function makeBase2 (name)
        use m
            character(*), intent(in) :: name
        end function
    end interface

    associate (x => makeData (1), x1 => makeData('x1'))
        if (x%id /= 1)  error stop 1_4
        if (x1%name /= 'x1') error stop 2_4
    end associate

end

type (base1(20,4)) function makeBase1 (i)
use m
    integer(4),intent(in) :: i

    makeBase1%id = i
end function

type (base2(1,20)) function makeBase2 (name)
use m
    character(*), intent(in) :: name

    makeBase2%name = name
end function

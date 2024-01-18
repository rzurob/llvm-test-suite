! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_poly/misc/fmisc007.f
! opt variations: -qck -qnok -qnol

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fmisc007.f
! %VERIFY: fmisc007.out:fmisc007.vf
! %STDIN:
! %STDOUT: fmisc007.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/22/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items(defect 289042)
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

    type base2(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
        character(n2)    name
    end type
end module

program fmisc007
use m

    interface makeData
        type (base1(20,4)) function makeBase1 (i)
        use m
            integer(4),intent(in) :: i
        end function

        type (base2(4,20)) function makeBase2 (name)
        use m
            character(*), intent(in) :: name
        end function
    end interface

    associate (x => makeData (1), x1 => makeData('x1'))
        print *, x, x1
    end associate

end

type (base1(20,4)) function makeBase1 (i)
use m
    integer(4),intent(in) :: i

    makeBase1%id = i
end function

type (base2(4,20)) function makeBase2 (name)
use m
    character(*), intent(in) :: name

    makeBase2%name = name
end function

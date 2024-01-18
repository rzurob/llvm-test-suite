! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/class/fclass004a.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fclass004a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/01/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : class keyword (intrinsic assignment, RHS can be
!*                               poly-entities; also contains pointer)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        class (*), pointer :: data => null()
        integer(k1)   :: id = 0
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,15)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type
end module

program flcass004a
use m
    type (base(4)) :: b1

    class (base(4)), pointer :: b2

    type (child(4,1,15)), target :: c1

    real*4, target :: r1(2:10)


    c1 = child(4,1,15) (r1(3), 10, 'c1_test')

    b2 => c1

    b1 = b2

    if (b1%id /= 10) error stop 1_4

    if (.not. associated (b1%data, r1(3))) error stop 2_4

end

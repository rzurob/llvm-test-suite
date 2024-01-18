! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/class/fclass004a2.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fclass004a2.f
! %VERIFY: fclass004a2.out:fclass004a2.vf
! %STDIN:
! %STDOUT: fclass004a2.out
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
!*  DESCRIPTION                : class keyword (RHS as polymorphic data in an
!*                               intrinsic assignment; use arrays)
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
        integer(k1)   :: id
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    contains

    character(2) function int2Char (i)
        integer*4, intent(in) :: i

        write (int2Char, '(i2.2)') i
    end function
end module

program fclass004a2
use m
    interface operator (==)
        elemental logical function baseEqInt (b, i)
        use m
            type (base(4)), intent(in) :: b
            integer*4, intent(in) :: i
        end function
    end interface

    class (base(4)), pointer :: b_ptr(:)
    type (child(4,1,20)), target :: c1 (3:12)

    type (base(4)), allocatable :: b1(:)

    allocate (b1(10))

    c1 = (/(child(4,1,20) (i, 'c1_'//int2Char(i)), i=3,12)/)

    b_ptr => c1

    b1 = b_ptr

    print *, b1

    if (.not. all (b1 == (/(i, i=3,12)/))) error stop 1_4

    b1 = b_ptr (10)

    print *, b1

    if (.not. all (b1 == 10)) error stop 2_4
end

elemental logical function baseEqInt (b, i)
use m
    type (base(4)), intent(in) :: b
    integer*4, intent(in) :: i

    baseEqInt = (b%id == i)
end function

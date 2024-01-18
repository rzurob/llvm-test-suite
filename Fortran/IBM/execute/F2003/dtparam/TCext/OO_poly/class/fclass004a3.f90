! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/class/fclass004a3.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fclass004a3.f
! %VERIFY: fclass004a3.out:fclass004a3.vf
! %STDIN:
! %STDOUT: fclass004a3.out
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

program fclass004a3
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
    type (child(4,1,20)), target :: c2 (3:12, 2:11)

    type (base(4)), allocatable :: b1(:)

    allocate (b1(5))

    c1 = (/(child(4,1,20) (i, 'c1_'//int2Char(i)), i=3,12)/)

    b_ptr => c1

    b1 = b_ptr(::2)

    print *, b1

    if (.not. all (b1 == (/(i, i=3,12,2)/))) error stop 1_4

    c2 (8,:) = c1

    b_ptr => c2(8, :)

    b1 = b_ptr(2::2)

    print *, b1

    if (.not. all (b1 == (/(i, i=4,12,2)/))) error stop 2_4
end

elemental logical function baseEqInt (b, i)
use m
    type (base(4)), intent(in) :: b
    integer*4, intent(in) :: i

    baseEqInt = (b%id == i)
end function

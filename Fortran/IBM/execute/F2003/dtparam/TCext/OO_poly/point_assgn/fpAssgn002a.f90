! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/point_assgn/fpAssgn002a.f
! opt variations: -qnol -qnodeferredlp

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn002a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (C717, part2: sequence
!*                               type pointer or pointers of derived types with
!*                               BIND attributes are allowed to assign to
!*                               unlimited poly target)
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
    type seq1(n1,k1,k2)    ! (20,4,2)
        integer, kind :: k1,k2
        integer, len  :: n1
        sequence
        integer(k1)   :: i1
        integer(k2)   :: i2
    end type

    type, bind(c) :: bindType
        real*4 :: x
    end type
end module

program fpAssgn002a
use m

    logical, external :: precision_r4
    class (*), pointer :: x1 => null()
    class (*), pointer :: x2 => null()

    type (seq1(:,4,2)), pointer :: s1_ptr => null()
    type (seq1(20,4,2)), target :: s1_obj = seq1(20,4,2) (i2 = 1, i1 = 10)

    type (bindType), pointer :: b1_ptr => null()
    type (bindType), target :: b1_obj

    b1_obj = bindType (x = 1.0)

    x1 => s1_obj

    !! this way we get the sequence type values back
    s1_ptr => x1

    if (.not. associated (s1_ptr, s1_obj)) error stop 1_4
    if ((s1_ptr%i1 /= 10) .or. (s1_ptr%i2 /= 1)) error stop 2_4


    x2 => b1_obj
    b1_ptr => x2

    if (.not. associated (b1_ptr, b1_obj)) error stop 3_4

    if (.not. precision_r4(b1_ptr%x, 1.0)) error stop 4_4
end

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: implicit001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 10/27/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                : A and MOLD have implicit declared types.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  ===================================================================
!*  REVISION HISTORY
!*                    MM/DD/YY :
!*                        Init :
!*                    Comments :
!*  ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        character(10) :: c
    end type

    type(Base) :: cc
    class(Child), pointer :: bb => null()
    class(*), pointer :: aa => null()
end module

program implicit001
use m
    implicit type(Base) (b), class(Child) (c), class(*) (u)
    class(*), pointer :: mold1 => null()
    allocatable umold2
    allocatable c1
    allocatable c2
    allocatable u1

    if(.NOT. extends_type_of(b1, mold1)) error stop 1_4
    if(.NOT. extends_type_of(c1, umold2)) error stop 2_4
    if(.NOT. extends_type_of(u1, mold1)) error stop 3_4
    if(.NOT. extends_type_of(bb, umold2)) error stop 4_4
    if(.NOT. extends_type_of(cc, mold1)) error stop 5_4
    if(.NOT. extends_type_of(aa, umold2)) error stop 6_4

    if(same_type_as(b1, mold1)) error stop 7_4
    if(same_type_as(c1, umold2)) error stop 8_4
    if(same_type_as(u1, mold1)) error stop 9_4
    if(same_type_as(bb, umold2)) error stop 10_4
    if(same_type_as(cc, mold1)) error stop 11_4
    if(same_type_as(aa, umold2)) error stop 12_4

    allocate(Child::mold1)
    allocate(Child::umold2)
    allocate(Base::u1)
    allocate(Child::aa)

    if(extends_type_of(b2, mold1)) error stop 13_4
    if(.NOT. extends_type_of(c2, mold1)) error stop 14_4
    if(extends_type_of(cc, mold1)) error stop 15_4
    if(.NOT. extends_type_of(bb, mold1)) error stop 16_4
    if(extends_type_of(u1, mold1)) error stop 17_4
    if(.NOT. extends_type_of(aa, mold1)) error stop 18_4

    if(same_type_as(b1, mold1)) error stop 19_4
    if(.NOT. same_type_as(c1, umold2)) error stop 20_4
    if(same_type_as(u1, mold1)) error stop 21_4
    if(.NOT. same_type_as(bb, umold2)) error stop 22_4
    if(same_type_as(cc, mold1)) error stop 23_4
    if(.NOT. same_type_as(aa, umold2)) error stop 24_4
end

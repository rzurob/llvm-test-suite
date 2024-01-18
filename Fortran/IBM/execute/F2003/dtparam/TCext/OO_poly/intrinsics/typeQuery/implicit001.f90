! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp /tstdev/OO_poly/intrinsics/typeQuery/implicit001.f
! opt variations: -qnock -qnol -qnodeferredlp

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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(k2,n2)    ! (20,4,1,10)
        integer, kind             :: k2
        integer, len              :: n2
        character(kind=k2,len=n2) :: c
    end type

    type(Base(20,4)) :: cc
    class(Child(:,4,1,:)), pointer :: bb => null()
    class(*), pointer :: aa => null()
end module

program implicit001
use m
    implicit type(Base(20,4)) (b), class(Child(20,4,1,10)) (c), class(*) (u)
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

    allocate(Child(20,4,1,10)::mold1)
    allocate(Child(20,4,1,10)::umold2)
    allocate(Base(20,4)::u1)
    allocate(Child(20,4,1,10)::aa)

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

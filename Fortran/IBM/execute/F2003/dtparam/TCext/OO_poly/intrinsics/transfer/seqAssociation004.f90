! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp -qreuse=self -qreuse=base /tstdev/OO_poly/intrinsics/transfer/seqAssociation004.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: seqAssociation004.f
! %VERIFY: seqAssociation004.out:seqAssociation004.vf
! %STDIN:
! %STDOUT: seqAssociation004.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 12/29/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    SOURCE or MOLD of TRANSFER is a dummy argument. Dummy argument
!*  is an explicit-shape or assumed-size array. Actual argument is
!*  sequence associated with dummy argument. Actual argument is an
!*  array element. Poly.
!*
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

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j
    end type

    type Base1(k2,n2)    ! (4,20)
        integer, kind     :: k2
        integer, len      :: n2
        integer(k2)          j
        type(Base(n2,k2)) :: k
        integer(k2)          m
    end type

    type, extends(Base1) :: Child1    ! (4,20)
        integer(k2) n
    end type
end module

program seqAssociation004
use m
    type(Base1(4,:)), allocatable :: b1(:,:)

    allocate(b1(4,3), SOURCE=reshape((/(Base1(4,20)(i, Base(20,4)(i+1), i+2), &
     i=3,14)/), (/4,3/)))

    call sub1(b1(3,2))

    contains

    subroutine sub1(arg1)
        class(Base1(4,*)) :: arg1(2,*)

        associate(name1=>transfer(arg1(:,:3), (/Child(20,4)(1,1)/), 9))
            if(.NOT. same_type_as(name1, Child(20,4)(1,1))) error stop 1_4
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        end associate
    end subroutine
end

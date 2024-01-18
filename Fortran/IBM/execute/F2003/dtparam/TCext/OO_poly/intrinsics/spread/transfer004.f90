! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_poly/intrinsics/spread/transfer004.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=base

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: transfer004.f
! %VERIFY: transfer004.out:transfer004.vf
! %STDIN:
! %STDOUT: transfer004.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 01/06/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    Function return of spread is the SOURCE or MOLD of transfer.
!*    Poly and unlimited poly.
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
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(AbstractParent) :: Base(n2,k2)    ! (4,20,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      i
    end type

    type, extends(Base) :: Child(n3,k3)    ! (4,20,20,4,20,4)
        integer, kind :: k3
        integer, len  :: n3
        integer(k3)      j
    end type
end module

program transfer004
use m
    class(AbstractParent(4,:)), pointer :: c1(:,:,:)
    class(*), pointer :: b1(:)

    allocate(c1(2,2,2), SOURCE=reshape((/(Child(4,20,20,4,20,4)(i,i-1),i=101,108)/), &
     (/2,2,2/)))

    associate(name1=>transfer(spread(c1, 4, 2), (/Base(4,20,20,4)(1)/)))
        if(.NOT. same_type_as(name1, Base(4,20,20,4)(1))) error stop 1_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate

    allocate(b1(8), SOURCE=(/(Base(4,20,20,4)(i),i=101,108)/))

    associate(name1=>transfer(spread(b1, 1, 2), &
     spread((/Child(4,20,20,4,20,4)(1,1),Child(4,20,20,4,20,4)(2,2)/), 2, 3)))
        if(.NOT. same_type_as(name1, Child(4,20,20,4,20,4)(1,2))) error stop 2_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate
end

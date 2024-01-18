! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/merge/typeBound003.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: typeBound003.f
! %VERIFY: typeBound003.out:typeBound003.vf
! %STDIN:
! %STDOUT: typeBound003.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 01/25/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    Cross testing type bound.
!*    Unlimited-poly
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
        contains
        procedure :: mergeWith
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4,20)
        integer(k1) j
    end type

    contains

    function mergeWith(this, fs, mask)
        class(AbstractParent(4,*)), intent(in) :: this
        class(AbstractParent(4,*)), intent(in) :: fs
        logical, intent(in) :: mask(:,:)
        class(AbstractParent(4,:)), pointer :: mergeWith(:,:)
        allocate(mergeWith(size(mask,DIM=1),size(mask,DIM=2)), &
         SOURCE=merge(this, fs, mask))
        select type(mergeWith)
            type is (Base(4,*))
                print *, "B", mergeWith
                print *, size(mergeWith)
                print *, shape(mergeWith)
            type is (Child(4,*))
                print *, "C", mergeWith
                print *, size(mergeWith)
                print *, shape(mergeWith)
            class default
                error stop 5_4
        end select
    end function
end module

program typeBound003
use m
    class(*), pointer :: b1
    logical :: m1(3,2)

    m1 = reshape((/.FALSE.,.TRUE.,.FALSE.,.FALSE., &
     .TRUE.,.FALSE./), (/3,2/))

    allocate(b1, SOURCE=Base(4,20)(7))
    select type(b1)
        type is (Base(4,*))
            select type(name1=>b1%mergeWith(Base(4,20)(2), m1))
                type is (Base(4,*))
                    print *, "Base", name1
                    print *, size(name1)
                    print *, shape(name1)
                class default
                    error stop 1_4
            end select
        class default
            error stop 2_4
    end select

    deallocate(b1)
    allocate(b1, SOURCE=Child(4,20)(8,9))
    select type(b1)
        type is (Child(4,*))
            select type(name1=>b1%mergeWith(Child(4,20)(3,4), m1))
                type is (Child(4,*))
                    print *, "Child", name1
                    print *, size(name1)
                    print *, shape(name1)
                class default
                    error stop 3_4
            end select
        class default
            error stop 4_4
    end select
end

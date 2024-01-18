! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: typeBound002.f
! %VERIFY: typeBound002.out:typeBound002.vf
! %STDIN:
! %STDOUT: typeBound002.out
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
!*    Poly
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
    type, abstract :: AbstractParent
        contains
        procedure :: mergeWith
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type

    contains

    subroutine mergeWith(this, fs, mask)
        class(AbstractParent), intent(in) :: this
        class(AbstractParent), intent(in) :: fs
        logical, intent(in) :: mask(:,:)
        select type(name1=>merge(this, fs, mask))
            type is (Base)
                print *, "Base", name1
                print *, size(name1)
                print *, shape(name1)
            type is (Child)
                print *, "Child", name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 1_4
        end select
    end subroutine
end module

program typeBound002
use m
    class(Base), pointer :: b1
    logical :: m1(3,2)

    m1 = reshape((/.FALSE.,.TRUE.,.FALSE.,.FALSE., &
     .TRUE.,.FALSE./), (/3,2/))

    allocate(b1, SOURCE=Base(7))
    call b1%mergeWith(Base(2), m1)

    deallocate(b1)
    allocate(b1, SOURCE=Child(8,9))
    call b1%mergeWith(Child(3,-3), m1)
end

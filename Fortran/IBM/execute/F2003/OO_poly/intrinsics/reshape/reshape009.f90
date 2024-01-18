! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: reshape009.f
! %VERIFY: reshape009.out:reshape009.vf
! %STDIN:
! %STDOUT: reshape009.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 11/20/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    SOURCE is non-poly
!*    Assigned data entity is unlimited poly
!*    PAD and ORDER are specified. PAD has different declared type but
!*      same dynamic type as SOURCE
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
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program reshape009
use m
    type(Child) :: c1(10)
    class(*), allocatable :: b1(:,:)
    class(Base), pointer :: b2(:)

    c1 = (/ (Child(i,i+100), i=1,10) /)
    allocate(b2(2), SOURCE = (/Child(-1,1),Child(-2,2)/))

    allocate(b1(3,5), SOURCE = reshape(c1, (/3,5/), b2, (/2,1/)))

    print *, c1
    select type (b1)
        type is (Child)
            print *, b1
        class default
            error stop 1_4
    end select
end

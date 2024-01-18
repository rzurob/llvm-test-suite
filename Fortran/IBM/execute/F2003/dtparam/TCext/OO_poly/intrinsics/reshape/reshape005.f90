! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_poly/intrinsics/reshape/reshape005.f
! opt variations: -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: reshape005.f
! %VERIFY: reshape005.out:reshape005.vf
! %STDIN:
! %STDOUT: reshape005.out
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
!*    Assigned data entity is poly
!*    PAD and ORDER are specified. PAD has same declared type as SOURCE
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type
end module

program reshape005
use m

    interface assignment (=)
        elemental subroutine ct(c, t)
            use m
            class(Base(4)), intent(out) :: c
            type(Child(4)), intent(in) :: t
        end subroutine
    end interface

    type(Child(4)) :: c1(10)
    class(Base(4)), pointer :: b2(:,:) => null()

    c1 = (/ (Child(4)(i,i+1), i=1,10) /)

    allocate(b2(3,5))
    b2 = reshape(c1, (/3,5/), (/Child(4)(-1,1),Child(4)(-2,2)/), (/2,1/))

    print *, c1
    print *, b2%i
end

elemental subroutine ct(c, t)
    use m
    class(Base(4)), intent(out) :: c
    type(Child(4)), intent(in) :: t
    
    select type ( c )
       type is ( Base(4) )
          c%i = t%i
       type is ( Child(4) )
          c%i = t%i
          c%j = t%j
    end select
end subroutine

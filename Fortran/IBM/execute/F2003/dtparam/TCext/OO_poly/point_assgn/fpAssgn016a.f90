! GB DTP extension using:
! ftcx_dtp -qck -ql -qnodefaultpv -qnodeferredlp -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn016a.f
! opt variations: -qnock -qnol -qdefaultpv -qdeferredlp -qreuse=none

!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn016a.f
! %VERIFY: fpAssgn016a.out:fpAssgn016a.vf
! %STDIN:
! %STDOUT: fpAssgn016a.out
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 04/22/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : data pointer assignment (data pointer
!*                               assignment in the forall construct)
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2)    ! (20,4,1)
        integer, kind             :: k2
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    type (base(20,4)) :: b1_m(10)
    type (child(20,4,1)) :: c1_m(10)

    interface makeData
        pure function makeBasePtr (b)
        import base
            type (base(20,4)), pointer :: makeBasePtr
            type (base(*,4)), intent(in) :: b
        end function

        pure function makeChildPtr (c)
        import child
            type (child(20,4,1)), pointer :: makeChildPtr
            type (child(*,4,1)), intent(in) :: c
        end function
    end interface

    contains

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(*,4,1)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

pure function makeBasePtr (b)
use m, only: base
    type (base(20,4)), pointer :: makeBasePtr
    type (base(*,4)), intent(in) :: b

    allocate (makeBasePtr)

    makeBasePtr%id = b%id
end function

pure function makeChildPtr (c)
use m, only: child
    type (child(20,4,1)), pointer :: makeChildPtr
    type (child(*,4,1)), intent(in) :: c

    allocate (makeChildPtr)

    makeChildPtr = c
end function

module m1
use m
    type container(n2,k3)    ! (20,4)
        integer, kind               :: k3
        integer, len                :: n2
        class(base(n2,k3)), pointer :: data => null()
    end type
end module

program fpAssgn016a
use m1
    type (container(20,4)) :: co1(10)

    b1_m = (/(base(20,4)(i), i=1,10)/)

    c1_m = (/(child(20,4,1)(i+1, 'c1_m_'//char(ichar ('0')+i)), i=0,9)/)

    forall (i=1:10, (mod (i,2) == 0))
        co1(i)%data => makeData (b1_m(i))
    end forall

    forall (i=1:10, (mod (i,2) /= 0))
        co1(i)%data => makeData (c1_m(i))
    end forall

    do i =1, 10
        if (.not. associated (co1(i)%data)) call zzrc(int(i,4_4))

        call co1(i)%data%print
        deallocate (co1(i)%data)
    end do

end

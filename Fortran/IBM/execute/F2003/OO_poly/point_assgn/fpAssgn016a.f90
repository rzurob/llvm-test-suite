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
    type base
        integer*4 :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    type (base) :: b1_m(10)
    type (child) :: c1_m(10)

    interface makeData
        pure function makeBasePtr (b)
        import base
            type (base), pointer :: makeBasePtr
            type (base), intent(in) :: b
        end function

        pure function makeChildPtr (c)
        import child
            type (child), pointer :: makeChildPtr
            type (child), intent(in) :: c
        end function
    end interface

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

pure function makeBasePtr (b)
use m, only: base
    type (base), pointer :: makeBasePtr
    type (base), intent(in) :: b

    allocate (makeBasePtr)

    makeBasePtr%id = b%id
end function

pure function makeChildPtr (c)
use m, only: child
    type (child), pointer :: makeChildPtr
    type (child), intent(in) :: c

    allocate (makeChildPtr)

    makeChildPtr = c
end function

module m1
use m
    type container
        class (base), pointer :: data => null()
    end type
end module

program fpAssgn016a
use m1
    type (container) :: co1(10)

    b1_m = (/(base(i), i=1,10)/)

    c1_m = (/(child(i+1, 'c1_m_'//char(ichar ('0')+i)), i=0,9)/)

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

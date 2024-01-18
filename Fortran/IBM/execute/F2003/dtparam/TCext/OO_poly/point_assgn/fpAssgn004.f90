! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn004.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

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
! %GROUP: fpAssgn004.f
! %VERIFY: fpAssgn004.out:fpAssgn004.vf
! %STDIN:
! %STDOUT: fpAssgn004.out
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
!*  DATE                       : 02/04/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : data pointer assignment (polymorphic pointer
!*                               assigned to expr, where expr refers to a function that
!*                               returns a pointer type compatible with the
!*                               pointer object)
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
    type dataType(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure, pass :: print => printData
    end type

    contains

    subroutine printData (d)
        class (dataType(4,*)), intent(in) :: d

        print *, 'dataType, empty type'
    end subroutine
end module

module m1
use m

    type, extends(dataType) :: mData    ! (4,20)
        integer(k1) :: id

        contains

        procedure, pass :: print => printmData
    end type

    contains

    subroutine printmData (d)
        class (mData(4,*)), intent(in) :: d

        print *, 'id = ', d%id
    end subroutine
end module

program fpAssgn004
use m1

    interface
        function createmData (i)
        use m1
            type (mData(4,:)), pointer :: createmData

            integer*4, intent(in) :: i
        end function

        function assgnmData (i)
        use m1
            type(mData(4,:)), pointer :: assgnmData
            integer*4, intent(in) :: i
        end function
    end interface

    class (dataType(4,:)), pointer :: d_ptr

    d_ptr => createmData(10)

    if (.not. associated(d_ptr)) error stop 1_4

    call d_ptr%print

    deallocate(d_ptr)

    d_ptr => assgnmData (-10)

    call d_ptr%print
end

!! this function calls allocate to create a mData object
function createmData (i)
use m1
    type (mData(4,20)), pointer :: createmData
    integer*4, intent(in) :: i

    allocate (createmData)

    createmData = mData(4,20) (i)
end function


!! this function assgins a value to an static mData object
function assgnmData (i)
use m1
    type (mData(4,20)), pointer :: assgnmData
    integer*4, intent(in) :: i

    type (mData(4,20)), pointer, static :: md1 => null()

    if (.not. associated(md1)) allocate (md1)

    md1%id = i

    assgnmData => md1
end function

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
! %GROUP: fpAssgn004a2.f
! %VERIFY: fpAssgn004a2.out:fpAssgn004a2.vf
! %STDIN:
! %STDOUT: fpAssgn004a2.out
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
!*                               pointer object; function call resolved to
!*                               one specific in generics)
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
    type dataType
        contains

        procedure, pass :: print => printData
    end type

    contains

    subroutine printData (d)
        class (dataType), intent(in) :: d

        print *, 'dataType, empty type'
    end subroutine
end module

module m1
use m

    type, extends(dataType) :: mData
        integer*4 :: id

        contains

        procedure, pass :: print => printmData
    end type

    type, extends(mData) :: nData
        character*20 :: name = 'no-name'

        contains

        procedure, pass :: print => printnData
    end type

    contains

    subroutine printmData (d)
        class (mData), intent(in) :: d

        print *, 'id = ', d%id
    end subroutine

    subroutine printnData (d)
        class (nData), intent(in) :: d

        call printmData(d%mData)

        print *, 'name = ', d%name
    end subroutine
end module

program fpAssgn004a2
use m1

    interface makeData
        function createmData (i)
        use m1
            type (mData), pointer :: createmData

            integer*4, intent(in) :: i
        end function

        function createnData (i, c)
        use m1
            type(nData), pointer :: createnData
            integer*4, intent(in) :: i
            character(*), intent(in) :: c
        end function

        function createDataType()
        use m
            type(dataType), pointer :: createDataType
        end function
    end interface

    class (dataType), pointer :: d_ptr

    d_ptr => makeData ()

    call d_ptr%print

    deallocate (d_ptr)

    d_ptr => makeData (10)

    call d_ptr%print

    deallocate (d_ptr)

    d_ptr => makeData (100, 'd_ptr pointer')
    call d_ptr%print

    deallocate (d_ptr)

end

!! this function calls allocate to create an mData object
function createmData (i)
use m1
    type (mData), pointer :: createmData
    integer*4, intent(in) :: i

    allocate (createmData)

    createmData = mData (i)
end function


!! this function calls allocate to create an nData object
function createnData (i, c)
use m1
    type (nData), pointer :: createnData
    integer*4, intent(in) :: i
    character(*), intent(in) :: c

    allocate (createnData)

    createnData = nData(i, c)
end function


!! this function calls allocate to create a dataType object
function createDataType ()
use m
    type(dataType), pointer :: createDataType

    allocate (createDataType)

end function


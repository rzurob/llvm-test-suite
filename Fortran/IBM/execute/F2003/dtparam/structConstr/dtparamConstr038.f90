!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 03/17/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Data target is a function reference which
!                               returns a procedure pointer.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    abstract interface
        function genReal8 (d1)
            real(8), intent(in) :: d1(:)

            real(8), allocatable :: genReal8(:)
        end function
    end interface

    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
        procedure(genReal8), nopass, pointer :: genData
    end type
end module

program dtparamConstr038
use m
    abstract interface
        function genProcPtr (p)
        use m
            procedure(genReal8) :: p
            procedure(genReal8), pointer :: genProcPtr
        end function
    end interface

    procedure(genProcPtr) convertProc2ProcPtr
    procedure(genReal8) convertArray2Alloc

    type (base(8,:)), allocatable :: b1(:)

    logical(4), external :: precision_r8

    allocate (base(8,128):: b1(2))

    b1(1) = base(8,128)(0.0, convertProc2ProcPtr(convertArray2Alloc))

    b1(2) = base(8,128)((/(i, i=1,128)/), &
        convertProc2ProcPtr(convertArray2Alloc))

    b1(1)%data = b1(1)%genData(log(1.0d0*(/(i, i=1,128)/)))
    b1(2)%data = b1(2)%genData(sqrt(b1(2)%data))

    !! verify
    if ((.not. associated(b1(1)%genData, convertArray2Alloc)) .or. &
        (.not. associated(b1(2)%genData, convertArray2Alloc))) error stop 1_4


    do i = 1, 128
        if (.not. precision_r8(b1(1)%data(i), log(1.0d0*i))) error stop 2_4

        if (.not. precision_r8(b1(2)%data(i), sqrt(1.0d0*i))) error stop 3_4
    end do
end


real(8) function convertArray2Alloc (d1)
    real(8), intent(in) :: d1(:)

    allocatable convertArray2Alloc(:)

    allocate (convertArray2Alloc(size(d1)), source=d1)
end function


function convertProc2ProcPtr (p)
use m
    procedure(genReal8) p
    procedure(genReal8), pointer :: convertProc2ProcPtr

    convertProc2ProcPtr => p
end function

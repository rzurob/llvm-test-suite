! GB DTP extension using:
! ftcx_dtp -qck -qnodeferredlp /tstdev/OO_poly/point_assgn/fpAssgn024a3.f
! opt variations: -qnock -qdeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (poly-pointer as
!*                               function return; the return is array)
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
    type base(k1,n1)    ! (1,15)
        integer, kind             :: k1
        integer, len              :: n1
        character(kind=k1,len=n1) :: name = ''

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child(k2)    ! (1,15,4)
        integer, kind :: k2
        integer(k2)   :: id = 0

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase(b)
        class (base(1,*)), intent(in) :: b

        print *, b%name
    end subroutine

    subroutine printChild (b)
        class (child(1,*,4)), intent(in) :: b

        print *, b%name, b%id
    end subroutine
end module

program fpAssgn024a2
use m
    interface makeData
        function produceBasePtr (c, n)
        use m
            class (base(1,15)), pointer :: produceBasePtr(:)
            character(*), intent(in) :: c
            integer, intent(in) :: n
        end function

        function produceChildPtr (c, i, n)
        use m
            class (base(1,15)), pointer :: produceChildPtr(:)
            character(*), intent(in) :: c
            integer*4, intent(in) :: i
            integer, intent(in) :: n
        end function
    end interface

    type (base(1,15)), pointer :: b1(:)
    class (base(1,15)), pointer :: b2(:)

    b1 => makeData ('b1 test', 4)

    b2 =>  makeData ('b2 test', 2)

    if ((size (b1) /= 4) .or. (size(b2) /= 2)) error stop 1_4

    if (any (b1%name /= 'b1 test')) error stop 2_4

    call b2(1)%print
    call b2(2)%print

    deallocate (b1, b2)

    b1 => makeData ('test2', 10, 3)

    b2 => makeData ('test2', 10, 5)

    if ((size (b1) /= 3) .or. (size(b2) /= 5)) error stop 3_4

    if (any (b1%name /= 'test2')) error stop 4_4

    if (any (b2%name /= 'test2')) error stop 5_4

    call b1(2)%print

    call b2(2)%print

    call b2(3)%print
    deallocate (b2)
end

!! produce an array of the same data
function produceBasePtr (c, n)
use m
    class (base(1,15)), pointer :: produceBasePtr(:)
    character(*), intent(in) :: c
    integer, intent(in) :: n

    allocate (produceBasePtr(n))

    produceBasePtr%name = c
end function

!! produce an array of the same data
function produceChildPtr (c, i, n)
use m
    class (base(1,15)), pointer :: produceChildPtr(:)
    character(*), intent(in) :: c
    integer*4, intent(in) :: i
    integer, intent(in) :: n

    type (child(1,15,4)), pointer :: tmp(:)
    allocate (tmp (n))

    tmp%name = c
    tmp%id = i

    produceChildPtr => tmp
end function

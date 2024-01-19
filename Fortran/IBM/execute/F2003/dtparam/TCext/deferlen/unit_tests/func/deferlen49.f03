! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/deferlen/unit_tests/func/deferlen49.f
! opt variations: -qck -qnok -qnol

!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : Testing the deferred length character
!*                               in function call with polymophic.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id
    end type

    type, extends(base) :: person(k2,n2)    ! (20,4,4,20)
        integer, kind :: k2
        integer, len  :: n2
        character(:), pointer     :: name
        character(:), allocatable :: title
    end type

end module

program deferlen48
use m
    interface
        function getName (b)
        use m
            type (person(*,4,4,*)), intent(in) :: b
            character(:), pointer     :: getName
        end function
    end interface

    interface
        function getTitle (b)
        use m
            type (person(*,4,4,*)), intent(in) :: b
            character(:), allocatable :: getTitle
        end function
    end interface

    type(person(20,4,4,20)) :: p1, p2, p3
    character(:), pointer :: ptr
    character(10), target :: char1, char2

    char1 = 'John'
    ptr => char1
    p1 = person(20,4,4,20) (3, ptr, 'Professor')

    if (p1%name /= 'John')  error stop 1
    if (p1%title /= 'Professor') error stop 2

    ptr => getName(p1)
    if (ptr /= 'John') error stop 3

    ptr = getTitle(p1)
    if (ptr /= 'Professor') error stop 4

    p1%title = 'Engineer'
    ptr = getTitle(p1)

    if (ptr /= 'Engineer') error stop 5

    char1 = 'Henry'
    if (p1%name /= 'Henry')  error stop 6
    if (p1%title /= 'Engineer') error stop 7

    ptr => getName(p1)
    if (ptr /= 'Henry') error stop 8

end

function getName (b)
   use m
   type (person(*,4,4,*)), intent(in) :: b
   character(:), pointer     :: getName
   getName => b%name
end function

function getTitle (b)
   use m
   type (person(*,4,4,*)), intent(in) :: b
   character(:), allocatable :: getTitle
   allocate(getTitle, source = b%title)
   getTitle = b%title
end function

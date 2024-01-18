!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Deferred Character Length
!*
!*  PROGRAMMER                 : James Ren
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  DRIVER STANZA              : xlf90/95
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : Testing the deferred length character
!*                               in function call with polymophic.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base
        integer*4 :: id
    end type

    type, extends(base) :: person
        character(:), pointer     :: name
        character(:), allocatable :: title
    end type

end module

program deferlen48
use m
    interface
        function getName (b)
        use m
            type (person), intent(in) :: b
            character(:), pointer     :: getName
        end function
    end interface

    interface
        function getTitle (b)
        use m
            type (person), intent(in) :: b
            character(:), allocatable :: getTitle
        end function
    end interface

    type(person) :: p1, p2, p3
    character(:), pointer :: ptr
    character(10), target :: char1, char2

    char1 = 'John'
    ptr => char1
    p1 = person (3, ptr, 'Professor')

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
   type (person), intent(in) :: b
   character(:), pointer     :: getName
   getName => b%name
end function

function getTitle (b)
   use m
   type (person), intent(in) :: b
   character(:), allocatable :: getTitle
   allocate(getTitle, source = b%title)
   getTitle = b%title
end function

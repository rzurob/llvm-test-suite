!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Testing the ASSOCIATE related
!*                               with characters with deferred length
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

    type base
        integer*4 id
    end type

    type, extends(base) :: child
        character(:), allocatable :: name
    end type

    type (child), pointer:: b1
    type (child), target :: c1

    c1 = child (10, name = 'Thomas')

    b1 => c1
    associate (iitem => b1%name)
       if (iitem /= 'Thomas') error stop 1
    end associate

    end

!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : Testing the function result which has
!*                               allocatable attributes and is
!*                               character with deferred length.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
        character*3 char
        character*12 :: item

        item = fnc ('1234567890'(3:5))
        if (item /= "345") error stop 1

        item = fnc ('1234567890'(3:5)) // fnc ('1234567890'(5:10))
        if (item /= '345567') error stop 2

        contains

        function fnc (str)
           character(3) str
           character(:), allocatable :: fnc
           allocate(fnc, source=str)
           fnc = str
        end function
        end

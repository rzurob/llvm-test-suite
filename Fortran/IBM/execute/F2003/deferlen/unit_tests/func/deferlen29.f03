!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  REQUIRED COMPILER OPTIONS  : -qfixed
!*
!*  DESCRIPTION                : Testing the function result as
!*                               character with deferred length.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

         interface
            function fnc (n)
              character(:), allocatable :: fnc
            end function
         end interface

         character*9  str

         str = fnc(3) // fnc(6)
         if (str /= '123123456') error stop 1

         str = fnc(2) // fnc(-1) // fnc(6)
         if (str /= '12123456') error stop 2

         str =  fnc(2) // fnc(0) // fnc(7)
         if (str /= '121234567') error stop 3

         end

         function fnc (n)
            character(:), allocatable :: fnc
            allocate(character(n)::fnc)
            fnc(:) = '1234567890'
         end


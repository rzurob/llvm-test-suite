!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  REQUIRED COMPILER OPTIONS  : -qfixed
!*
!*  DESCRIPTION                : Testing the function and its entry result
!*                               as character with deferred length.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

         interface
            function fnc (n)
               character(:), allocatable :: fnc
            end function

            function ent (n)
               character(:), allocatable :: ent
            end function
         end interface

         character*9  str


         str = fnc(6)
         if (str /= '123456') error stop 1

         str = ent(6)
         if (str /= '123456') error stop 2

         str = fnc(2)//fnc(3)//fnc(4)
         if (str /= '121231234') error stop 3

         str = ent(2)//ent(3)//ent(4)
         if (str /= '121231234') error stop 4

         str = ent(1)//fnc(3)//ent(5)
         if (str /= '112312345') error stop 5

         end

          function fnc (n)
            character(:), allocatable :: fnc, ent
               allocate (character(n)::fnc)
               fnc(:) = '12345678'
            return
            entry ent (n)
               allocate (character(n)::ent)
               ent(:) = '12345678'
         end

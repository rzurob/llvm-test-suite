!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : Testing the user defined operator on
!*                               characters with deferred length.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

interface operator(+)
      function add(char1, char2)
         character(:), intent(in),  allocatable :: char1, char2
         character(len(char1) + len(char2)) add
      end function
end interface

character(:), allocatable :: char1, char2
character(10) result
allocate(character(4)::char1)
allocate(character(6)::char2)

char1 = '1234'
char2 = 'abcdef'

result = char1 + char2
if (result /= '1234abcdef') error stop 1
deallocate (char1, char2)

end

function add(char1, char2)

   character(:), allocatable :: char1, char2
   character(len(char1) + len(char2)) add

   add = char1//char2

end function



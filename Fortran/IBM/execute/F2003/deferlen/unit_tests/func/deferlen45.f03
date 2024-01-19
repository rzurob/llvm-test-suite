!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Testing the function result which is
!*                               a character with deferred length and
!*                               pointer attribute.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

 implicit none

 character(:), pointer :: char
 character(4) dd

 allocate (character(4)::char)
 char = '1234'

 dd =  fun(char)
 associate (item =>dd)
 if (item /= '1234') error stop 1
 end associate

 associate (item =>dd//'5678')
 if (item /= '12345678') error stop 2
 end associate

 contains
 function fun(ch)
    character(:), pointer :: ch
    character(:), pointer ::fun
    fun => ch
 end function
 end

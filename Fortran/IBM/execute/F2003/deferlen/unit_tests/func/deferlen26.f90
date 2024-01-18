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
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Testing the function result which is
!*                               a character with deferred length
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
interface
  function fun(x)
    character(*) x
    character(:), allocatable ::fun
  end function
end interface
  
  character (12) char , char1
  char = "Hello World!"
  char1 = fun(char)

  if (char1 /= "Hello World!") error stop 1

end

function fun(x)
    character(*) x
    character(:), allocatable ::fun
    allocate(character(len(x))::fun)
    fun = x
end function

!*  ===================================================================
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
interface
  function fun(x)
    character(*) x
    character(:), allocatable ::fun
  end function
end interface

  character (12) char , char1
  char = "Hello World!"
  char1 = fun(char)
  associate (iitem => fun(char))
     if (iitem /= 'Hello World!') error stop 1
  end associate

end

function fun(x)
    character(*) x
    character(:), allocatable ::fun
    allocate(character(len(x))::fun)
    fun = x
end function

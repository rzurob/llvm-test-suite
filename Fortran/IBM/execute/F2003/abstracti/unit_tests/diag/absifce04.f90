!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : The BIND(C) attribute with a NAME=
!*                               specifier shall not be specified in
!*                               the procedure statement of an interface
!*                               body for an abstract interface.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
abstract interface  ! interface with overloading
    function f1a()   bind(c, name='f1a_B')  ! Error, name= isn't allowed
    end function
end interface
end

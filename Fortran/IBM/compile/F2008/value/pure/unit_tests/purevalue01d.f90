!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME  : F2008/value/pure/unit_tests/purevalue01d.f
!*  DATE            : 2010-12-01
!*  DRIVER STANZA   : xlf2003
!*
!*  DESCRIPTION
!*  - use value attribute in dummy args of a defined operator
!*
!234567890123456789012345678901234567890123456789012345678901234567890123456789

interface operator (.def_op.)
  function foo (a)
    integer, value :: a
    integer :: foo
  end function foo
end interface

interface operator (.def_op.)
  function moo (a)
    integer :: foo
    real, value :: a
  end function moo

  integer function boo (a)
    complex, value :: a
  end function
end interface

end

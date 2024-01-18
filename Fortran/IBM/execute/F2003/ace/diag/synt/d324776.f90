!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : d324776
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-10-18
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : diagnostic - interface name used in AC
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : interface
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Procedures can be called in AC's via interface names, but the interface name
!*  should not be used without arguments in AC, just as procedure pointers are
!*  not allowed.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program d324776

  implicit none
  interface interfaceFunc
     integer function anotherFunc(a)
       integer :: a
     end function anotherFunc
  end interface

  integer :: array(2)

  array = (/ interfaceFunc, interfaceFunc /)
  array = (/ 1, interfaceFunc /)
  array = (/ interfaceFunc, 1 /)
  array = (/ 1.1, interfaceFunc /)
  array = (/ .true., interfaceFunc /)
  print *, (/ interfaceFunc /)

end program d324776

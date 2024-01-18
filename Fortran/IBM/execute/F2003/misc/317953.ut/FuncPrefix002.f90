!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : FuncPrefix002.f
!*
!*  DATE                       : Feb 09,2007
!*
!*  PRIMARY FUNCTIONS TESTED   : function prefix ICE
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Defect Number 317953
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  ICE caused by the function prefix containing a type component that is a specification
!*  expression. This test case covers class pointer.

!234567890123456789012345678901234567890123456789012345678901234567890

  module m
      type :: child! (l)
          integer, private :: l = 20
          character(20), private :: name = 'default_pointer'
      end type

      contains

      character(c%l) function getName (c)
          class(child),pointer :: c
          getName = c%name
      end function
  end module

  program t
      use m
      class(child),pointer :: e
      allocate(e)
      print *,'getName=',getName(e)
  end



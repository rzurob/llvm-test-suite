!*********************************************************************
!*  ===================================================================
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
!*  expression.

!234567890123456789012345678901234567890123456789012345678901234567890

  module m
      type :: child! (l)
          integer, private :: l = 20
          character(20), private :: name = 'default'
      end type

      contains

      character(c%l) function getName (c)
          class(child), intent(in) :: c
          getName = c%name
      end function
  end module

  program t
      use m
      type(child) :: e
      print *,'getNamem=',getName(e)
      print *,'length=',len(getName(e))
  end


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
!*  expression. This test case covers nested derived type array
!*  component

!234567890123456789012345678901234567890123456789012345678901234567890

  module m
      type :: child! (l)
          integer,private :: l(3) = (/20,30,40/)
          character(20), private :: name = 'default_nested_array'
      end type
      type :: ch
          type(child) :: d
      end type
      contains

      character(c%d%l(2)) function getName (c)
          class(ch),intent(in) :: c
          getName = c%d%name
      end function
  end module

  program t
      use m
      type(ch),allocatable :: e
      allocate(e)
      print *,'getNamem=',getName(e)
      print *,'length=',len(getName(e))
  end


!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : FuncPrefix003.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Zheming Gu
!*  DATE                       : Feb 09,2007
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : function prefix ICE
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Defect Number 317953
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  ICE caused by the function prefix containing a type component that is a specification
!*  expression. This test case covers class allocatable attribute.

!234567890123456789012345678901234567890123456789012345678901234567890

  module m
      type :: child! (l)
          integer,private :: l = 20

          character(20), private :: name = 'default_allocatable'
      end type

      contains

      character(c%l) function getName (c)
          class(child),allocatable :: c
          getName = c%name
      end function
  end module

  program t
      use m
      class(child),allocatable :: e
      allocate(e)
      print *,'getNamem=',getName(e)
      print *,'length=',len(getName(e))
  end



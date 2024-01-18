! GB DTP extension using:
! ftcx_dtp -qnodeferredlp /tstdev/F2003/misc/317953.ut/FuncPrefix002.f
! opt variations: -qck -qdeferredlp

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
!*  expression. This test case covers class pointer.

!234567890123456789012345678901234567890123456789012345678901234567890

  module m
      type :: child(k1,n1)! (l)    ! (4,20)
          integer, kind          :: k1
          integer, len           :: n1
          integer(k1), private   :: l = 20
          character(n1), private :: name = 'default_pointer'
      end type

      contains

      character(c%l) function getName (c)
          class(child(4,*)),pointer :: c
          getName = c%name
      end function
  end module

  program t
      use m
      class(child(4,20)),pointer :: e
      allocate(e)
      print *,'getName=',getName(e)
  end



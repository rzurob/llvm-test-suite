! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/F2003/misc/317953.ut/FuncPrefix006.f
! opt variations: -qck -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

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
      type :: child(k1,n1)! (l)    ! (4,20)
          integer, kind          :: k1
          integer, len           :: n1
          integer(k1),private    :: l(3) = (/20,30,40/)
          character(n1), private :: name = 'default_nested_array'
      end type
      type :: ch(k2,n2)    ! (4,20)
          integer, kind      :: k2
          integer, len       :: n2
          type(child(k2,n2)) :: d
      end type
      contains

      character(c%d%l(2)) function getName (c)
          class(ch(4,*)),intent(in) :: c
          getName = c%d%name
      end function
  end module

  program t
      use m
      type(ch(4,:)),allocatable :: e
      allocate(ch(4,20)::e)
      print *,'getNamem=',getName(e)
      print *,'length=',len(getName(e))
  end


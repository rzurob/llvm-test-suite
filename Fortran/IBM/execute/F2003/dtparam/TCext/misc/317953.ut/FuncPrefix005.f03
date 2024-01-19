! GB DTP extension using:
! ftcx_dtp -qck -qk -qnol -qnodefaultpv -qnodeferredlp -qreuse=self /tstdev/F2003/misc/317953.ut/FuncPrefix005.f
! opt variations: -qnock -qnok -ql -qdefaultpv -qdeferredlp -qreuse=none

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
!*  expression. This test case covers nested derived type

!234567890123456789012345678901234567890123456789012345678901234567890

  module m
      type :: child(k1,k2,n1)! (l)    ! (4,1,20)
          integer, kind                      :: k1,k2
          integer, len                       :: n1
          integer(k1),private                :: l = 20
          character(kind=k2,len=n1), private :: name = 'default_nested'
      end type
      type :: ch(k3,k4,n2)    ! (4,1,20)
          integer, kind         :: k3,k4
          integer, len          :: n2
          type(child(k3,k4,n2)) :: d
      end type
      contains

      character(c%d%l) function getName (c)
          class(ch(4,1,*)),intent(in) :: c
          getName = c%d%name
      end function
  end module

  program t
      use m
      type(ch(4,1,20)),allocatable :: e
      allocate(e)
      print *,'getNamem=',getName(e)
      print *,'length=',len(getName(e))
  end


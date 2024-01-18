! GB DTP extension using:
! ftcx_dtp /tstdev/F2003/misc/317953.ut/FuncPrefix001.f
! opt variations: -qck

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : FuncPrefix001.f
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
!*  expression. 

!234567890123456789012345678901234567890123456789012345678901234567890

  module m
      type :: child(k1,n1)! (l)    ! (4,20)
          integer, kind          :: k1
          integer, len           :: n1
          integer(k1), private   :: l = 20
          character(n1), private :: name = 'default'
      end type

      contains

      character(c%l) function getName (c)
          class(child(4,*)), intent(in) :: c
          getName = c%name
      end function
  end module

  program t
      use m
      type(child(4,20)) :: e
      print *,'getNamem=',getName(e)
      print *,'length=',len(getName(e))
  end



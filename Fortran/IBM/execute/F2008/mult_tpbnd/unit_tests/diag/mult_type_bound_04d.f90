! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : mult_type_bound_diag4.f 
!*
!*  PROGRAMMER                 : Paul Liu
!*  DATE                       : 2011-05-20
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : no module
!*                               redefinition
!*                               procedure,nopass :: foo => real_foo1, foo => real_foo2 ! 2 foo

!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      program t

        type point
          contains
          procedure,nopass :: foo => real_foo1, foo => real_foo2 ! 2 foo
        end type point

      end program t
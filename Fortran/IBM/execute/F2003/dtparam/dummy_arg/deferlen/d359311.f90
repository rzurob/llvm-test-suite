!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d359311.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Nov. 24 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :  
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  DEFECT 359311 
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(l1)
      integer,len   :: l1=3
      character(l1) :: c="xlf" 
   end type
end module

program d359311
  use m
  implicit none

  type(dtp(3)) :: dt
  contains

    subroutine sub(dt)
      type(dtp(*)),intent(in) :: dt 
        print *,dt%c//dt%c    
    end subroutine
end

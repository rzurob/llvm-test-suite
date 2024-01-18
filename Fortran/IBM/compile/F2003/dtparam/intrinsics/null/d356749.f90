!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d356749.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Sept. 25 2008 
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
!*
!* 1. DEFECT 356749 
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type any(l)  
    integer,len :: l
    character(3),allocatable :: c1
    character(3),pointer     :: c2
  end type
end module

program d356749
   use m
   implicit none

   type(any(3)),target :: any1=any(3)(null("wrong),null("wrong))
  
end program


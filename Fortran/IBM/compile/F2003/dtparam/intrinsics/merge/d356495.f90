!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d356495.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Sept. 20 2008 
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
!* 1. DEFECT 356495 
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type :: dtp(k,l)
     integer,kind   :: k=2
     integer,len    :: l=4
      integer(k)    :: i(2)=1
      character(l)  :: c(2)="xlf"
   end type
end module

program d356495
   use m
   implicit none

   type(dtp(2,4)),parameter :: dtp2(2)=[dtp(2,4)(i=[-1,1],c=["c","d"] ), &
               dtp(2,4)(i=[1,2],c(2)=["a","b"]) ]  !<===syntax error at c(2) 

end program


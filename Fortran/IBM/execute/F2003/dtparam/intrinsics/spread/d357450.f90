!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d357450.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Oct. 15 2008 
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
!*  1. DEFECT 357450
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dtp(l)
    integer(2),len :: l
    character(l),allocatable :: c1
    type(dtp(l)),pointer :: dtp1=>null()
  end type
  type container
      type(dtp(:)),allocatable   :: dtp2 
   end type
end module
program d357450
  use m
  implicit none

  type(container) :: contain1
  type(dtp(2)),target :: tar1
  tar1 = dtp(2)("ab",null())

  contain1%dtp2=dtp(2)("cd",tar1)

  if(contain1%dtp2%l /= 2)                  stop 1
  if(contain1%dtp2%c1 /= "cd")              stop 2
  if(.not. associated(contain1%dtp2%dtp1))  stop 3
  if(contain1%dtp2%dtp1%c1 /= "ab")         stop 4

end program



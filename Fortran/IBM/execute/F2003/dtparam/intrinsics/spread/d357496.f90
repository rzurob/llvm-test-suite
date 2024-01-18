!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d357496.f   
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
!*  1. DEFECT 357496
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type first(l1)
    integer,len :: l1
    character(l1) :: c1
  end type
   type container(l)
      integer,len :: l
      type(first(l))  :: first2(l)
   end type
end module

program d357496
  use m
  implicit none

  type(container(2)),target  :: contain1
  contain1=container(2)( [first(2)(c1="123"),first(2)(c1="456")])
  call verify(spread(contain1,1,3))
  contains

     subroutine verify(dt)
         type(container(*)),intent(in) :: dt(:)
         integer :: i
         print *,lbound(dt,1),ubound(dt,1)
         do i=lbound(dt,1),ubound(dt,1)
            print *,dt(i)%first2(1)%c1,"|",dt(i)%first2(2)%c1
            print *,(dt(i)%first2(1)%c1 /= "12"),(dt(i)%first2(2)%c1 /= "45")
         end do
     end subroutine

end program


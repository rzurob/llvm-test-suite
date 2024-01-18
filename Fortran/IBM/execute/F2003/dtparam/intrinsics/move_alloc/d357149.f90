!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d357149.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Oct. 6 2008 
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
!*  1. DEFECT 357149
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
      integer,len  :: l1
      integer :: i1(l1) 
   end type
   type,extends(base) :: child(l2)
      integer,len    :: l2
      integer :: i2(l1:l2)
   end type
end module

program d357149
  use m
  implicit none

  integer :: i

  class(base(3)),allocatable:: from1(:) 

  allocate(child(3,5) :: from1(3:6))
  select type(from1)
    type is(child(*,*))
       do i=3,6
          from1(i)%i1=[1,2,3]
          from1(i)%i2=[3,4,5]
          print *,lbound(from1(i)%i1,1),ubound(from1(i)%i1,1)
          print *,lbound(from1(i)%i2,1),ubound(from1(i)%i2,1)
          print *,from1(i)%i1,i 
          print *,from1(i)%i2,i
       end do
    class default
       error stop 100_4
  end select

end program


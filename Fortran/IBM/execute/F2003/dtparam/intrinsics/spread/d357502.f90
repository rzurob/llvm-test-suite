!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d357502.f   
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
!*  1. DEFECT 357502
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type first(l1)
    integer,len :: l1
    character(l1) :: c1
  end type
  type second(l2)
    integer,len :: l2
    type(first(l2)) :: first1
  end type

   type container(l1,l2)
      integer,len :: l1,l2
      type(first(l1))  :: first2(l1)
      type(second(l2)) :: second2(l1:l2)
   end type
end module

program d357502
  use m
  implicit none

  type(container(2,3))  :: contain1

  contain1=container(2,3)( &
           first2=[first(2)(c1="12"),first(2)(c1="45")], &
          second2=[second(3)(first1=first(3)(c1="aaa") ),&
                   second(3)(first1=first(3)(c1="bbb") ) ]  )

  call verify(spread(contain1,1,3))

  contains
     subroutine verify(dt)
         type(container(*,*)),intent(in) :: dt(:)
         integer :: i

         do i=lbound(dt,1),ubound(dt,1)
            print *,dt(i)%first2(1)%c1
            print *,dt(i)%first2(2)%c1 
            print *,dt(i)%second2(2)%first1%c1
            print *,dt(i)%second2(3)%first1%c1
         end do
     end subroutine

end program



!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d361009.f
!*
!*  DATE                       : Jan. 16 2009
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type A(l)
     integer,len  :: l
     integer :: i
  end type
  contains
      subroutine sub(arg)
         type(A(*)),intent(in) :: arg(:)
         print *,lbound(arg,1),ubound(arg,1)
         do i=1,2
           print *,arg(i)%i
         end do
         print *,arg
      end subroutine
end module

program d361009

  use m
  type(A(1)) :: a1(-1:0)
  a1%i=-99
  call sub(a1)

end program

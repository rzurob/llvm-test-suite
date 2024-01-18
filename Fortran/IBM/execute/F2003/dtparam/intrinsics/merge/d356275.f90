!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 17 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. DEFECT 356275
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A(l)
      integer,len  :: l
      character(l) :: c1
   end type
   type,extends(A) :: B(m)
      integer,len  :: m
      character(m) :: c2
   end type
end module

program d356275
   use m
   implicit none

   class(A(:)),pointer    :: poly1=>null()
   type(B(3,4)),target    :: b1=B(3,4)(c1="123",c2="456")
   poly1=>b1

   call sub1([poly1,poly1])
    contains
       subroutine sub1(dt)
          class(A(*)),intent(in) :: dt(:)
          select type(y=>dt)
             type is(B(*,*))
                if(y%l /= 3)                              error stop 11_4
                if(y%m /= 4)                              error stop 12_4
                if(any(y%c1 /= "123"))                    error stop 13_4
                if(any(y%c2 /= "456"))                    error stop 14_4
             class default
                error stop 100_4
          end select
       end subroutine
end program



!*  ===================================================================
!*
!*  DATE                       : Sept. 22 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : INTRINSICS(MERGE)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 13.7.75
!* 2. INTRINSICS:MERGE(TSOURCE,FSOURCE,MASK)
!* 3. DERIVED TYPE HAS PROCEDURE POINTER COMPONENT
!* 4. MERGE AS ARGUMENT OF PROCEDURE
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k,l)
     integer,kind :: k
     integer,len  :: l
     integer(k)   :: i
     character(l) :: ch
     procedure(sub1),pointer, nopass :: proc=>null()
   end type

   contains

      subroutine sub1(dt)
         type(dtp(4,*)),intent(in) :: dt
         print *,dt%k,dt%l
         print *,dt%i,dt%ch
      end subroutine

    function func1()
        procedure(sub1), pointer :: func1
        func1 => sub1
    end function

end module

program mergeProcPointerComp01
   use m
   implicit none

    integer :: k

    type(dtp(4,3)) :: dtp1(3)= [( dtp(4,3)(i=k,ch=char(k+64)),k=1,3 )]

    type(dtp(4,3)) :: dtp2(3)= [( dtp(4,3)(i=-k,ch=char(k+96)),k=1,3 )]

    do k=1,3

      dtp1(k)%proc => func1()

      call dtp1(k)%proc(merge(dtp1(k),dtp2(k),.true.))

      call dtp1(k)%proc(merge(dtp1(k),dtp2(k),.false.))

    end do
end program

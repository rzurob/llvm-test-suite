!*********************************************************************
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
!* 3. DEFECT 356524
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(k1,l1)
     integer,kind :: k1
     integer,len  :: l1

     integer(k1)   :: i1
     character(l1) :: c1
  end type

  type,extends(base) :: child(k2,l2)
     integer(2),kind :: k2
     integer(2),len  :: l2

     integer(k2) :: i2
     character(l2) :: c2
  end type
end module

program d356524

   use m
   implicit none

   integer :: k

   class(base(4,:)),allocatable :: poly1(:),poly5(:)

   type(child(4,3,2,6))  :: chld1(6)

   chld1=[( (child(4,3,2,6)(i1=k,c1=char(k+64),i2=k+10,c2=char(k+96))),k=1,6 )]

   allocate(poly1(6),source=chld1)

   allocate(poly5(6),source=merge(poly1,poly1,.true.))

   select type(poly5)
      type is(child(4,*,2,*))
        print *,poly5%i1
        print *,"|",poly5%c1,"|"
      class default
        error stop 10_4
   end select

end program


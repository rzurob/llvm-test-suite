!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : listDirectLogicalCompWrite01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Jan. 8 2009 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : LIST-DIRECTED INTRINSIC IO 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!* 1. Derived type has multiple layers of nested DT components, ultimate components are logical, and derived type has sequence statement
!* 2. Test write statement , execute write statement inside recursive function
!* 3. Test write statement when object is in common block
!234567890123456789012345678901234567890123456789012345678901234567890
module m1
   type inner1(k1,l1)
       integer,kind :: k1
       integer,len  :: l1 ! l1=3
       sequence       
       logical(k1)  :: g1(l1)
       logical(k1)  :: g2 
   end type
end module

module m2
  use m1
  
  type inner2(k2,l2)
     integer,kind :: k2
     integer,len  :: l2 ! l2=2
     sequence
     type(inner1(k2,l2+1)) :: inn1
  end type 

  type inner3(k3,l3)
     integer,kind :: k3
     integer,len  :: l3 ! l3=2
     sequence
     type(inner2(k3,l3))   :: inn2
  end type 

  type outer(k4,l4)
     integer,kind :: k4
     integer,len  :: l4 !l4=3
     sequence
     type(inner3(k4,l4-1)) :: inn3
  end type

  contains

   recursive subroutine writeDT(dt,i)
     type(outer(8,*)),intent(in) :: dt(:)

     if(i <= size(dt,1)) then
        associate(x=>modFun(dt(i)) )
           write(*,*) x
        end associate
     else
        return
     end if 

     ! call recursively until write all elements
     call writeDT(dt,i+1)

   end subroutine

   function modFun(dt)
     type(outer(8,*)),intent(in) :: dt
     type(outer(8,:)),allocatable :: modFun
    
     allocate(modFun,source=dt)
     modFun%inn3%inn2%inn1%g1=.not. dt%inn3%inn2%inn1%g1
     modFun%inn3%inn2%inn1%g2=.not. dt%inn3%inn2%inn1%g2
   end function
end module


program listDirectLogicalCompWrite01
  use m2
  implicit none

  integer :: i

  type(outer(8,3)) :: out
  type(outer(8,3)),target :: tar(6:9)
  type(outer(8,:)),pointer :: ptr(:)
  common /com/ out

  do i=lbound(tar,1),ubound(tar,1)
      tar(i)%inn3%inn2%inn1%g1=[.true.,.true.,.false.]
      tar(i)%inn3%inn2%inn1%g2=.false.
  end do

  ptr=>tar

  call writeDT(ptr,1) 

  out=tar(6)

  write(*,*) out

end program

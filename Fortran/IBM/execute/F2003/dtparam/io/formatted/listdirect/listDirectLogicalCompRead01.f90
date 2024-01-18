!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : listDirectLogicalCompRead01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Jan. 14 2009 
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
!* 1. Test Read statement with logical ultimate components 
!* 2. Use implicit statement
!* 3, Use different input for logical data
!234567890123456789012345678901234567890123456789012345678901234567890

module m1
  type log1(k1,l1)
    integer,kind :: k1 ! k1=8
    integer,len  :: l1 ! l1=5
    sequence
    logical(k1) :: g1(l1-1)
  end type

end module

module m2
  use m1
  
  type log2(k2,l2)
     integer,kind :: k2 ! k2=4
     integer,len  :: l2 ! l2=6
     sequence 
     logical(k2) :: g2
     logical(k2) :: g3(l2-1)
     type(log1(k2+k2,l2-1)) :: lcomp1  
  end type

  type log3(k3,l3)
     integer,kind :: k3 ! k3=2
     integer,len  :: l3 ! l3=3

     type(log2(k3,l3)) :: lcomp2
  end type

  contains
 
    function retrieveData(dt,unit)
      type(log2(4,:)), allocatable :: dt(:)
      type(log3(4,:)), allocatable :: retrieveData(:)
      integer unit

      allocate(log3(4, dt%l2) :: retrieveData(2))

      call read(dt,unit)
 
      retrieveData%lcomp2 = dt

    end function 

    subroutine read(bt,unit)
      type(log2(4,:)), allocatable :: bt(:)
      integer :: unit

      read(unit,*)  bt(0) 
      read(unit,*,decimal='comma')  bt(1)

    end subroutine
end module


program listDirectLogicalCompRead01
  use m2
  implicit none

  call sub  
  
end program

subroutine sub
   use m2 

   integer :: i
   character(256) :: msg

   type(log3(4,:)), allocatable :: tt(:)
   type(log2(4,:)), allocatable :: ss(:)

   allocate( log2(4,6) :: ss(0:1))

   open(10,file='listDirectLogicalCompRead01.dat', iostat=ios,iomsg=msg)

   if(ios <> 0) then
      print *,"fail to open the file"
      print *,"iostat=",ios
      print *,"iomsg=",msg
      stop 10
   end if 

   ! initialize derived type
   do i=lbound(ss,1),ubound(ss,1)
       ss(i)%lcomp1%g1=.false.
       ss(i)%g2=.false.
       ss(i)%g3=.false.
   end do

   ! following is the input logical value we want to read

   !.true. .false.,true,free
   !T,F, .truth. , .fffff., FalseFalse TrueTrue
   !2*.true  ; 1*; 1*.too fog.
   !1*f ; 1*T ft 2*truth ; 100*Fall /note:100*Fall are ignored

   tt=retrieveData(ss,10) 

   ! output results for verification
   do i=lbound(tt,1),ubound(tt,1)
       write(*,*) tt(i)%lcomp2%g2
       write(*,*) tt(i)%lcomp2%g3
       write(*,*) tt(i)%lcomp2%lcomp1%g1 
   end do 

   close(10,status='keep')  

end subroutine

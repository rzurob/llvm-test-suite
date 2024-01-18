!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : listDirectStreamAccess01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Jan. 20 2009 
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
!*  1. Test read with list directed IO and stream access
!*  2. Derived type has character , integer, and logical ultimate components
!234567490123456749012345674901234567490123456749012345674901234567490
module m1
  type DT0(k0,l0)
     integer,kind :: k0 ! k0=4
     integer,len  :: l0 ! l0=3

     sequence
     integer(k0)  :: i1=-99 
     integer(2*k0) :: i2(l0+1)=-99
  end type
  type DT1(k1,l1)
     integer,kind :: k1 ! k1=4
     integer,len  :: l1 ! l1=4

     sequence
     character(k1) :: c1(l1-1)="****"
     character(l1+1) :: c2="*****"
     type(DT0(k1,l1-1)) :: dt0comp
  end type 
end module

module m2
  use m1
  type DT2(k2,l2)
     integer,kind :: k2 ! k2=2
     integer,len  :: l2 ! l2=4

     sequence
     logical(k2)  :: g1=.false.
     logical(2*k2) :: g2(l2)=.false.
     type(DT1(2*k2,l2)) :: dt1comp
  end type

  contains

    subroutine read1(dt2,unit) 
       implicit type(DT2(2,4)) (d)
       allocatable :: dt2(:)
       intent(inout) :: dt2
       integer :: unit

       if(.not. allocated(dt2)) then
          allocate(DT2(2,4) :: dt2(-1:0))
       end if

       call read2(dt2(-1),unit) 
       call read3(dt2(0),unit) 

    end subroutine 

    subroutine read2(dt2,unit)
       implicit type(DT2(2,*)) (d)
       intent(inout) :: dt2
       integer :: mypos,unit

       inquire(unit,pos=mypos)
       if(mypos /= 1)      stop 11

       read(unit,*,pos=mypos) dt2%g1,dt2%g2(1)
       inquire(unit,pos=mypos)
       if(mypos /= 20)     stop 12
     
       read(unit,*,pos=mypos) dt2%g2(2:4)
       inquire(unit,pos=mypos) 
       if( mypos /= 38)     stop 13

       read(unit,*,pos=mypos) dt2%dt1comp%c1
       inquire(unit,pos=mypos) 
       if( mypos /= 58)      stop 14 

       read(unit,*,pos=mypos) dt2%dt1comp%c2
       inquire(unit,pos=mypos) 
       if(mypos /= 68)       stop 15

       read(unit,*,pos=mypos) dt2%dt1comp%dt0comp%i1,dt2%dt1comp%dt0comp%i2(1)
       inquire(unit,pos=mypos)
       if(mypos /= 83)       stop 16
       
       read(unit,*) dt2%dt1comp%dt0comp%i2(2:4)

    end subroutine

    subroutine read3(dt2,unit)
       implicit type(DT2(2,4)) (d)
       intent(inout) :: dt2
       integer :: unit,mypos

       inquire(unit,pos=mypos)
       if(mypos /= 93)     stop 17

       read(unit,*,pos=mypos,decimal='comma') dt2%g1,dt2%g2
       inquire(unit,pos=mypos)
       if(mypos /= 118)    stop 18

       read(unit,*,pos=mypos,decimal='point') dt2%dt1comp%c1, dt2%dt1comp%c2
       inquire(unit,pos=mypos)
       if(mypos /= 152)    stop 19

       read(unit,*) dt2%dt1comp%dt0comp%i1
       read(unit,*,decimal='comma') dt2%dt1comp%dt0comp%i2

    end subroutine

end module

program listDirectStreamAccess01
use m2

   integer :: ios
   character(256) :: msg

   implicit type(DT2(2,4)) (D)
   
   allocatable :: dt2(:)

   open(10,file='listDirectStreamAccess01.dat',status='old',&
        form='formatted',access='stream',action='read',&
        position='rewind',iostat=ios,iomsg=msg)

   if( ios <> 0)  then
      print *,"fail to open the file"
      print *,"iostat=",ios
      print *,"iomsg=",msg
      stop 10
   end if

   call read1(dt2,10)

   !output results for verification
   
   do i=lbound(dt2,1),ubound(dt2,1)
      write(*,*) dt2(i)
   end do 

  close(10,status='keep') 
             
end program

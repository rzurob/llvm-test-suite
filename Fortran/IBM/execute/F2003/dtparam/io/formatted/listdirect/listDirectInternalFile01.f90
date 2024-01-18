!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : listDirectInternalFile01.f
!*
!*  DATE                       : Jan. 23 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : LIST-DIRECTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. Read external records into internal file(default character array), and
!*   read records from internal file into derived type, and verify results,
!*   use list directed IO with external file and internal file
!* 2. Derived type is polymorphic type and has 2 dimensional array components
!234567490123456749012345674901234567490123456749012345674901234567490
module m1
   type base(k1,l1)
     integer,kind :: k1
     integer,len  :: l1

     character(k1+1) :: c1(l1:l1+1,l1:l1+1)="***"
   end type

   type,extends(base) :: child(k2,l2)
     integer,kind :: k2
     integer,len  :: l2

     integer(k1+k2)  :: i1(l1-1:l2+1,l1:l2)=-99
   end type

end module

module m2
  use m1

  type,extends(child) :: gen3(k3,l3)
     integer,kind :: k3
     integer,len  :: l3

     real(k1+k2+k3) :: r1(l1+l2:l3-1,l1+l2:l3+1)=0.
     complex(k1+k2+k3) :: x1(l1+l2:l3-1,l1+l2:l3+1)=(0.,0.)
  end type

  type,extends(gen3)  :: gen4(k4,l4)
     integer,kind :: k4
     integer,len  :: l4

     logical(k1+k2+k3-k4) :: g1(l1+l3:l2+l4,l1+l3:l2+l4)=.false.
  end type

end module

program listDirectInternalFile01
  use m2
  implicit none

  logical,external :: precision_r8,precision_x6
  integer :: ios,i
  character(256) :: msg

  character(40) :: buffer(3:19)

  class(base(2,:)),pointer :: ptr=>null()

  class(base(2,:)),allocatable,target :: tar

  open(10,file='listDirectInternalFile01.dat',form='formatted' ,&
      access='sequential',iostat=ios,iomsg=msg)

  if(ios <> 0) then

     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     stop 9

  end if

  do i=3,19
     read(unit=10,fmt=*) buffer(i)
  end do

  allocate(gen4(2,3,2,3,4,7,4,8) :: tar)

  ptr=>tar

  select type(ptr)
     type is(gen4(2,*,2,*,4,*,4,*))

      if(lbound(ptr%c1,1) /= 3 .or. lbound(ptr%c1,2) /= 3)  stop 11
      if(ubound(ptr%c1,1) /= 4 .or. ubound(ptr%c1,2) /= 4)  stop 12
      if(lbound(ptr%i1,1) /= 2 .or. lbound(ptr%i1,2) /= 3)  stop 13
      if(ubound(ptr%i1,1) /= 4 .or. ubound(ptr%i1,2) /= 3)  stop 14
      if(lbound(ptr%r1,1) /= 6 .or. lbound(ptr%r1,2) /= 6)  stop 15
      if(ubound(ptr%r1,1) /= 6 .or. ubound(ptr%r1,2) /= 8)  stop 16
      if(lbound(ptr%x1,1) /= 6 .or. lbound(ptr%x1,2) /= 6)  stop 17
      if(ubound(ptr%x1,1) /= 6 .or. ubound(ptr%x1,2) /= 8)  stop 18
      if(lbound(ptr%g1,1) /= 10 .or. lbound(ptr%g1,2) /= 10) stop 19
      if(ubound(ptr%g1,1) /= 11 .or. ubound(ptr%g1,2) /= 11) stop 20

      read(buffer(3),fmt=*) ptr%c1(3,3)
      read(buffer(4),fmt=*) ptr%c1(4,3)
      read(buffer(5),*)     ptr%c1(3,4)
      read(buffer(6),*)     ptr%c1(4,4)

      read(buffer(7),*)     ptr%i1(2,3)
      read(buffer(8),*)     ptr%i1(3,3)
      read(buffer(9),*)     ptr%i1(4,3)

      read(buffer(10),*,decimal='comma')  ptr%r1(6,6)
      read(buffer(11),*,decimal='comma')  ptr%r1(6,7)
      read(buffer(12),*,decimal='comma')  ptr%r1(6,8)

      read(buffer(13),*)    ptr%x1(6,6)
      read(buffer(14),*)    ptr%x1(6,7)
      read(buffer(15),*)    ptr%x1(6,8)

      read(buffer(16),*)    ptr%g1(10,10)
      read(buffer(17),*)    ptr%g1(11,10)
      read(buffer(18),*)    ptr%g1(10,11)
      read(buffer(19),*)    ptr%g1(11,11)

      class default
        stop 10

  end select

  ! verify results

  select type(tar)
      type is(gen4(2,*,2,*,4,*,4,*))
         if(tar%c1(3,3) /= "ABC")                   stop 22
         if(tar%c1(4,3) /= "abc")                   stop 23
         if(tar%c1(3,4) /= "DEF")                   stop 24
         if(tar%c1(4,4) /= "def")                   stop 25

         if(tar%i1(2,3) /= -12 )                    stop 26
         if(tar%i1(3,3) /= 57 )                     stop 27
         if(tar%i1(4,3) /= 22 )                     stop 28

         if(.not. precision_r8(tar%r1(6,6),-3.4D-2 ))  stop 29
         if(.not. precision_r8(tar%r1(6,7),2.4_8))     stop 30
         if(.not. precision_r8(tar%r1(6,8),-3._8))     stop 31

         if(.not. precision_x6(tar%x1(6,6),(5.6_8,7.8_8) ))       stop 32
         if(.not. precision_x6(tar%x1(6,7),(4.5D-2,2.3D3) ))      stop 33
         if(.not. precision_x6(tar%x1(6,8),(0.006_8,-0.345_8) ))  stop 34

         if(tar%g1(10,10) .neqv. .true.)                stop 35
         if(tar%g1(11,10) .neqv. .false.)               stop 36
         if(tar%g1(10,11) .neqv. .true.)                stop 37
         if(tar%g1(11,11) .neqv. .false.)               stop 38

      class default

         stop 21

  end select

  close(10)

end program

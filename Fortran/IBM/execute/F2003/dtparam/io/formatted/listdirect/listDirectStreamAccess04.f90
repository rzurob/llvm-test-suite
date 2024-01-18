!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 22 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : LIST-DIRECTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. Test Read statement with list directed IO and stream access
!* 2. Derived type is polymorphic type
!234567490123456749012345674901234567490123456749012345674901234567490
module m1
   type base(k1,l1)
      integer,kind  :: k1=4
      integer,len   :: l1=2

      character(k1) :: c1(l1:k1)="****"
      integer(k1)   :: i1(l1:l1+1)=-99
   end type

   type,extends(base) :: child(k2,l2)
      integer,kind  :: k2=8
      integer,len   :: l2=4

      real(k2)      :: r1(l1:l2)=0.
      complex(2*k2) :: x1(l1:l2)=(0.,0.)
   end type

end module

module m2
   use m1
   type,extends(child) :: gen3(k3,l3)
      integer,kind :: k3=4
      integer,len  :: l3=5

      logical(k3) :: g1(2:3,4:5)=.false.
   end type

   contains

      function getResult(dt1,dt2)
         class(base(4,:)),allocatable ,intent(in) :: dt1
         class(base(4,*)),allocatable ,intent(in) :: dt2
         class(base(4,dt2%l1)),pointer :: getResult(:)

         allocate(getResult(2),source=[dt2,dt1] )

      end function

      subroutine read1(dt,unit)
         class(base(4,:)),allocatable,intent(inout) :: dt
         integer,intent(in) :: unit
         integer :: mypos

         allocate(gen3(4,2,8,4,2,5) :: dt)

         select type(dt)
            type is(gen3(4,*,8,*,2,*))
                inquire(unit,pos=mypos)
                if(mypos /= 1)             stop 12
                read(unit,*,pos=mypos) dt
            class default
               stop 11
         end select

      end subroutine

      subroutine read2(dt,unit)
         class(base(4,*)),allocatable,intent(inout) :: dt
         integer,intent(in) :: unit

         select type(dt)
            type is(gen3(4,*,8,*,2,*))
                read(unit,*,decimal='comma') dt
            class default
               stop 13
         end select

      end subroutine

end module

program listDirectStreamAccess04
   use m2

   logical,external :: precision_r8,precision_x3
   integer :: ios,i
   character(256) :: msg

   implicit class(base(4,:)) (T)
   implicit class(base) (S)

   allocatable :: t1,s1,t2(:)

   allocate(gen3(4,2,8,4,2,5) :: s1)

   open(10,file='listDirectStreamAccess04.dat',form='formatted',&
        access='stream',iostat=ios,iomsg=msg)

   if(ios <> 0) then
       print *,"fail to open the file"
       print *,"iostat=",ios
       print *,"iomsg=",msg
       stop 10
   end if

   call read1(t1,10)

   ! verify t1

   select type(t1)
      type is(gen3(4,*,8,*,2,*))

        if(any(t1%c1 /= ["BLUE","****","STAR"] ))              stop 15
        if(any(t1%i1 /= [-201,3]))                             stop 16
        if(.not. precision_r8(t1%r1(2),-0.45_8))               stop 17
        if(.not. precision_r8(t1%r1(3),5.0_8))                 stop 18
        if(.not. precision_r8(t1%r1(4),2.0_8))                 stop 19
        if(.not. precision_x3(t1%x1(2),(2.1Q2,-2.1Q-1)))       stop 20
        if(.not. precision_x3(t1%x1(3),(0.005_16,-3.008_16)))  stop 21
        if(.not. precision_x3(t1%x1(4),(-3.7_16,4.0_16)))      stop 22

        if(t1%g1(2,4) .neqv. .true.)                           stop 23
        if(t1%g1(3,4) .neqv. .false.)                          stop 24
        if(t1%g1(2,5) .neqv. .false.)                          stop 25
        if(t1%g1(3,5) .neqv. .true.)                           stop 26
      class default
        stop 27
   end select

   call read2(s1,10)

   ! verify s1

   select type(s1)
      type is(gen3(4,*,8,*,2,*))

        if(any(s1%c1 /= ["****","xlft","'xy'"] ))             stop 28
        if(any(s1%i1 /= [-99,-7]))                             stop 29
        if(.not. precision_r8(s1%r1(2),-3.2D2))                stop 30
        if(.not. precision_r8(s1%r1(3),0.4_8))                 stop 31
        if(.not. precision_r8(s1%r1(4),1.11_8))                stop 32
        if(.not. precision_x3(s1%x1(2),(3.2_16,-7.5_16)))      stop 33
        if(.not. precision_x3(s1%x1(3),(-1.0Q-4,12.0Q-3)))     stop 34
        if(.not. precision_x3(s1%x1(4),(1.0_16,1.0_16)))       stop 35

        if(s1%g1(2,4) .neqv. .false.)                          stop 36
        if(s1%g1(3,4) .neqv. .true.)                           stop 37
        if(s1%g1(2,5) .neqv. .true.)                           stop 38
        if(s1%g1(3,5) .neqv. .false.)                          stop 39

      class default
        stop 40
   end select

   allocate(t2(-1:0),source=getResult(t1,s1) )

   ! verify t2
   select type(x=>t2)
       type is(gen3(4,*,8,*,2,*))

        if(any(x(-1)%c1 /= ["****","xlft","'xy'"] ))              stop 42
        if(any(x(-1)%i1 /= [-99,-7]))                             stop 43
        if(.not. precision_r8(x(-1)%r1(2),-3.2D2))                stop 44
        if(.not. precision_r8(x(-1)%r1(3),0.4_8))                 stop 45
        if(.not. precision_r8(x(-1)%r1(4),1.11_8))                stop 46
        if(.not. precision_x3(x(-1)%x1(2),(3.2_16,-7.5_16)))      stop 47
        if(.not. precision_x3(x(-1)%x1(3),(-1.0Q-4,12.0Q-3)))     stop 48
        if(.not. precision_x3(x(-1)%x1(4),(1.0_16,1.0_16)))       stop 49

        if(x(-1)%g1(2,4) .neqv. .false.)                          stop 50
        if(x(-1)%g1(3,4) .neqv. .true.)                           stop 51
        if(x(-1)%g1(2,5) .neqv. .true.)                           stop 52
        if(x(-1)%g1(3,5) .neqv. .false.)                          stop 53

        if(any(x(0)%c1 /= ["BLUE","****","STAR"] ))              stop 54
        if(any(x(0)%i1 /= [-201,3]))                             stop 55
        if(.not. precision_r8(x(0)%r1(2),-0.45_8))               stop 56
        if(.not. precision_r8(x(0)%r1(3),5.0_8))                 stop 57
        if(.not. precision_r8(x(0)%r1(4),2.0_8))                 stop 58
        if(.not. precision_x3(x(0)%x1(2),(2.1Q2,-2.1Q-1)))       stop 59
        if(.not. precision_x3(x(0)%x1(3),(0.005_16,-3.008_16)))  stop 60
        if(.not. precision_x3(x(0)%x1(4),(-3.7_16,4.0_16)))      stop 61

        if(x(0)%g1(2,4) .neqv. .true.)                           stop 62
        if(x(0)%g1(3,4) .neqv. .false.)                          stop 63
        if(x(0)%g1(2,5) .neqv. .false.)                          stop 64
        if(x(0)%g1(3,5) .neqv. .true.)                           stop 65

       class default
          stop 41
   end select

   close(10)

end program

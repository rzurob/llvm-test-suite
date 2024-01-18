!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : listDirectAsynchronous05.f
!*
!*  DATE                       : Jan. 27 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : LIST-DIRECTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. Test Read statement with asynchronous IO
!* 2. Derived type has multiple derived type components
!* 3. Derived type is polymorphic type and it is automatic object
!234567490123456749012345674901234567490123456749012345674901234567490

program listDirectAsynchronous05
   call sub
end program

subroutine sub
   type A(k1,l1)
      integer,kind :: k1=2
      integer,len  :: l1=2

      character(l1) :: c1="**"
      character(k1) :: c2(l1)="##"
      integer(k1)   :: i1=-99
   end type

   type B(k2,l2)
      integer,kind :: k2=2
      integer,len  :: l2=4

      logical(k2)   :: g1(l2-1:l2,l2:l2+1)=.false.
      integer(2*k2) :: i2(l2-1:l2,1)=-99
   end type

   type base(k3,l3)
       integer,kind :: k3=2
       integer,len  :: l3=3

       type(A(k3,l3-1)) :: a1comp(l3:l3+1)
   end type

   type,extends(base) :: child(k4,l4)
       integer,kind :: k4=2
       integer,len  :: l4=3

       type(B(k4,l4+1)) :: b1comp
   end type

   type(child(2,3,2,3)) :: obj1(2)

   class(base(2,3)),allocatable :: obj3(:)

   call readData(obj3)

   ! verify obj3

   select type(obj3)
   type is(child(2,*,2,*))

   write(*,*)
   write(*,*) "Value of obj3:"

   write(*,*) obj3(lbound(obj3,1))
   write(*,*) obj3(lbound(obj3,1) + 1)
   write(*,*)
   write(*,*) obj3(lbound(obj3,1))%a1comp, &
              obj3(lbound(obj3,1))%b1comp
   write(*,*) obj3(lbound(obj3,1) + 1)%a1comp,&
              obj3(lbound(obj3,1) + 1)%b1comp
   write(*,*)
   write(*,*) obj3(lbound(obj3,1))%a1comp(3)%c1, &
              obj3(lbound(obj3,1))%a1comp(3)%c2, &
              obj3(lbound(obj3,1))%a1comp(3)%i1, &
              obj3(lbound(obj3,1))%a1comp(4)%c1, &
              obj3(lbound(obj3,1))%a1comp(4)%c2, &
              obj3(lbound(obj3,1))%a1comp(4)%i1, &
              obj3(lbound(obj3,1))%b1comp%g1,    &
              obj3(lbound(obj3,1))%b1comp%i2
   write(*,*) obj3(lbound(obj3,1)+1)%a1comp(3)%c1, &
              obj3(lbound(obj3,1)+1)%a1comp(3)%c2, &
              obj3(lbound(obj3,1)+1)%a1comp(3)%i1, &
              obj3(lbound(obj3,1)+1)%a1comp(4)%c1, &
              obj3(lbound(obj3,1)+1)%a1comp(4)%c2, &
              obj3(lbound(obj3,1)+1)%a1comp(4)%i1, &
              obj3(lbound(obj3,1)+1)%b1comp%g1,    &
              obj3(lbound(obj3,1)+1)%b1comp%i2

   write(*,*)

   write(*,*) obj3(lbound(obj3,1))%a1comp(3)%c1, &
             obj3(lbound(obj3,1))%a1comp(3)%c2(1), &
             obj3(lbound(obj3,1))%a1comp(3)%c2(2), &
             obj3(lbound(obj3,1))%a1comp(3)%i1, &
             obj3(lbound(obj3,1))%a1comp(4)%c1, &
             obj3(lbound(obj3,1))%a1comp(4)%c2(1), &
             obj3(lbound(obj3,1))%a1comp(4)%c2(2), &
             obj3(lbound(obj3,1))%a1comp(4)%i1, &
             obj3(lbound(obj3,1))%b1comp%g1(3,4),    &
             obj3(lbound(obj3,1))%b1comp%g1(4,4),    &
             obj3(lbound(obj3,1))%b1comp%g1(3,5),    &
             obj3(lbound(obj3,1))%b1comp%g1(4,5),    &
             obj3(lbound(obj3,1))%b1comp%i2(3,1),    &
             obj3(lbound(obj3,1))%b1comp%i2(4,1)

   write(*,*) obj3(lbound(obj3,1)+1)%a1comp(3)%c1, &
              obj3(lbound(obj3,1)+1)%a1comp(3)%c2(1), &
              obj3(lbound(obj3,1)+1)%a1comp(3)%c2(2), &
              obj3(lbound(obj3,1)+1)%a1comp(3)%i1, &
              obj3(lbound(obj3,1)+1)%a1comp(4)%c1, &
              obj3(lbound(obj3,1)+1)%a1comp(4)%c2(1), &
              obj3(lbound(obj3,1)+1)%a1comp(4)%c2(2), &
              obj3(lbound(obj3,1)+1)%a1comp(4)%i1, &
              obj3(lbound(obj3,1)+1)%b1comp%g1(3,4),    &
              obj3(lbound(obj3,1)+1)%b1comp%g1(4,4),    &
              obj3(lbound(obj3,1)+1)%b1comp%g1(3,5),    &
              obj3(lbound(obj3,1)+1)%b1comp%g1(4,5),    &
              obj3(lbound(obj3,1)+1)%b1comp%i2(3,1),    &
              obj3(lbound(obj3,1)+1)%b1comp%i2(4,1)

   class default

       stop 13

   end select

   contains

       subroutine  readData(dt)
          class(base(2,3)),allocatable,intent(inout) :: dt(:)
          class(base(2,obj1%l3)),allocatable :: obj2(:)

          integer :: ios
          character(256) :: msg

          allocate(dt(size(obj1)),source=obj1)

          open(10,file='listDirectAsynchronous05.dat',status='old',&
               form='formatted',access='sequential',position='rewind',&
               asynchronous='yes',iostat=ios,iomsg=msg)

          if(ios <> 0) then
              print *,"fail to open the file"
              print *,"iostat=",ios
              print *,"iomsg=",msg
              stop 10
          end if

          select type(dt)
              type is(child(2,*,2,*))

                 read(10,*,asynchronous='yes') dt(lbound(dt,1))

                 read(10,*,asynchronous='yes') dt(lbound(dt,1)+1)

              class default

                 stop 11

          end select

          ! dt is the pending input/output storage sequence
          call set_auto(dt,obj2)

          wait(10)

          select type(obj2)

          type is(child(2,*,2,*))

          ! verify obj2
          write(*,*) "Value of obj2:"
          write(*,*) obj2(lbound(obj2,1))
          write(*,*) obj2(lbound(obj2,1) + 1)
          write(*,*)
          write(*,*) obj2(lbound(obj2,1))%a1comp, &
                     obj2(lbound(obj2,1))%b1comp
          write(*,*) obj2(lbound(obj2,1) + 1)%a1comp,&
                     obj2(lbound(obj2,1) + 1)%b1comp
          write(*,*)
          write(*,*) obj2(lbound(obj2,1))%a1comp(3)%c1, &
                     obj2(lbound(obj2,1))%a1comp(3)%c2, &
                     obj2(lbound(obj2,1))%a1comp(3)%i1, &
                     obj2(lbound(obj2,1))%a1comp(4)%c1, &
                     obj2(lbound(obj2,1))%a1comp(4)%c2, &
                     obj2(lbound(obj2,1))%a1comp(4)%i1, &
                     obj2(lbound(obj2,1))%b1comp%g1,    &
                     obj2(lbound(obj2,1))%b1comp%i2
          write(*,*) obj2(lbound(obj2,1)+1)%a1comp(3)%c1, &
                     obj2(lbound(obj2,1)+1)%a1comp(3)%c2, &
                     obj2(lbound(obj2,1)+1)%a1comp(3)%i1, &
                     obj2(lbound(obj2,1)+1)%a1comp(4)%c1, &
                     obj2(lbound(obj2,1)+1)%a1comp(4)%c2, &
                     obj2(lbound(obj2,1)+1)%a1comp(4)%i1, &
                     obj2(lbound(obj2,1)+1)%b1comp%g1,    &
                     obj2(lbound(obj2,1)+1)%b1comp%i2

          write(*,*)

          write(*,*) obj2(lbound(obj2,1))%a1comp(3)%c1, &
                     obj2(lbound(obj2,1))%a1comp(3)%c2(1), &
                     obj2(lbound(obj2,1))%a1comp(3)%c2(2), &
                     obj2(lbound(obj2,1))%a1comp(3)%i1, &
                     obj2(lbound(obj2,1))%a1comp(4)%c1, &
                     obj2(lbound(obj2,1))%a1comp(4)%c2(1), &
                     obj2(lbound(obj2,1))%a1comp(4)%c2(2), &
                     obj2(lbound(obj2,1))%a1comp(4)%i1, &
                     obj2(lbound(obj2,1))%b1comp%g1(3,4),    &
                     obj2(lbound(obj2,1))%b1comp%g1(4,4),    &
                     obj2(lbound(obj2,1))%b1comp%g1(3,5),    &
                     obj2(lbound(obj2,1))%b1comp%g1(4,5),    &
                     obj2(lbound(obj2,1))%b1comp%i2(3,1),    &
                     obj2(lbound(obj2,1))%b1comp%i2(4,1)

          write(*,*) obj2(lbound(obj2,1)+1)%a1comp(3)%c1, &
                     obj2(lbound(obj2,1)+1)%a1comp(3)%c2(1), &
                     obj2(lbound(obj2,1)+1)%a1comp(3)%c2(2), &
                     obj2(lbound(obj2,1)+1)%a1comp(3)%i1, &
                     obj2(lbound(obj2,1)+1)%a1comp(4)%c1, &
                     obj2(lbound(obj2,1)+1)%a1comp(4)%c2(1), &
                     obj2(lbound(obj2,1)+1)%a1comp(4)%c2(2), &
                     obj2(lbound(obj2,1)+1)%a1comp(4)%i1, &
                     obj2(lbound(obj2,1)+1)%b1comp%g1(3,4),    &
                     obj2(lbound(obj2,1)+1)%b1comp%g1(4,4),    &
                     obj2(lbound(obj2,1)+1)%b1comp%g1(3,5),    &
                     obj2(lbound(obj2,1)+1)%b1comp%g1(4,5),    &
                     obj2(lbound(obj2,1)+1)%b1comp%i2(3,1),    &
                     obj2(lbound(obj2,1)+1)%b1comp%i2(4,1)

          class default

              stop 12

          end select

          close(10)

       end subroutine

       ! dt has asynchronous attribute
       subroutine set_auto(dt,obj2)
          class(base(2,*)),intent(in),asynchronous :: dt(:)
          class(base(2,dt%l3)),allocatable :: obj2(:)

          allocate(obj2(size(dt)),source=dt)

       end subroutine

end subroutine

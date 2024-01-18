!*  ===================================================================
!*
!*  TEST CASE NAME             : defAssignProcPtrComp03a.f
!*
!*  DATE                       : Feb. 18 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. Test defined assignment with interface block
!*  2. Derived type is polymorphic type, base, child, gen3 has their own procedure pointer component
!234567490123456749012345674901234567490123456749012345674901234567490
module m
   type base(k1,l1)
      integer,kind :: k1
      integer,len  :: l1

      integer(k1)  :: i1(k1:k1+l1+1)
      procedure(fun),nopass,pointer :: procptr1=>null()
   end type

   type,extends(base) :: child(k2,l2)
      integer,kind   :: k2
      integer,len    :: l2

      integer(k2),allocatable :: i2(:)
      procedure(fun),nopass,pointer :: procptr2=>null()
   end type

   type,extends(child) :: gen3(k3,l3)
      integer,kind  :: k3
      integer,len   :: l3

      integer(k3),pointer :: i3(:)
      procedure(sub),nopass,pointer :: procptr3=>null()
   end type

   contains

      subroutine assign1(this,dt)
         class(*),intent(inout) :: this
         class(*),intent(in)    :: dt

         print *,"in assign1"
         select type(this)
            type is(base(2,*))
                select type(dt)
                   type is(base(2,*))
                       print *,"it is base"
                       this%i1=dt%i1
                       this%procptr1=>dt%procptr1
                   class default
                       stop 11
                end select
            type is(child(2,*,4,*))
                select type(dt)
                   type is(child(2,*,4,*))
                        print *,"it is child"
                        this%i1=dt%i1
                        if(allocated(this%i2)) deallocate(this%i2)
                        if(allocated(dt%i2))  &
                           allocate(this%i2(size(dt%i2)),source=dt%i2)
                        this%procptr1=>dt%procptr1
                        this%procptr2=>dt%procptr2
                   class default
                        stop 12
                end select
            type is(gen3(2,*,4,*,8,*))
                 select type(dt)
                     type is(gen3(2,*,4,*,8,*))
                        print *,"it is gen3"
                        this%i1=dt%i1
                        if(allocated(this%i2)) deallocate(this%i2)
                        if(allocated(dt%i2))  &
                          allocate(this%i2(size(dt%i2)),source=dt%i2)
                        if(associated(dt%i3))  &
                            allocate(this%i3(size(dt%i3)),source=dt%i3)

                        this%procptr1=>dt%procptr1
                        this%procptr2=>dt%procptr2
                        this%procptr3=>dt%procptr3

                      class default
                         stop 13
                 end select
         end select

      end subroutine

      subroutine assign2(this,dt)
         class(*),intent(inout) :: this(:)
         class(*),intent(in)    :: dt(:)
         integer :: i

         print *,"in assign2"
         do i=lbound(this,1),ubound(this,1)

         select type(x=>this(i))
            type is(base(2,*))
                select type(y=>dt(i))
                   type is(base(2,*))
                       print *,"it is base"
                       x%i1=y%i1
                       x%procptr1=>y%procptr1
                   class default
                       stop 14
                end select
            type is(child(2,*,4,*))
                select type(y=>dt(i))
                   type is(child(2,*,4,*))
                        print *,"it is child"
                        x%i1=y%i1
                        if(allocated(x%i2)) deallocate(x%i2)
                        if(allocated(y%i2))  &
                           allocate(x%i2(size(y%i2)),source=y%i2)
                        x%procptr1=>y%procptr1
                        x%procptr2=>y%procptr2

                   class default
                        stop 15
                end select
            type is(gen3(2,*,4,*,8,*))
                 select type(y=>dt(i))
                     type is(gen3(2,*,4,*,8,*))
                        print *,"it is gen3"
                        x%i1=y%i1
                        if(allocated(x%i2)) deallocate(x%i2)
                        if(allocated(x%i2))  &
                          allocate(x%i2(size(y%i2)),source=y%i2)
                        if(associated(x%i3))  &
                            allocate(x%i3(size(y%i3)),source=y%i3)

                        x%procptr1=>y%procptr1
                        x%procptr2=>y%procptr2
                        x%procptr3=>y%procptr3

                      class default
                         stop 16
                 end select
         end select

         end do

      end subroutine
      function fun(arg)
          class(*),intent(in)  :: arg
          class(*),allocatable :: fun

          allocate(fun,source=arg)

      end function

      subroutine sub(arg)
          class(*),intent(inout) :: arg

          select type(arg)
             type is(base(2,*))
                arg%i1=-arg%i1
             type is(child(2,*,4,*))
                arg%i1=2*arg%i1
                arg%i2=arg%i2+10
             class default
                stop 10
          end select

      end subroutine

end module

program defAssignProcPtrComp03a
   use m
   implicit none

   integer :: i
   class(*),allocatable :: obj1(:),obj2(:),obj3,obj4(:)

   procedure(fun),pointer :: procptr1=>null()

   procedure(sub),pointer :: procptr2=>null()

   allocate(gen3(2,1,4,2,8,3) :: obj1(2),obj2(2))

   select type(x=>obj1)
       type is(gen3(2,*,4,*,8,*))
          x(1)%i1=[1_2,2_2,3_2]
          x(2)%i1=[4_2,5_2,6_2]
          x(1)%i2=[10]
          x(2)%i2=[11,12]
          allocate(x(1)%i3(3),source=[20_8,30_8,40_8])
          allocate(x(2)%i3(1),source=[50_8])

          do i=1,2
             x(i)%procptr1=>fun
             x(i)%procptr2=>fun
             x(i)%procptr3=>sub
          end do

       class default
          stop 17
   end select

   procptr1=>fun
   procptr2=>sub

   print *,"****TEST  1****"
   ! invoke assign1
   call assign1(obj2(2), procptr1(obj1(1)))

   associate(x=>obj2(2))
      select type(x)
        type is(gen3(2,*,4,*,8,*))

           if(any(x%i1 /=[1_2,2_2,3_2]))               stop 19
           if(any(x%i2 /= 10))                         stop 20
           if(any(x%i3 /= [20_8,30_8,40_8]))           stop 21

           allocate(obj3,source=-100)

           associate(y=>x%procptr1(obj3))
              select type(y)
                 type is(integer)
                     if(y /= -100)                     stop 22
                 class default
                   stop 23
              end select
           end associate

           deallocate(obj3)
           allocate(obj3,source=x%child%base)

           associate(y=>x%procptr2(obj3))
               select type(y)
                  type is(base(2,*))
                     if(any(y%i1 /= [1_2,2_2,3_2]))    stop 24
                  class default
                     stop 25
               end select
           end associate

           call x%procptr3(x%child)

           if(any(x%i1 /= [2_2,4_2,6_2]))              stop 26
           if(any(x%i2 /= 20))                         stop 27

           call x%procptr3(x%child%base)

           if(any(x%i1 /= [-2_2,-4_2,-6_2]))           stop 28

        class default
           stop 29
      end select
   end associate

   print *,"****TEST  2****"
   ! invoke assign1
   call assign1(obj2(1), fun(obj1(2)))

   associate(x=>obj2(1))
      select type(x)
        type is(gen3(2,*,4,*,8,*))
           if(any(x%i1 /=[4_2,5_2,6_2]))               stop 30
           if(any(x%i2 /= [11,12]))                    stop 31
           if(any(x%i3 /= [50_8]))                     stop 32

           deallocate(obj3)

           allocate(obj3,source=x%i1(3))

           associate(y=>x%procptr1(obj3))
              select type(y)
                 type is(integer(2))
                     if(y /= 5_2)                      stop 33
                 class default
                   stop 34
              end select
           end associate

           deallocate(obj3)
           allocate(obj3,source=x%child%base)

           associate(y=>x%procptr2(obj3))
               select type(y)
                  type is(base(2,*))
                     if(any(y%i1 /= [4_2,5_2,6_2]))    stop 35
                  class default
                     stop 36
               end select
           end associate

           call x%procptr3(x%child)

           if(any(x%i1 /= [8_2,10_2,12_2]))            stop 37
           if(any(x%i2 /= [21,22]))                    stop 38

           call x%procptr3(x%child%base)

           if(any(x%i1 /= [-8_2,-10_2,-12_2]))         stop 39

        class default
           stop 40
      end select

   end associate

   deallocate(obj2)

   allocate(gen3(2,1,4,2,8,3) :: obj2(2))

   print *,"****TEST  3****"
   ! invoke assign2
   call assign2(obj2,obj1)

   select type(x=>obj2)
      type is(gen3(2,*,4,*,8,*))

        if(any(x(1)%i1 /= [1_2,2_2,3_2]))               stop 41
        if(any(x(1)%i2 /= [10]))                        stop 42
        if(any(x(1)%i3 /= [20_8,30_8,40_8]))            stop 43

        if(any(x(2)%i1 /= [4_2,5_2,6_2]))               stop 44
        if(any(x(2)%i2 /= [11,12]))                     stop 45
        if(any(x(2)%i3 /= [50_8]))                      stop 46

        if(.not. associated(x(1)%procptr1,fun))         stop 47
        if(.not. associated(x(1)%procptr2,fun))         stop 48
        if(.not. associated(x(1)%procptr3,sub))         stop 49

        if(.not. associated(x(2)%procptr1,fun))         stop 50
        if(.not. associated(x(2)%procptr2,fun))         stop 51
        if(.not. associated(x(2)%procptr3,sub))         stop 52

        if(.not. associated(x(1)%procptr1,fun))         stop 53
        if(.not. associated(x(1)%procptr2,fun))         stop 54
        if(.not. associated(x(1)%procptr3,sub))         stop 55

        deallocate(obj3)

        allocate(obj3,source=x(1)%child)

        call procptr2(obj3)

        select type(obj3)
            type is(child(2,*,4,*))
              if(any(obj3%i1 /= [2_2,4_2,6_2]))         stop 56
              if(any(obj3%i2 /= 20))                    stop 57
            class default
              stop 58
        end select

        deallocate(obj3)

        allocate(obj3,source=x(2)%child%base)

        call procptr2(obj3)

        select type(obj3)
            type is(base(2,*))
              if(any(obj3%i1 /= [-4_2,-5_2,-6_2]))      stop 59
            class default
              stop 60
        end select

      class default

        stop 61

   end select

   deallocate(obj2)

   allocate(child(2,1,4,2) :: obj2(1))

   print *,"****TEST  4****"
   select type(x=>obj1(1))
      type is(gen3(2,*,4,*,8,*))
         ! invoke assign1
         call assign1(obj2(1), x%procptr1(x%child))

         select type(y=>obj2(1))
            type is(child(2,*,4,*))
                if(any(y%i1 /= [1_2,2_2,3_2]))          stop 62
                if(any(y%i2 /= 10))                     stop 63
            class default
               stop 64
         end select

      class default
          stop 65
   end select

   deallocate(obj2)

   allocate(child(2,1,4,2) :: obj2(2))

   print *,"****TEST  5****"
   select type(x=>obj1)
      type is(gen3(2,*,4,*,8,*))
         allocate(obj4(2),source=x%child)
      class default
         stop 66
   end select
   ! invoke assign2
   call assign2(obj2, obj4)

   select type(x=>obj2)
        type is(child(2,*,4,*))
            if(any(x(1)%i1 /= [1,2,3]))                 stop 67
            if(any(x(2)%i1 /= [4,5,6]))                 stop 68
            if(any(x(1)%i2 /= 10))                      stop 69
            if(any(x(2)%i2 /= [11,12]))                 stop 70
            if(.not. associated(x(1)%procptr1,fun))     stop 73
            if(.not. associated(x(2)%procptr1,fun))     stop 74
            if(.not. associated(x(1)%procptr2,fun))     stop 75
            if(.not. associated(x(2)%procptr2,fun))     stop 76

            associate(y=>x(1)%procptr1(x(1)) )
               select type(y)
                  type is(child(2,*,4,*))
                     if(any(y%i1 /= [1,2,3]))           stop 77
                     if(any(y%i2 /= [10]))              stop 78
                  class default
                     stop 79
               end select
            end associate

            associate(y=>x(2)%procptr1(x(2)))
               select type(y)
                  type is(child(2,*,4,*))
                     if(any(y%i1 /= [4,5,6]))           stop 80
                     if(any(y%i2 /= [11,12]))           stop 81
                  class default
                     stop 82
               end select
            end associate

        class default
            stop 83
   end select

   deallocate(obj2)

   allocate(child(2,1,4,2) :: obj2(2))

   print *,"****TEST  6****"
   do i=1,2
     select type(x=>obj1)
        type is(gen3(2,*,4,*,8,*))
             ! invoke assign1
             call assign1(obj2(i), x(i)%child)
        class default
           stop 84
     end select
   end do

   select type(x=>obj2)
        type is(child(2,*,4,*))

            if(any(x(1)%i1 /= [1,2,3]))                 stop 85
            if(any(x(2)%i1 /= [4,5,6]))                 stop 86
            if(any(x(1)%i2 /= 10))                      stop 87
            if(any(x(2)%i2 /= [11,12]))                 stop 88
            if(.not. associated(x(1)%procptr1,fun))     stop 89
            if(.not. associated(x(2)%procptr1,fun))     stop 90
            if(.not. associated(x(1)%procptr2,fun))     stop 91
            if(.not. associated(x(2)%procptr2,fun))     stop 92

            associate(y=>x(1)%procptr1(x(1)) )
               select type(y)
                  type is(child(2,*,4,*))
                     if(any(y%i1 /= [1,2,3]))           stop 93
                     if(any(y%i2 /= [10]))              stop 94
                  class default
                     stop 95
               end select
            end associate

            associate(y=>x(2)%procptr1(x(2)))
               select type(y)
                  type is(child(2,*,4,*))
                     if(any(y%i1 /= [4,5,6]))           stop 96
                     if(any(y%i2 /= [11,12]))           stop 97
                  class default
                     stop 98
               end select
            end associate

        class default
            stop 99
   end select

   deallocate(obj2)

   allocate(base(2,1) :: obj2(2))

   print *,"****TEST  7****"
   do i=1,2
     select type(x=>obj1)
        type is(gen3(2,*,4,*,8,*))
             ! invoke assign1
             call assign1(obj2(i), x(i)%child%base)
        class default
           stop 100
     end select
   end do

   select type(x=>obj2)
        type is(base(2,*))
            if(any(x(1)%i1 /= [1,2,3]))                 stop 101
            if(any(x(2)%i1 /= [1,2,3]))                 stop 102
            if(.not. associated(x(1)%procptr1,fun))     stop 103
            if(.not. associated(x(2)%procptr1,fun))     stop 104
        class default
            stop  105
   end select

end program

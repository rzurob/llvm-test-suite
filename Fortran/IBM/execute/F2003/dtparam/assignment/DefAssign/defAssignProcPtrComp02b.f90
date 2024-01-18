!*  ===================================================================
!*
!*  TEST CASE NAME             : defAssignProcPtrComp02b.f
!*
!*  DATE                       : Feb. 17 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. Test defined assignment with generic binding
!*  2. Derived type has procedure pointer component
!*  3. Procedure pointer component associates with defined assignment procedure
!234567490123456749012345674901234567490123456749012345674901234567490
module mA
   type A(l1)
     integer,len :: l1 ! l1=4
     character(l1)  :: c1(l1)="(**)"
     integer        :: i1(l1-1:l1+1) = -99
     procedure(assignChar),nopass,pointer :: aptr1=>null()
     procedure(assignInt),nopass,pointer  :: aptr2=>null()
     contains
         procedure,nopass :: assignChar
         procedure,nopass :: assignInt
         procedure,pass   :: assignA
         generic :: assignment(=)=>assignA
   end type
   contains
     subroutine assignChar(char1,char2)
        character(*),intent(inout):: char1
        character(*),intent(in)   :: char2

        print *,"in assignChar"
        char1=char2(4:4)//char2(3:3)//char2(2:2)//char2(1:1)
     end subroutine

     subroutine assignInt(int1,int2)
        integer,intent(inout)  :: int1
        integer,intent(in)     :: int2

        print *,"in assignInt"
        int1=-int2
     end subroutine

     subroutine assignA(this,ta)
        class(A(*)),intent(inout) :: this
        type(A(*)),intent(in)  :: ta
        integer :: i

        print *,"in assignA"
        select type(this)
           type is(A(*))

             if(associated(ta%aptr1)) this%aptr1=>ta%aptr1
             if(associated(ta%aptr2)) this%aptr2=>ta%aptr2

             do i=lbound(this%c1,1) ,ubound(this%c1,1)
               if(associated(ta%aptr1,assignChar)) then
                  call this%aptr1(this%c1(i),ta%c1(i)) ! invoke assignChar
               end if
             end do

             do i=lbound(this%i1,1) ,ubound(this%i1,1)
               if(associated(ta%aptr2,assignInt)) then
                  call this%aptr2(this%i1(i),ta%i1(i))  ! invoke assignInt
               end if
             end do

           class default
              stop 10
        end select

     end subroutine

end module

module mB
   use mA
   type B(l2)
     integer,len :: l2 ! l2=3
     type(A(l2+1)),pointer :: acomp=>null()
     procedure(assignA),nopass,pointer :: bptr => null()
     contains
         procedure,pass :: assignB
         generic :: assignment(=)=>assignB
   end type

   contains

     subroutine assignB(this,tb)
        class(B(*)),intent(inout) :: this
        type(B(*)),intent(in) :: tb

        print *,"in assignB"
        select type(this)
           type is(B(*))

            if(associated(tb%bptr))  this%bptr=>tb%bptr
            allocate(A(4) :: this%acomp)

            if(associated(tb%acomp))  this%acomp=tb%acomp  !<== invoke assignA

            if(associated(tb%bptr,assignA)) then
               call this%bptr(this%acomp,tb%acomp)
            end if
           class default
               stop 11
        end select
      end subroutine

end module

module mC
   use mB
   type C(l3)
     integer,len  :: l3 ! l3=4
     type(B(l3-1)) :: bcomp=B(3)()
     procedure(assignB),nopass,pointer :: cptr => null()
     contains
        procedure,pass :: assignC
        generic :: assignment(=)=>assignC
   end type

   contains

      subroutine assignC(this,tc)
         class(C(*)),intent(inout) :: this
         type(C(*)),intent(in) :: tc

         print *,"in assignC"
         select type(this)
            type is(C(*))
               if(associated(tc%cptr))  this%cptr=>tc%cptr

               this%bcomp=tc%bcomp     !<== invoke assignB

               if(associated(tc%cptr,assignB)) then
                  call this%cptr(this%bcomp,tc%bcomp)
               end if
            class default
               stop 12
          end select

       end subroutine

end module

program defAssignProcPtrComp02b
   use mC
   implicit none

   type(A(4)),target :: aobj1

   type(B(3)) :: bobj1

   class(C(:)),allocatable :: cobj1

   class(*),allocatable    :: upoly

   allocate(C(4)  :: cobj1)

   allocate(C(4)  :: upoly)

   ! invoke assignC
   cobj1=C(4)(B(3)(aobj1,null()),null())

   select type(cobj1)
      type is(C(*))
         associate(x=>cobj1%bcomp%acomp)
            if(any(x%c1  /= "(**)"))                    stop 14
            if(any(x%i1 /= -99))                        stop 15
         end associate
         if(associated(cobj1%cptr))                     stop 16
         if(associated(cobj1%bcomp%bptr))               stop 17

         if(associated(cobj1%bcomp%acomp%aptr1))        stop 18
         if(associated(cobj1%bcomp%acomp%aptr2))        stop 19
      class default
         stop 13
   end select

   ! invoke assignA
   aobj1=A(4)(["xlf","XLF","ibm","IBM"],[1,2,3],assignChar,assignInt)

   if(any(aobj1%c1 /= ["flx","FLX","mbi","MBI"]))       stop 20
   if(any(aobj1%i1 /= [-1,-2,-3]))                      stop 21
   if(.not. associated(aobj1%aptr1,assignChar))         stop 22
   if(.not. associated(aobj1%aptr2,assignInt))          stop 23

   ! invoke assignB
   bobj1=B(3)(aobj1,assignA)

   if(any(bobj1%acomp%c1 /= ["xlf","XLF","ibm","IBM"])) stop 24
   if(any(bobj1%acomp%i1 /= [1,2,3]))                   stop 25
   if(.not. associated(bobj1%bptr,assignA))             stop 26
   if(.not. associated(bobj1%acomp%aptr1,assignChar))   stop 27
   if(.not. associated(bobj1%acomp%aptr2,assignInt))    stop 28

   select type(upoly)
      type is(C(*))
         upoly=C(4)(bobj1,assignB)  ! invoke assignC

         if(any(upoly%bcomp%acomp%c1 /= ["flx","FLX","mbi","MBI"]))   stop 29
         if(any(upoly%bcomp%acomp%i1 /= [-1,-2,-3]))                  stop 30
         if(.not. associated(upoly%cptr,assignB))                     stop 31
         if(.not. associated(upoly%bcomp%bptr,assignA))               stop 32
         if(.not. associated(upoly%bcomp%acomp%aptr1,assignChar))     stop 33
         if(.not. associated(upoly%bcomp%acomp%aptr2,assignInt))      stop 34
      class default
         stop 35
   end select

end program

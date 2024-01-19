!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 19 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. defect 362386
!234567490123456749012345674901234567490123456749012345674901234567490
module m
   type A(l1)
     integer,len :: l1
     character(4)  :: c1(4)="(**)"
     integer       :: i1(3:5) = -99
     procedure(assignChar),nopass,pointer :: aptr1=>null()
   end type

   type B(l2)
     integer,len :: l2
     type(A(4)),pointer :: acomp=>null()
   end type

   type C(l3)
     integer,len  :: l3
     type(B(3)) :: bcomp=B(3)()
   end type

    interface assignment(=)
       module procedure assignA,assignB,assignC
    end interface
    contains

      subroutine assignChar(char1,char2)
         character(*),intent(inout):: char1
         character(*),intent(in)   :: char2

         print *,"in assignChar"
      end subroutine

      subroutine assignA(this,ta)
          class(A(4)),intent(inout) :: this
          type(A(4)),intent(in)    :: ta

          print *,"in assignA"
          select type(this)
             type is(A(*))
                print *,associated(this%aptr1)
             class default
                stop 3
          end select
      end subroutine

     subroutine assignB(this,tb)
        class(B(3)),intent(inout) :: this
        type(B(3)),intent(in) :: tb

        print *,"in assignB"

        select type(this)
              type is(B(*))
                 allocate(A(4) :: this%acomp)
                 this%acomp=tb%acomp
                 print *,associated(this%acomp%aptr1)
              class default
                 stop 2
        end select
     end subroutine

     subroutine assignC(this,tc)
         class(C(4)),intent(inout) :: this
         type(C(4)),intent(in) :: tc

         print *,"in assignC"

         select type(this)
            type is(C(*))
               this%bcomp=tc%bcomp  !<== invoke assignB
                 print *,associated(this%bcomp%acomp%aptr1)
            class default
               stop 1
         end select

     end subroutine
end module

program d362386
   use m
   type(A(4)),target :: aobj1

   type(C(4)) :: cobj1

   cobj1=C(4)(B(3)(aobj1)) !<==invoke assignC
   print *,associated(cobj1%bcomp%acomp%aptr1)

end program


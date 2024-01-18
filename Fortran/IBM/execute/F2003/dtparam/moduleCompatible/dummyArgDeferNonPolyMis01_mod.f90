module m
   type dtp(l1,l2)
      integer,len      :: l1,l2
      character(l1+l2) :: c
   end type

   interface
     subroutine set(arg1,arg2)
        import
        type(dtp(:,:)),target,allocatable,intent(inout) :: arg1(:)
        type(dtp(*,*)),intent(in) :: arg2
     end subroutine
   end interface

   integer,parameter :: N=4

   contains

      subroutine outer
         type(dtp(:,:)),target,allocatable :: dtp1(:)
         type(dtp(:,:)),pointer            :: dtp2(:)=>null()

            allocate(dtp1(2:N),source=&
                   [dtp(1,2)("000"),dtp(1,2)("111"),dtp(1,2)("222")] )

            dtp2(4:)=>dtp1 ! dtp2 is poiner

            call inner(dtp1) ! actual argument dtp1 is target

            print *,dtp1
            print *,dtp2

            contains

               subroutine inner(arg)
                   ! dummy argument is target
                   type(dtp(:,:)),target,allocatable :: arg(:)

                   ! modify dtp1,dtp2 through arg
                   call set(arg,dtp(1,2)("123"))

                   ! modify dtp1 will effect value of dtp2
                   dtp1(3)=dtp(1,2)("456")

                   !modify dtp1,dtp2 through arg
                   arg(ubound(arg,1)) = dtp(1,2)("789")

               end subroutine

      end subroutine
end module

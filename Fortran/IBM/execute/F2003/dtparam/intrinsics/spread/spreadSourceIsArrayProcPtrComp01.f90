!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 20 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : SPREAD(SOURCE,DIM,NCOPIES)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.114
!*  2. SOURCE IS POLYMORPHIC ARRAY AND HAS PROCEDURE POINTER COMPONENTS
!234567890123456789012345678901234567890123456789012345678901234567890
module m

  interface
      integer function f1(int)
         integer,intent(in) :: int
      end function

      function f2(char)
         character(*),intent(in) :: char
         character(char%len) :: f2
      end function
  end interface

   type base(l1)
     integer,len     :: l1
     procedure(f1),nopass,pointer :: procptr1=>null()
   end type

   type,extends(base) :: child(k2)
     integer,kind     :: k2
     procedure(f2),nopass,pointer :: procptr2=>null()
   end type

   contains

       integer function fun1(int)
           integer,intent(in) :: int
           fun1=int
       end function

       function fun2(char)
           character(*),intent(in) :: char
           character(char%len) :: fun2
           fun2=char
       end function
end module

program spreadSourceIsArrayProcPtrComp01
  use m
  implicit none

  integer :: i
  class(base(:)),pointer :: b1(:)
  class(base(:)),pointer :: b2(:,:)

  allocate(b1(2:5),source=child(2,4)())

  select type(b1)
    type is(child(*,4))
        do i=lbound(b1,1),ubound(b1,1)
           b1(i)%procptr1=>fun1
           b1(i)%procptr2=>fun2
        end do
    class default
      error stop 100_4
  end select

  allocate(b2(2,2),source=reshape(b1,(/2,2/)) )

  call verify1(spread(b1,1,5)) ! dim is 1
  !   spread(..) becomes ...
  !   | b1(2), b1(3), b1(4), b1(5) |
  !   | b1(2), b1(3), b1(4), b1(5) |
  !   | b1(2), b1(3), b1(4), b1(5) |
  !   | b1(2), b1(3), b1(4), b1(5) |
  !   | b1(2), b1(3), b1(4), b1(5) |


  call verify2(spread(b1,2,5)) ! dim is 2
  !   spread(..) becomes ...
  !   | b1(2), b1(2), b1(2), b1(2), b1(2) |
  !   | b1(3), b1(3), b1(3), b1(3), b1(3) |
  !   | b1(4), b1(4), b1(4), b1(4), b1(4) |
  !   | b1(5), b1(5), b1(5), b1(5), b1(5) |


  !   b2(1,1) - (fun1(int),fun2(char))
  !   b2(1,2) - (fun1(int),fun2(char))
  !   b2(2,1) - (fun1(int),fun2(char))
  !   b2(2,2) - (fun1(int),fun2(char))

  call verify3(spread(b2,1,5)) ! dim is 1
  !   shape is 5,2,2
  !   dt(x,1,1) - (x is 1 - 5) - (fun1(int),fun2(char))
  !   dt(x,1,2) - (x is 1 - 5) - (fun1(int),fun2(char))
  !   dt(x,2,1) - (x is 1 - 5) - (fun1(int),fun2(char))
  !   dt(x,2,2) - (x is 1 - 5) - (fun1(int),fun2(char))

  !   | (fun1(int),fun2(char)), (fun1(int),fun2(char)) |   b2(1,1) , b2(1,2)
  !   | (fun1(int),fun2(char)), (fun1(int),fun2(char)) |   b2(2,1) , b2(2,2)

  call verify4(spread(b2,2,5)) ! dim is 2
  !   shape is 2 5 2
  !   dt(1,x,1) - (x is 1 - 5) - (fun1(int),fun2(char))
  !   dt(1,x,2) - (x is 1 - 5) - (fun1(int),fun2(char))
  !   dt(2,x,1) - (x is 1 - 5) - (fun1(int),fun2(char))
  !   dt(2,x,2) - (x is 1 - 5) - (fun1(int),fun2(char))

  contains

     subroutine verify1(dt)
        class(base(*)),intent(in) :: dt(:,:)

        ! element order:
        ! dt(1,1) - b1(2) - (fun1(int),fun2(char))
        ! dt(2,1) - b1(2)
        ! dt(3,1) - b1(2)
        ! dt(4,1) - b1(2)
        ! dt(5,1) - b1(2)
        ! dt(1,2) - b1(3) - (fun1(int),fun2(char)
        ! dt(2,2) - b1(3)
        ! dt(3,2) - b1(3)
        ! dt(4,2) - b1(3)
        ! dt(5,2) - b1(3)
        ! dt(1,3) - b1(4) - (fun1(int),fun2(char))
        ! dt(2,3) - b1(4)
        ! dt(3,3) - b1(4)
        ! dt(4,3) - b1(4)
        ! dt(5,3) - b1(4)
        ! dt(1,4) - b1(5) - (fun1(int),fun2(char))
        ! dt(2,4) - b1(5)
        ! dt(3,4) - b1(5)
        ! dt(4,4) - b1(5)
        ! dt(5,4) - b1(5)

        select type(dt)
           type is(child(*,4))
             if(dt%k2 /= 4)                              error stop 10_4
             if(dt%l1 /= 2)                              error stop 11_4
             if(size(dt,1) /= 5)                         error stop 12_4
             if(size(dt,2) /= 4)                         error stop 13_4
             do i=1,5

               if(dt(i,1)%procptr1(i) /= i)              error stop 14_4
               if(dt(i,1)%procptr2(char(i)) /= char(i))  error stop 15_4

               if(dt(i,2)%procptr1(i) /= i)              error stop 16_4
               if(dt(i,2)%procptr2(char(i)) /= char(i))  error stop 17_4

               if(dt(i,3)%procptr1(i) /= i)              error stop 18_4
               if(dt(i,3)%procptr2(char(i)) /= char(i))  error stop 19_4

               if(dt(i,4)%procptr1(i) /= i)              error stop 20_4
               if(dt(i,4)%procptr2(char(i)) /= char(i))  error stop 21_4

             end do
           class default
               error stop 101_4
        end select
     end subroutine

     subroutine verify2(dt)
        class(base(*)),intent(in) :: dt(:,:)

        ! element order
        ! dt(1,1) - b1(2) - (fun1(int),fun2(char))
        ! dt(2,1) - b1(3) - (fun1(int),fun2(char))
        ! dt(3,1) - b1(4) - (fun1(int),fun2(char))
        ! dt(4,1) - b1(5) - (fun1(int),fun2(char))
        ! dt(1,2) - b1(2)
        ! dt(2,2) - b1(3)
        ! dt(3,2) - b1(4)
        ! dt(4,2) - b1(5)
        ! dt(1,3) - b1(2)
        ! dt(2,3) - b1(3)
        ! dt(3,3) - b1(4)
        ! dt(4,3) - b1(5)
        ! dt(1,4) - b1(2)
        ! dt(2,4) - b1(3)
        ! dt(3,4) - b1(4)
        ! dt(4,4) - b1(5)
        ! dt(1,5) - b1(2)
        ! dt(2,5) - b1(3)
        ! dt(3,5) - b1(4)
        ! dt(4,5) - b1(5)

        select type(dt)
           type is(child(*,4))
             if(dt%k2 /= 4)                              error stop 22_4
             if(dt%l1 /= 2)                              error stop 23_4
             if(size(dt,1) /= 4)                         error stop 24_4
             if(size(dt,2) /= 5)                         error stop 25_4
             do i=1,5

               if(dt(1,i)%procptr1(i) /= i)              error stop 26_4
               if(dt(1,i)%procptr2(char(i)) /= char(i))  error stop 27_4

               if(dt(2,i)%procptr1(i) /= i)              error stop 28_4
               if(dt(2,i)%procptr2(char(i)) /= char(i))  error stop 29_4

               if(dt(3,i)%procptr1(i) /= i)              error stop 30_4
               if(dt(3,i)%procptr2(char(i)) /= char(i))  error stop 31_4

               if(dt(4,i)%procptr1(i) /= i)              error stop 32_4
               if(dt(4,i)%procptr2(char(i)) /= char(i))  error stop 33_4

             end do
            class default
               error stop 102_4
        end select

     end subroutine

    subroutine verify3(dt)
       class(base(*)),intent(in) :: dt(:,:,:)

       ! element order:
       ! dt(1,1,1) - b2(1,1) - (fun1(int),fun2(char))
       ! dt(2,1,1) - b2(1,1)
       ! dt(3,1,1) - b2(1,1)
       ! dt(4,1,1) - b2(1,1)
       ! dt(5,1,1) - b2(1,1)

       ! dt(1,2,1) - b2(2,1) - (fun1(int),fun2(char))
       ! dt(2,2,1) - b2(2,1)
       ! dt(3,2,1) - b2(2,1)
       ! dt(4,2,1) - b2(2,1)
       ! dt(5,2,1) - b2(2,1)

       ! dt(1,1,2) - b2(1,2) - (fun1(int),fun2(char))
       ! dt(2,1,2) - b2(1,2)
       ! dt(3,1,2) - b2(1,2)
       ! dt(4,1,2) - b2(1,2)
       ! dt(5,1,2) - b2(1,2)

       ! dt(1,2,2) - b2(2,2) - (fun1(int),fun2(char))
       ! dt(2,2,2) - b2(2,2)
       ! dt(3,2,2) - b2(2,2)
       ! dt(4,2,2) - b2(2,2)
       ! dt(5,2,2) - b2(2,2)

        select type(dt)
           type is(child(*,4))
             if(dt%k2 /= 4)                                error stop 34_4
             if(dt%l1 /= 2)                                error stop 35_4
             if(size(dt,1) /= 5)                           error stop 36_4
             if(size(dt,2) /= 2)                           error stop 37_4
             if(size(dt,3) /= 2)                           error stop 38_4
             do i=1,5
               if(dt(i,1,1)%procptr1(i) /= i)              error stop 39_4
               if(dt(i,1,1)%procptr2(char(i)) /= char(i))  error stop 40_4

               if(dt(i,2,1)%procptr1(i) /= i)              error stop 41_4
               if(dt(i,2,1)%procptr2(char(i)) /= char(i))  error stop 42_4

               if(dt(i,1,2)%procptr1(i) /= i)              error stop 43_4
               if(dt(i,1,2)%procptr2(char(i)) /= char(i))  error stop 44_4

               if(dt(i,2,2)%procptr1(i) /= i)              error stop 45_4
               if(dt(i,2,2)%procptr2(char(i)) /= char(i))  error stop 46_4

            end do
            class default
               error stop 103_4
        end select


    end subroutine

    subroutine verify4(dt)
       class(base(*)),intent(in) :: dt(:,:,:)

       ! element order:
       ! dt(1,1,1) - (fun1(int),fun2(char))
       ! dt(2,1,1) - (fun1(int),fun2(char))
       ! dt(1,2,1) -
       ! dt(2,2,1) -
       ! dt(1,3,1) -
       ! dt(2,3,1) -
       ! dt(1,4,1) -
       ! dt(2,4,1) -
       ! dt(1,5,1) -
       ! dt(2,5,1) -

       ! dt(1,1,2) - (fun1(int),fun2(char))
       ! dt(2,1,2) - (fun1(int),fun2(char))
       ! dt(1,2,2) -
       ! dt(2,2,2) -
       ! dt(1,3,2) -
       ! dt(2,3,2) -
       ! dt(1,4,2) -
       ! dt(2,4,2) -
       ! dt(1,5,2) -
       ! dt(2,5,2) -

        select type(dt)
           type is(child(*,4))
             if(dt%k2 /= 4)                                error stop 47_4
             if(dt%l1 /= 2)                                error stop 48_4
             if(size(dt,1) /= 2)                           error stop 49_4
             if(size(dt,2) /= 5)                           error stop 50_4
             if(size(dt,3) /= 2)                           error stop 51_4
             do i=1,5
               if(dt(1,i,1)%procptr1(i) /= i)              error stop 52_4
               if(dt(1,i,1)%procptr2(char(i)) /= char(i))  error stop 53_4

               if(dt(2,i,1)%procptr1(i) /= i)              error stop 54_4
               if(dt(2,i,1)%procptr2(char(i)) /= char(i))  error stop 55_4

               if(dt(1,i,2)%procptr1(i) /= i)              error stop 56_4
               if(dt(1,i,2)%procptr2(char(i)) /= char(i))  error stop 57_4

               if(dt(2,i,2)%procptr1(i) /= i)              error stop 58_4
               if(dt(2,i,2)%procptr2(char(i)) /= char(i))  error stop 59_4
            end do
           class default
                 error stop 104_4
        end select

    end subroutine
end program


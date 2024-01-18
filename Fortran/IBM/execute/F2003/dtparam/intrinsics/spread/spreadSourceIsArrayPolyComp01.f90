!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : spreadSourceIsArrayPolyComp01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Oct. 20 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : SPREAD(SOURCE,DIM,NCOPIES) 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.114
!*  2. SOURCE IS POLYMORPHIC ARRAY AND HAS POLYMORPHIC COMPONENT 
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
     integer,len     :: l1
     character(l1)   :: c1
   end type

   type,extends(base) :: child(k2)
     integer,kind     :: k2 
     integer(k2)      :: i1
     class(base(:)),pointer  :: base1=>null()
   end type
end module

program spreadSourceIsArrayPolyComp01
  use m
  implicit none

  integer :: i
  class(base(:)),target,allocatable :: b1(:)
  class(base(:)),target,allocatable :: b2(:,:)

  allocate(b1(2:5),source=[child(2,4)(c1="11",i1=11), &
                           child(2,4)(c1="22",i1=22), &
                           child(2,4)(c1="33",i1=33), &
                           child(2,4)(c1="44",i1=44)] )

  select type(b1)
    type is(child(*,4))
        do i=lbound(b1,1),ubound(b1,1)
            b1(i)%base1=>b1(i)
        end do
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


  !   b2(1,1) - (c1="11",i1=11)
  !   b2(1,2) - (c1="33",i1=33)
  !   b2(2,1) - (c1="22",i1=22)
  !   b2(2,2) - (c1="44",i1=44)
  call verify3(spread(b2,1,5)) ! dim is 1
  !   shape is 5,2,2  
  !   dt(x,1,1) - (x is 1 - 5) - (c1="11",i1=11)
  !   dt(x,1,2) - (x is 1 - 5) - (c1="33",i1=33)
  !   dt(x,2,1) - (x is 1 - 5) - (c1="22",i1=22)
  !   dt(x,2,2) - (x is 1 - 5) - (c1="44",i1=44)

  !   | (c1="11",i1=11), (c1="33",i1=33) |   b2(1,1) , b2(1,2) 
  !   | (c1="22",i1=22), (c1="44",i1=44) |   b2(2,1) , b2(2,2)

  call verify4(spread(b2,2,5)) ! dim is 2
  !   shape is 2 5 2
  !   dt(1,x,1) - (x is 1 - 5) - (c1="11",i1=11)
  !   dt(1,x,2) - (x is 1 - 5) - (c1="33",i1=33)
  !   dt(2,x,1) - (x is 1 - 5) - (c1="22",i1=22)
  !   dt(2,x,2) - (x is 1 - 5) - (c1="44",i1=44)

  contains

     subroutine verify1(dt)
        class(base(*)),intent(in) :: dt(:,:)

        ! element order: 
        ! dt(1,1) - b1(2) - (c1="11",i1=11)
        ! dt(2,1) - b1(2)
        ! dt(3,1) - b1(2)
        ! dt(4,1) - b1(2)
        ! dt(5,1) - b1(2)
        ! dt(1,2) - b1(3) - (c1="22",i1=22)
        ! dt(2,2) - b1(3)
        ! dt(3,2) - b1(3)
        ! dt(4,2) - b1(3)
        ! dt(5,2) - b1(3)
        ! dt(1,3) - b1(4) - (c1="33",i1=33)
        ! dt(2,3) - b1(4)
        ! dt(3,3) - b1(4)
        ! dt(4,3) - b1(4)
        ! dt(5,3) - b1(4)
        ! dt(1,4) - b1(5) - (c1="44",i1=44)
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
             if(dt%c1%len  /= 2)                         error stop 14_4
             if(dt%i1%kind /= 4)                         error stop 15_4
             do i=1,5
               if(dt(i,1)%c1 /= "11")                    error stop 16_4
               if(dt(i,1)%i1 /= 11)                      error stop 17_4
               if(dt(i,2)%c1 /= "22")                    error stop 18_4
               if(dt(i,2)%i1 /= 22)                      error stop 19_4

               if(dt(i,3)%c1 /= "33")                    error stop 20_4
               if(dt(i,3)%i1 /= 33)                      error stop 21_4
               if(dt(i,4)%c1 /= "44")                    error stop 22_4
               if(dt(i,4)%i1 /= 44)                      error stop 23_4

               if(.not. associated(dt(i,1)%base1))       error stop 24_4
               if(.not. associated(dt(i,2)%base1))       error stop 25_4
               if(.not. associated(dt(i,3)%base1))       error stop 26_4
               if(.not. associated(dt(i,4)%base1))       error stop 27_4 

             end do 
           class default
               error stop 100_4
        end select
     end subroutine   

     subroutine verify2(dt)
        class(base(*)),intent(in) :: dt(:,:)
        
        ! element order
        ! dt(1,1) - b1(2) - (c1="11",i1=11)
        ! dt(2,1) - b1(3) - (c1="22",i1=22)
        ! dt(3,1) - b1(4) - (c1="33",i1=33)
        ! dt(4,1) - b1(5) - (c1="44",i1=44)
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
             if(dt%k2 /= 4)                              error stop 28_4
             if(dt%l1 /= 2)                              error stop 29_4
             if(size(dt,1) /= 4)                         error stop 30_4
             if(size(dt,2) /= 5)                         error stop 31_4
             if(dt%c1%len  /= 2)                         error stop 32_4
             if(dt%i1%kind /= 4)                         error stop 33_4
             do i=1,5

               if(dt(1,i)%c1 /= "11")                    error stop 34_4
               if(dt(1,i)%i1 /= 11)                      error stop 35_4
               if(dt(2,i)%c1 /= "22")                    error stop 36_4
               if(dt(2,i)%i1 /= 22)                      error stop 37_4

               if(dt(3,i)%c1 /= "33")                    error stop 38_4
               if(dt(3,i)%i1 /= 33)                      error stop 39_4
               if(dt(4,i)%c1 /= "44")                    error stop 40_4
               if(dt(4,i)%i1 /= 44)                      error stop 41_4

               if(.not. associated(dt(1,i)%base1))       error stop 42_4
               if(.not. associated(dt(2,i)%base1))       error stop 43_4
               if(.not. associated(dt(3,i)%base1))       error stop 44_4
               if(.not. associated(dt(4,i)%base1))       error stop 45_4

             end do
            class default
              error stop 101_4
        end select

     end subroutine

    subroutine verify3(dt)
       class(base(*)),intent(in) :: dt(:,:,:)

       ! element order:
       ! dt(1,1,1) - b2(1,1) - (c1="11",i1=11) 
       ! dt(2,1,1) - b2(1,1) 
       ! dt(3,1,1) - b2(1,1)
       ! dt(4,1,1) - b2(1,1)
       ! dt(5,1,1) - b2(1,1)

       ! dt(1,2,1) - b2(2,1) - (c1="22",i1=22)
       ! dt(2,2,1) - b2(2,1)
       ! dt(3,2,1) - b2(2,1)
       ! dt(4,2,1) - b2(2,1)
       ! dt(5,2,1) - b2(2,1)

       ! dt(1,1,2) - b2(1,2) - (c1="33",i1=33)
       ! dt(2,1,2) - b2(1,2)
       ! dt(3,1,2) - b2(1,2)
       ! dt(4,1,2) - b2(1,2)
       ! dt(5,1,2) - b2(1,2)

       ! dt(1,2,2) - b2(2,2) - (c1="44",i1=44)
       ! dt(2,2,2) - b2(2,2)
       ! dt(3,2,2) - b2(2,2)
       ! dt(4,2,2) - b2(2,2)
       ! dt(5,2,2) - b2(2,2)

        select type(dt)
           type is(child(*,4))
             if(dt%k2 /= 4)                              error stop 46_4
             if(dt%l1 /= 2)                              error stop 47_4
             if(size(dt,1) /= 5)                         error stop 48_4
             if(size(dt,2) /= 2)                         error stop 49_4
             if(size(dt,3) /= 2)                         error stop 50_4
             if(dt%c1%len  /= 2)                         error stop 51_4
             if(dt%i1%kind /= 4)                         error stop 52_4
             do i=1,5
               if(dt(i,1,1)%c1 /= "11")                  error stop 53_4
               if(dt(i,1,1)%i1 /= 11)                    error stop 54_4
               if(dt(i,2,1)%c1 /= "22")                  error stop 55_4
               if(dt(i,2,1)%i1 /= 22)                    error stop 56_4

               if(dt(i,1,2)%c1 /= "33")                  error stop 57_4
               if(dt(i,1,2)%i1 /= 33)                    error stop 58_4
               if(dt(i,2,2)%c1 /= "44")                  error stop 59_4
               if(dt(i,2,2)%i1 /= 44)                    error stop 60_4

               if(.not. associated(dt(i,1,1)%base1))     error stop 61_4
               if(.not. associated(dt(i,2,1)%base1))     error stop 62_4
               if(.not. associated(dt(i,1,2)%base1))     error stop 63_4
               if(.not. associated(dt(i,2,2)%base1))     error stop 64_4

            end do
           class default
               error stop 102_4
        end select

    end subroutine

    subroutine verify4(dt)
       class(base(*)),intent(in) :: dt(:,:,:)
       
       ! element order:
       ! dt(1,1,1) - (c1="11",i1=11)
       ! dt(2,1,1) - (c1="22",i1=22)
       ! dt(1,2,1) - 
       ! dt(2,2,1) - 
       ! dt(1,3,1) -
       ! dt(2,3,1) - 
       ! dt(1,4,1) -
       ! dt(2,4,1) -
       ! dt(1,5,1) -
       ! dt(2,5,1) -

       ! dt(1,1,2) - (c1="33",i1=33)
       ! dt(2,1,2) - (c1="44",i1=44)
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
             if(dt%k2 /= 4)                              error stop 65_4
             if(dt%l1 /= 2)                              error stop 66_4
             if(size(dt,1) /= 2)                         error stop 67_4
             if(size(dt,2) /= 5)                         error stop 68_4
             if(size(dt,3) /= 2)                         error stop 69_4
             if(dt%c1%len  /= 2)                         error stop 70_4
             if(dt%i1%kind /= 4)                         error stop 71_4
             do i=1,5
               if(dt(1,i,1)%c1 /= "11")                  error stop 72_4
               if(dt(1,i,1)%i1 /= 11)                    error stop 73_4
               if(dt(2,i,1)%c1 /= "22")                  error stop 74_4
               if(dt(2,i,1)%i1 /= 22)                    error stop 75_4

               if(dt(1,i,2)%c1 /= "33")                  error stop 76_4
               if(dt(1,i,2)%i1 /= 33)                    error stop 77_4
               if(dt(2,i,2)%c1 /= "44")                  error stop 78_4
               if(dt(2,i,2)%i1 /= 44)                    error stop 79_4

               if(.not. associated(dt(1,i,1)%base1))     error stop 80_4
               if(.not. associated(dt(2,i,1)%base1))     error stop 81_4
               if(.not. associated(dt(1,i,2)%base1))     error stop 82_4
               if(.not. associated(dt(2,i,2)%base1))     error stop 83_4

            end do
            class default
               error stop 103_4
        end select

    end subroutine
end program


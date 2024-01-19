!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 13 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : INTRINSICS(MERGE)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 13.7.75
!* 2. INTRINSICS:MERGE(TSOURCE,FSOURCE,MASK)
!* 3. TSOURCE,FSOURCE ARE POLYMORPHIC
!* 4. DERIVED TYPE HAS EXTENDED TYPE
!* 5. COMPONENT ARE SCALAR INTEGER OR ARRAY
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(k1)
     integer(2),kind :: k1=4
     integer(k1)     :: i1=k1
  end type
  type,extends(base) :: child(k2,l)
     integer,kind    :: k2=8
     integer(2),len  :: l=3
     integer(k2)     :: i2(l)=k2
  end type
end module

program mergeIntComp03
   use m
   implicit none

   integer :: i

   class(base),allocatable :: poly1
   class(base),pointer     :: poly2
   class(*),allocatable    :: poly3
   class(*),pointer        :: poly4

   allocate(poly1,source=child())
   allocate(poly2,source=child(4,8,3)(i1=2,i2=5))
   allocate(poly3,source=child(2,1,4)(i1=9,i2=(/(i,i=1,4)/) ))
   allocate(poly4,source=child(2,1,4)(i1=-9,i2=(/(i,i=-4,-1)/) ))

   call check1(merge(poly1,poly2,.true.),1)
   call check1(merge(poly1,poly2,.false.),2)
   call check2(merge(poly3,poly4,.true.),1)
   call check2(merge(poly3,poly4,.false.),2)

   contains
     subroutine check1(dt,flag)
        class(base),intent(in) :: dt
        integer,intent(in)  :: flag

        select type(dt)
           type is(child(4,8,*))
                 if(dt%k1 /= 4)                         error stop 10_4
                 if(dt%k2 /= 8)                         error stop 11_4
                 if(dt%k1%kind /= 2)                    error stop 13_4
                 if(dt%k2%kind /= 4)                    error stop 14_4
              if(flag .eq. 1) then
                 if(dt%l /= 3)                          error stop 15_4
                 if(dt%l%kind /= 2)                     error stop 16_4
                 if(dt%i1 /= 4)                         error stop 17_4
                 if(size(dt%i2,1) /= 3)                 error stop 18_4
                 if(dt%i2%kind /= 8)                    error stop 19_4
                 if(any(dt%i2 /= 8))                    error stop 20_4
              else if(flag .eq. 2) then
                 if(dt%l /= 3)                          error stop 21_4
                 if(dt%l%kind /= 2)                     error stop 22_4
                 if(dt%i1 /= 2)                         error stop 23_4
                 if(size(dt%i2,1) /= 3)                 error stop 24_4
                 if(dt%i2%kind /= 8)                    error stop 25_4
                 if(any(dt%i2 /= 5))                    error stop 26_4
              end if
           class is(base(4))
               error stop 100_4
           class default
               error stop 101_4
        end select
     end subroutine
     subroutine check2(dt,flag)
        class(*),intent(in) :: dt
        integer,intent(in)  :: flag

        select type(dt)
           type is(child(2,1,*))
                 if(dt%k1 /= 2)                         error stop 27_4
                 if(dt%k2 /= 1)                         error stop 28_4
                 if(dt%k1%kind /= 2)                    error stop 29_4
                 if(dt%k2%kind /= 4)                    error stop 30_4
              if(flag .eq. 1) then
                 if(dt%l /= 4)                          error stop 31_4
                 if(dt%l%kind /= 2)                     error stop 32_4
                 if(dt%i1 /= 9)                         error stop 33_4
                 if(size(dt%i2,1) /= 4)                 error stop 34_4
                 if(dt%i2%kind /= 1)                    error stop 35_4
                 if(any(dt%i2 /= [1,2,3,4] ))           error stop 36_4
              else if(flag .eq. 2) then
                 if(dt%l /= 4)                          error stop 37_4
                 if(dt%l%kind /= 2)                     error stop 38_4
                 if(dt%i1 /= -9)                        error stop 39_4
                 if(size(dt%i2,1) /= 4)                 error stop 40_4
                 if(dt%i2%kind /= 1)                    error stop 41_4
                 if(any(dt%i2 /= [-4,-3,-2,-1]))        error stop 42_4
              end if
           class default
              error stop 102_4
        end select
     end subroutine

end program

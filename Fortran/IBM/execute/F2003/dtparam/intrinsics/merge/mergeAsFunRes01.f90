!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : mergeAsFunRes01.f
!*
!*  DATE                       : Sept. 19 2008
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
!* 3. TSOURCE AND FSOURCE ARE DERIVED TYPE ALLOCATABLE ARRAY
!* 4. DRIVED TYPE COMPONENT AND CHARACTER COMPONENT
!* 5. MERGE AS FUNCTION RESULT
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type A(k1,l1)
     integer,kind :: k1
     integer,len  :: l1

     character(:),allocatable :: c1(:)
  end type
  type B(l2)
     integer,len  :: l2

     type(A(2,:)),allocatable :: a1(:)
  end type
end module

program mergeAsFunRes01
   use m
   implicit none

   interface
      function getMergeResult2(Ts,Fs,Mask)
         import
         type(B(*)),intent(in) :: Ts(:),Fs(:)
         logical,intent(in) :: Mask(:)
         type(B(:)),allocatable :: getMergeResult2(:)

      end function
   end interface

   type(A(2,:)),allocatable :: typeA1(:),typeA2(:)
   type(B(:)),allocatable :: typeB1(:),typeB2(:),typeB3(:)

   allocate(typeA1(2),source=[A(2,3)(c1=["1234","5678"]), &
                               A(2,3)(c1=["00","11","22","33"])] )

   allocate(typeA2(2),source=[A(2,3)(c1=["abc","def","ghi"]), &
                              A(2,3)(c1=["8888","9999"])] )

   allocate(typeB1(2),source=B(3)(a1=typeA1))

   allocate(typeB2(2),source=B(3)(a1=typeA2))

   typeB3=getMergeResult1()

!   print *,typeB3(1)%a1%l1,typeB3(1)%a1%l1
   if(typeB3%l2 /= 3)                                   error stop 10_4
   if(typeB3(1)%a1%k1 /= 2)                             error stop 11_4
   if(typeB3(1)%a1%l1 /= 3)                             error stop 12_4
   if(typeB3(1)%a1(1)%c1%len /= 4)                      error stop 13_4
   if(typeB3(1)%a1(2)%c1%len /= 2)                      error stop 14_4
   if(size(typeB3(1)%a1(1)%c1,1) /= 2)                  error stop 15_4
   if(size(typeB3(1)%a1(2)%c1,1) /= 4)                  error stop 16_4
   if(any(typeB3(1)%a1(1)%c1 /= ["1234","5678"]))       error stop 17_4
   if(any(typeB3(1)%a1(2)%c1 /=  &
              ["00","11","22","33"]  ))                 error stop 18_4

!    print *,typeB3(2)%a1%l1,typeB3(2)%a1%l1
   if(typeB3%l2 /= 3)                                   error stop 19_4
   if(typeB3(2)%a1%k1 /= 2)                             error stop 20_4
   if(typeB3(2)%a1%l1 /= 3)                             error stop 21_4
   if(typeB3(2)%a1(1)%c1%len /= 3)                      error stop 22_4
   if(typeB3(2)%a1(2)%c1%len /= 4)                      error stop 23_4
   if(size(typeB3(2)%a1(1)%c1,1) /= 3)                  error stop 24_4
   if(size(typeB3(2)%a1(2)%c1,1) /= 2)                  error stop 25_4
   if(any(typeB3(2)%a1(1)%c1 /= ["abc","def","ghi"]))   error stop 26_4
   if(any(typeB3(2)%a1(2)%c1 /=  &
                    ["8888","9999"]  ))                 error stop 27_4


   typeB3=getMergeResult2(typeB1,typeB2,[.false.,.true.])

   if(typeB3%l2 /= 3)                                   error stop 40_4
   if(typeB3(1)%a1%k1 /= 2)                             error stop 41_4
   if(typeB3(1)%a1%l1 /= 3)                             error stop 42_4
   if(typeB3(1)%a1(1)%c1%len /= 3)                      error stop 43_4
   if(typeB3(1)%a1(2)%c1%len /= 4)                      error stop 44_4
   if(size(typeB3(1)%a1(1)%c1,1) /= 3)                  error stop 45_4
   if(size(typeB3(1)%a1(2)%c1,1) /= 2)                  error stop 46_4
   if(any(typeB3(1)%a1(1)%c1 /= ["abc","def","ghi"]))   error stop 47_4
   if(any(typeB3(1)%a1(2)%c1 /=  &
              ["8888","9999"]  ))                       error stop 48_4

   if(typeB3%l2 /= 3)                                   error stop 49_4
   if(typeB3(2)%a1%k1 /= 2)                             error stop 50_4
   if(typeB3(2)%a1%l1 /= 3)                             error stop 51_4
   if(typeB3(2)%a1(1)%c1%len /= 4)                      error stop 52_4
   if(typeB3(2)%a1(2)%c1%len /= 2)                      error stop 53_4
   if(size(typeB3(2)%a1(1)%c1,1) /= 2)                  error stop 54_4
   if(size(typeB3(2)%a1(2)%c1,1) /= 4)                  error stop 55_4
   if(any(typeB3(2)%a1(1)%c1 /= ["1234","5678"]))       error stop 56_4
   if(any(typeB3(2)%a1(2)%c1 /=  &
                    ["00","11","22","33"]  ))           error stop 57_4


   contains
      function getMergeResult1()
         type(B(:)),allocatable :: getMergeResult1(:)
         getMergeResult1=merge(typeB1,typeB2,[.true.,.false.])
      end function

end program

function getMergeResult2(Ts,Fs,Mask)
    use m
    type(B(*)),intent(in) :: Ts(:),Fs(:)
    logical,intent(in) :: Mask(:)
    type(B(:)),allocatable :: getMergeResult2(:)

        getMergeResult2=merge(Ts,Fs,Mask)
end function


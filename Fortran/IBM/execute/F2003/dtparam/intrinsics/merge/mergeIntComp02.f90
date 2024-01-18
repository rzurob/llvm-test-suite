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
!* 3. TSOURCE,FSOURCE ARE DERIVED TYPE SCALAR OR ARRAY
!* 4. COMPONENT ARE SCALAR INTEGER AND LOGICAL COMPONENT
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type A(k)
     integer(2),kind :: k=8

     integer(k) :: i=k
     logical(k) :: l=.false.
  end type
end module

program mergeIntComp02
   use m
   implicit none

   integer :: i=0
   logical :: mask1(6)=[.true.,.false.,.true.,.false.,.true.,.false.]
   logical :: mask2(2,3)

   type(A),target  :: a1
   type(A),pointer :: a2
   type(A),allocatable :: a3,b3
   type(A),allocatable :: a4(:)

   type(A(4)),target :: a5(6)=(/(A(4)(i),i=1,6)/)
   type(A(4)),target :: a6(2,3)
   type(A(4)),pointer :: a7(:)=>null()
   type(A(4)),allocatable :: a8(:,:)

   mask2=reshape(mask1,(/2,3/))

   a2=>a1
   a3=a1

   b3=getMyDT1(merge(a1,a3,.false.))

   if(b3%k /= 8)                                          error stop 10_4
   if(b3%k%kind /= 2)                                     error stop 11_4
   if(b3%i /= 8)                                          error stop 12_4
   if(b3%i%kind /= 8)                                     error stop 13_4
   if(b3%l .neqv. .false.)                                error stop 14_4
   if(b3%l%kind /= 8)                                     error stop 15_4

   if(merge(a1%i,a2%i,.false.) /= 8)                      error stop 16_4
   if(merge(a1%l,a3%l,.false.) .neqv. .false.)            error stop 17_4

   b3=getMyDT1(merge(a2,A(i=10,l=.true.),.false.))
   if(b3%i /= 10)                                         error stop 18_4
   if(b3%l .neqv. .true.)                                 error stop 19_4

   a4=getMyDT2(merge([A(),A(i=2,l=.true.)], &
             [A(i=3,l=.false.),A()],[.false.,.true.]))

   if(size(a4,1) /= 2)                                    error stop 20_4
   if(a4%k /= 8)                                          error stop 21_4
   if(a4%k%kind /= 2)                                     error stop 22_4
   if(any(a4%i /= [3,2]))                                 error stop 23_4
   if(a4%i%kind /= 8)                                     error stop 24_4
   if(any(a4%l .neqv. [.false.,.true.]))                  error stop 25_4
   if(a4%l%kind /= 8)                                     error stop 26_4

   if( any(merge([a4(1)%i,a4(2)%i], &
      [3_8,2_8],.true.) /= [3_8,2_8]))                    error stop 27_4

   if( any(merge([a4(1)%l,a4(2)%l],[.false._8,.true._8], &
      .true.) .neqv.[.false._8,.true._8]))               error stop 28_4

   a6=reshape(a5,(/2,3/))
   a7=>a5
   a8=reshape((/(A(4)(i+10,mask1(i)),i=1,6)/),(/2,3/))

   associate(x=>merge(a5,(/(A(4)(i+10,mask1(i)),i=1,6)/),mask1))
      if(size(x,1) /= 6)                                  error stop 29_4
      if(any(x%i /= [1,12,3,14,5,16]))                    error stop 30_4
      if(any(x%l .neqv. .false. ))                        error stop 31_4
      if(x%k /= 4)                                        error stop 32_4
      if(x%k%kind /= 2)                                   error stop 33_4
      if(x%l%kind /= 4)                                   error stop 34_4

   end associate

   associate(x=>merge(a7,(/(A(4)(i+10,mask1(i)),i=1,6)/),.not. mask1))
      if(x%k /= 4)                                        error stop 35_4
      if(x%k%kind /= 2)                                   error stop 36_4
      if(x%l%kind /= 4)                                   error stop 37_4
      if(size(x,1) /= 6)                                  error stop 38_4
      if(any(x%i /= [11,2,13,4,15,6]))                    error stop 39_4
      if(any(x%l .neqv. mask1 ))                          error stop 40_4
   end associate

   associate(x=>merge(a6,a8,mask2))
      if(size(x,1) /= 2)                                  error stop 41_4
      if(size(x,2) /= 3)                                  error stop 42_4
      if(any(x(:,1)%i /= [1,12] ))                        error stop 43_4
      if(any(x(:,2)%i /= [3,14] ))                        error stop 44_4
      if(any(x(:,3)%i /= [5,16] ))                        error stop 45_4
      if(x%k /= 4)                                        error stop 46_4
      if(any(x(:,:)%l .neqv. .false. ))                   error stop 47_4
      if(x%k%kind /= 2)                                   error stop 48_4
      if(x%i%kind /= 4)                                   error stop 49_4
      if(x%l%kind /= 4)                                   error stop 50_4
   end associate

   contains
      function getMyDT1(dt)
           type(A),intent(in) :: dt
           type(A)    :: getMyDT1

           getMyDT1=dt
      end function

      function getMyDT2(dt)
           type(A),intent(in) :: dt(:)
           type(A)    :: getMyDT2(size(dt))

           getMyDT2=dt
      end function

end program

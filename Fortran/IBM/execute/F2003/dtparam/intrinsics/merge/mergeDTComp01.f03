!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 9 2008
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
!* 4. DERIVED TYPE COMPONENT USE OUTER DERIVED TYPE PARAMETER AS ITS TYPE PARAMETER
!* 5. MERGE AS ACTUAL ARGUMENT
!* 6. DEFECT 356132
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type B(k2,l2)
     integer(8),kind :: k2=4
     integer(2),len  :: l2=5
  end type

  type A(k1,l1)
     integer,kind :: k1=2
     integer,len  :: l1=3
     type(B(k1,l1))     :: b1
     type(B(2*k1,2*l1)) :: b2
  end type
end module

program mergeDTComp01
   use m
   implicit none

   type(A),target           :: a1
   type(A(2,:)),pointer     :: a2
   type(A(2,:)),allocatable :: a3

   type(A(1,10)),target     :: a4(2)
   type(A(1,:)),allocatable :: a5(:)
   type(A(1,:)),pointer     :: a6(:)


   a2=>a1
   a3=a1

   allocate(a5(2),source=A(1,10)(b1=B(1,10)(),b2=B(2,20)()))
   allocate(a6(2),source=a4)

   call check1(merge(a1,a1,.true.))
   call check1(merge(a1,a2,.false.))
   call check1(merge(a3,A(2,3)(b1=B(2,3)(),b2=B(4,6)()),.true.))

   call check2(merge(a4,a5,[.false.,.true.]))
   call check2(merge([a4(1),A(1,10)(b1=B(1,10)(), &
         b2=B(2,20)())],a6,[.false.,.true.] ))

   contains
     subroutine check1(dt)
         type(A(2,*)) :: dt
!          print *,dt%l1,dt%b1%l2,dt%b2%l2
          if(dt%k1 /= 2)                                     error stop 10_4
          if(dt%l1 /= 3)                                     error stop 11_4
          if(dt%b1%k2 /= 2)                                  error stop 12_4
          if(dt%b1%l2 /= 3)                                  error stop 13_4
          if(dt%b2%k2 /= 4)                                  error stop 14_4
          if(dt%b2%l2 /= 6)                                  error stop 15_4

          if(dt%b1%k2%kind /= kind(dt%b1%k2)  &
               .or. dt%b1%k2%kind /= 8)                      error stop 16_4
          if(dt%b2%k2%kind /= kind(dt%b2%k2)  &
               .or. dt%b2%k2%kind /= 8)                      error stop 17_4

          if(dt%b1%l2%kind /= kind(dt%b1%l2)  &
               .or. dt%b1%l2%kind /= 2)                      error stop 18_4
          if(dt%b2%l2%kind /= kind(dt%b2%l2)  &
               .or. dt%b2%l2%kind /= 2)                      error stop 19_4

     end subroutine

     subroutine check2(dt)
         type(A(1,*)) :: dt(:)
!          print *,dt%b1%k2,dt%b1%l2
!          print *,dt%b2%k2,dt%b2%l2
          if(size(dt,1) /= 2)                                error stop 20_4
          if(dt%k1 /= 1)                                     error stop 21_4
          if(dt%l1 /= 10)                                    error stop 22_4
          if(dt%b1%k2 /= 1)                                  error stop 23_4
          if(dt%b1%l2 /= 10)                                 error stop 24_4
          if(dt%b2%k2 /= 2)                                  error stop 25_4
          if(dt%b2%l2 /= 20)                                 error stop 26_4

          if(dt%b1%k2%kind /= kind(dt%b1%k2)  &
               .or. dt%b1%k2%kind /= 8)                      error stop 27_4
          if(dt%b2%k2%kind /= kind(dt%b2%k2)  &
               .or. dt%b2%k2%kind /= 8)                      error stop 28_4

          if(dt%b1%l2%kind /= kind(dt%b1%l2)  &
               .or. dt%b1%l2%kind /= 2)                      error stop 29_4
          if(dt%b2%l2%kind /= kind(dt%b2%l2)  &
               .or. dt%b2%l2%kind /= 2)                      error stop 30_4

     end subroutine

end program

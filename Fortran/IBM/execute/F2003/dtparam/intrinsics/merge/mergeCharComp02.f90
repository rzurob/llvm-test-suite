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
!* 3. TSOURCE,FSOURCE ARE DERIVED TYPE ARRAY OR SCALAR
!* 4. DERIVED TYPE HAS SCALAR CHARACTER COMPONENT
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type A(l)
     integer,len  :: l=3
     character(l) :: c1="xlf"
  end type
end module

program mergeCharComp02
   use m
   implicit none

   logical,parameter :: mask(2)=[.false.,.true.]
   type(A) :: a1
   type(A) :: a2=A()("aaa")
   type(A),parameter :: a3(2)=[A(3)("ab"),A(3)("cd")]
   type(A) :: a4(5:6)=[A(3)("ef"),A(3)("gh")]

   type(A(:)),allocatable :: a5(:)
   type(A(:)),allocatable :: a6
   type(A(:)),pointer :: a7=>null()
   type(A(:)),pointer :: a8(:)=>null()

   character(:),allocatable :: c1(:)

   a2=merge(a1,a2,.false.)
   if(a2%l /= 3)                                    error stop 10_4
   if(a2%c1 /= "aaa")                               error stop 11_4

   a5=merge(a4,a3,[.true.,.false.])
   if(.not. allocated(a5))                          error stop 12_4
   if(size(a5) /= 2)                                error stop 13_4
   if(a5%l /= 3)                                    error stop 14_4
   if(any(a5%c1 /= ['ef','cd']))                    error stop 15_4
   if(a5%c1%len /= 3)                               error stop 16_4
   if(size(a5,1,4) /= 2)                            error stop 17_4

   if(allocated(a5)) deallocate(a5)
   a5=merge(a3,a4,.true. .or. .false.)

   if(.not. allocated(a5))                          error stop 18_4
   if(size(a5) /= 2)                                error stop 19_4
   if(a5%l /= 3)                                    error stop 20_4
   if(any(a5%c1 /= ['ab','cd']))                    error stop 21_4
   if(a5%c1%len /= 3)                               error stop 22_4
   if(size(a5,1,4) /= 2)                            error stop 23_4

   a6=merge(a3(1),a4(6),.false.)
   if(.not. allocated(a6))                          error stop 24_4
   if(a6%l /= 3)                                    error stop 25_4
   if(a6%c1 /= "gh")                                error stop 26_4
   if(a6%c1%len /= 3)                               error stop 27_4

   a6=merge(a3(1),a4(6),.true. .and. .true._4)
   if(.not. allocated(a6))                          error stop 28_4
   if(a6%l /= 3)                                    error stop 29_4
   if(a6%c1 /= "ab")                                error stop 30_4
   if(a6%c1%len /= 3)                               error stop 31_4

   if(allocated(a5)) deallocate(a5)
   a5=merge(a3,a4(6),mask(2))
   if(.not. allocated(a5))                          error stop 32_4
   if(a5%l /= 3)                                    error stop 33_4
   if(size(a5,1) /= 2)                              error stop 34_4
   if(any(a5%c1 /= ["ab","cd"]))                    error stop 35_4
   if(a5%c1%len /= 3)                               error stop 36_4

   allocate(a7,source=merge(A()(),A()(),.true.) )

   if(a7%l /= 3)                                    error stop 37_4
   if(a7%c1 /= "xlf")                               error stop 38_4
   if(a7%c1%len /= 3)                               error stop 39_4

   deallocate(a7)
   allocate(a7,source=merge(A(7)("hello"),A(int(7.0))("xlftest"),.false.))

   if(a7%l /= 7)                                    error stop 40_4
   if(a7%c1 /= "xlftest")                           error stop 41_4
   if(a7%c1%len /= 7)                               error stop 42_4

   deallocate(a7)
   allocate(a7,source=merge(A(7)("hello"(1:4)),A(int(7.0))("xlftest"),.true.))

   if(a7%l /= 7)                                    error stop 43_4
   if(a7%c1 /= "hell")                              error stop 44_4
   if(a7%c1%len /= 7)                               error stop 45_4

   deallocate(a7)
   allocate(a7,source=merge(a3(2),a4(5),mask(1)))

   if(a7%l /= 3)                                    error stop 46_4
   if(a7%c1 /= "ef")                                error stop 47_4
   if(a7%c1%len /= 3)                               error stop 48_4

   allocate(a8(3),source=merge([A(4)("abc"),A(4)("def"),A(4)("ghi")], &
                            [A(4)("aaaa"),A(4)("bbbb"),A(4)("cccc")], &
                               [.true.,.false.,.true.] ) )

   if(a8%l /= 4)                                    error stop 49_4
   if(any(a8%c1 /= ["abc ","bbbb","ghi "]))         error stop 50_4

   deallocate(a8)
   allocate(a8(2),source=merge(a3,a4,mask))

   if(a8%l /= 3)                                    error stop 51_4
   if(any(a8%c1 /= ["ef","cd"]))                    error stop 52_4


   deallocate(a8)
   allocate(a8(3),source=merge([a3,a3(2)], &
                         [A(3)("abc"),a4], &
                         [.false.,.true.,.false.]))

   if(a8%l /= 3)                                    error stop 53_4
   if(any(a8%c1 /= ["abc","cd ","gh "]))            error stop 54_4

   c1=merge(a3%c1,a4%c1,[.true.,.false.])
   if(any(c1 /= ['ab','gh']))                       error stop 55_4


end program

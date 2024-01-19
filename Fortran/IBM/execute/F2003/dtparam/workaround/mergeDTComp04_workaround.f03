!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 15 2008
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
!* 3. TSOURCE,FSOURCE ARE DERIVED TYPE ARRAY OR COMPONENT
!* 4. DERIVED TYPE HAS ALLOCATABLE DT COMPONENT
!* 5. ! USE ASSOCIATE
!* 6. DEFECT 356178 356169
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A(l1)
       integer, len  :: l1
       character(l1) :: ca
   end type
   type B(l2)
       integer,len :: l2
       type(A(:)),allocatable :: type1
   end type
end module

program mergeDTComp04
   use m
   implicit none

   type(B(4)) :: b1(2),b2(2)

   type(B(:)),allocatable :: b3(:),b4(:)

   allocate(b3(2),source=[B(3)(type1=A(7)(ca="123")),&
                          B(3)(type1=A(7)(ca="456"))] )

   allocate(b4(2),source=[B(3)(type1=null()),&
                          B(3)(type1=null())] )


   allocate(b4(1)%type1,source=A(7)(ca="abc"))
   allocate(b4(2)%type1,source=A(7)(ca="def"))


   b1=[B(4)(type1=A(b1%l2)(ca="abc")),B(4)(A(b1%l2)(ca="def"))]
   b2=[B(4)(type1=A(4)(ca="000")),B(4)(A(4)(ca="111"))]

   call associate1(merge(b1,b2,[.true.,.false.]))
   call associate2(merge(b1,b2,[.false.,.true.]))

   call associate3(merge([b1(1)%type1,b1(2)%type1], [b2(1)%type1,b2(2)%type1],[.true.,.false.]))
   call associate4(merge([b1(1)%type1,b1(2)%type1], [b2(1)%type1,b2(2)%type1],[.false.,.true.]))
   call associate5(merge([b1(1)%type1%ca,b1(2)%type1%ca], [b2(1)%type1%ca,b2(2)%type1%ca],[.false.,.true.]))
   call associate6(merge([b1(1)%type1%ca,b1(2)%type1%ca], [b2(1)%type1%ca,b2(2)%type1%ca],[.true.,.false.]))
   call associate7(merge(b3,b4,[.true.,.false.]))
   call associate8(merge(b3,b4,[.false.,.true.]))
   call associate9(merge([b3(1)%type1,b3(2)%type1], [b4(1)%type1,b4(2)%type1],[.true.,.false.]))
   call associate10(merge([b3(1)%type1,b3(2)%type1], [b4(1)%type1,b4(2)%type1],[.false.,.true.]))
   call associate11(merge([b3(1)%type1%ca,b3(2)%type1%ca], [b4(1)%type1%ca,b4(2)%type1%ca],[.false.,.true.]))
   call associate12(merge([b3(1)%type1%ca,b3(2)%type1%ca], [b4(1)%type1%ca,b4(2)%type1%ca],[.true.,.false.]))

   contains

!   associate(x=>merge(b1,b2,[.true.,.false.]))
   subroutine associate1(x)
    type (B(4)), intent(in) :: X(2)
      if(x%l2 /= 4)                                     error stop 10_4
      if(x(1)%type1%l1 /= 4)                            error stop 11_4
      if(x(2)%type1%l1 /= 4)                            error stop 12_4
      if(x(1)%type1%ca /= "abc")                        error stop 13_4
      if(x(2)%type1%ca /= "111")                        error stop 14_4
   end subroutine

!   associate(x=>merge(b1,b2,[.false.,.true.]))
   subroutine associate2(x)
    type (B(*)), intent(in) :: X(2)
      if(x%l2 /= 4)                                     error stop 15_4
      if(x(1)%type1%l1 /= 4)                            error stop 16_4
      if(x(2)%type1%l1 /= 4)                            error stop 17_4
      if(x(1)%type1%ca /= "000")                        error stop 18_4
      if(x(2)%type1%ca /= "def")                        error stop 19_4
   end subroutine

!   associate(x=>merge([b1(1)%type1,b1(2)%type1], [b2(1)%type1,b2(2)%type1],[.true.,.false.]))
   subroutine associate3(x)
    type(A(*)), intent(in) :: x(2)
      if(x%l1 /= 4)                                     error stop 20_4
      if(any(x%ca /= ["abc","111"]))                    error stop 21_4
   end subroutine

!   associate(x=>merge([b1(1)%type1,b1(2)%type1], [b2(1)%type1,b2(2)%type1],[.false.,.true.]))
   subroutine associate4(x)
    type(A(*)), intent(in) :: x(:)
      if(x%l1 /= 4)                                     error stop 22_4
      if(any(x%ca /= ["000","def"]))                    error stop 23_4
   end subroutine

!   associate(x=>merge([b1(1)%type1%ca,b1(2)%type1%ca], [b2(1)%type1%ca,b2(2)%type1%ca],[.false.,.true.]))
   subroutine associate5(x)
    character(*), intent(in) :: x(2)
      if(len(x) /= x%len .or. x%len /= 4)               error stop 24_4
      if(any(x /= ["000","def"]))                       error stop 25_4
   end subroutine

!   associate(x=>merge([b1(1)%type1%ca,b1(2)%type1%ca], [b2(1)%type1%ca,b2(2)%type1%ca],[.true.,.false.]))
   subroutine associate6(x)
    character(*), intent(in) :: x(:)
      if(len(x) /= x%len .or. x%len /= 4)               error stop 26_4
      if(any(x /= ["abc","111"]))                       error stop 27_4
   end subroutine

!    associate(x=>merge(b3,b4,[.true.,.false.]))
   subroutine associate7(x)
     type (B(*)), intent(in) :: x(2)
       if(x%l2 /= 3)                                     error stop 28_4
       if(x(1)%type1%l1 /= 7)                            error stop 29_4
       if(x(2)%type1%l1 /= 7)                            error stop 30_4
       if(x(1)%type1%ca /= "123")                        error stop 31_4
       if(x(2)%type1%ca /= "def")                        error stop 32_4
    end subroutine

!    associate(x=>merge(b3,b4,[.false.,.true.]))
    subroutine associate8(x)
     type (B(*)), intent(in) :: x(:)
       if(x%l2 /= 3)                                     error stop 33_4
       if(x(1)%type1%l1 /= 7)                            error stop 34_4
       if(x(2)%type1%l1 /= 7)                            error stop 35_4
       if(x(1)%type1%ca /= "abc")                        error stop 36_4
       if(x(2)%type1%ca /= "456")                        error stop 37_4
    end subroutine

!   associate(x=>merge([b3(1)%type1,b3(2)%type1], [b4(1)%type1,b4(2)%type1],[.true.,.false.]))
   subroutine associate9 (x)
    type(A(*)), intent(in) :: x(2)
      if(x%l1 /= 7)                                     error stop 38_4
      if(any(x%ca /= ["123","def"]))                    error stop 39_4
   end subroutine

!   associate(x=>merge([b3(1)%type1,b3(2)%type1], [b4(1)%type1,b4(2)%type1],[.false.,.true.]))
   subroutine associate10(x)
    type(A(*)), intent(in) :: x(:)
      if(x%l1 /= 7)                                     error stop 40_4
      if(any(x%ca /= ["abc","456"]))                    error stop 41_4
   end subroutine

!   associate(x=>merge([b3(1)%type1%ca,b3(2)%type1%ca], [b4(1)%type1%ca,b4(2)%type1%ca],[.false.,.true.]))
   subroutine associate11(x)
    character(*), intent(in) :: x(:)
      if(len(x) /= x%len .or. x%len /= 7)               error stop 42_4
      if(any(x /= ["abc","456"]))                       error stop 43_4
   end subroutine

!   associate(x=>merge([b3(1)%type1%ca,b3(2)%type1%ca], [b4(1)%type1%ca,b4(2)%type1%ca],[.true.,.false.]))
   subroutine associate12(x)
    character(*), intent(in) :: x(2)
      if(len(x) /= x%len .or. x%len /= 7)               error stop 44_4
      if(any(x /= ["123","def"]))                       error stop 45_4
   end subroutine
end program

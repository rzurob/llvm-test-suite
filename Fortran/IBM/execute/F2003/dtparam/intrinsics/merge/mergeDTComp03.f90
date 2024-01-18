!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : mergeDTComp03.f
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
!* 3. TSOURCE,FSOURCE ARE DERIVED TYPE SCALAR OR COMPONENT
!* 4. DERIVED TYPE HAS ALLOCATABLE OR POINTER DT COMPONENT
!* 5. USE ASSOCIATE
!* 6. DEFECT 356159 356173
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A(l1)
       integer, len  :: l1
       character(l1) :: ca(l1)
   end type
   type B(l2)
       integer,len   :: l2
       character(l2) :: cb(l2)
       type(A(:)),allocatable :: type1
       type(A(:)),pointer     :: type2=>null()
   end type
end module

program mergeDTComp03
   use m
   implicit none

   type(A(4)),target :: a1=A(4)(ca="123")
   type(B(3)),target :: b1=B(3)(cb="xlf",type1=null())
   type(B(3)),target :: b2=B(3)(cb="xlc",type1=null())

   character(:), allocatable :: str(:)

   b1%type1=a1
   allocate(b1%type2,source=A(4)(ca="456"))
   b2%type1=A(4)(ca="000")
   allocate(b2%type2,source=A(4)(ca="111"))

   allocate (character(4) :: str(size(b1%cb)))

   str(:) = b1%cb

   call associate10_13(merge(b1%type1,b1%type2,.true.))

   call associate14_17(x=merge(b1%type1,b1%type2,.false.))

   call associate18_19(merge(b1%type1%ca,str,.true.))

   call associate20_21(x=merge(b1%type1%ca,str,.false.))

   call associate22_33(merge(b1,b2,.true.))

   call associate34_45(merge(b1,b2,.false.))

   contains

!   associate(x=>merge(b1%type1,b1%type2,.true.))
   subroutine associate10_13(x)
      type(A(*)), intent(in) :: x

      if(x%l1 /=  4)                                      error stop 10_4
      if(x%ca%len /= 4)                                   error stop 11_4
      if(size(x%ca) /= 4)                                 error stop 12_4
      if(any(x%ca /= "123"))                              error stop 13_4
   end subroutine

!   associate(x=>merge(b1%type1,b1%type2,.true.))
   subroutine associate14_17(x)
      type(a(*)), intent(in) :: x

      if(x%l1 /=  4)                                      error stop 14_4
      if(x%ca%len /= 4)                                   error stop 15_4
      if(size(x%ca) /= 4)                                 error stop 16_4
      if(any(x%ca /= "456"))                              error stop 17_4
   end subroutine

!   associate(x=>merge(b1%type1%ca,str,.true.))
   subroutine associate18_19(x)
     character(*), intent(in) :: x(:)

     if(x%len /= len(x) .or. x%len /= 4)                  error stop 18_4
     if(any(x /= "123"))                                  error stop 19_4
   end subroutine

!   associate(x=>merge(b1%type1%ca,str,.false.))
   subroutine associate20_21(x)
      character(*), intent(in) :: x(:)

      if(any(x /= "xlf"))                                 error stop 20_4
      if(x%len /= 4)                                      error stop 21_4
   end subroutine

!   associate(x=>merge(b1,b2,.true.))
   subroutine associate22_33(x)
      type(b(*)), intent(in) :: x

      if(x%l2 /= 3)                                       error stop 22_4
      if(any(x%cb /= "xlf"))                              error stop 23_4
      if(x%cb%len /= len(x%cb) .or. x%cb%len /= 3)        error stop 24_4
      if(size(x%cb) /= 3)                                 error stop 25_4
      if(x%type1%l1 /= 4)                                 error stop 26_4
      if(any(x%type1%ca /= "123"))                        error stop 27_4
      if(len(x%type1%ca) /= len(x%type1%ca) .or. &
           len(x%type1%ca) /= 4)                          error stop 28_4
      if(size(x%type1%ca,1) /= 4)                         error stop 29_4

      if(x%type2%l1 /= 4)                                 error stop 30_4
      if(any(x%type2%ca /= "456"))                        error stop 31_4
      if(len(x%type1%ca) /= len(x%type1%ca) .or. &
           len(x%type1%ca) /= 4)                          error stop 32_4
      if(size(x%type1%ca,1) /= 4)                         error stop 33_4
   end subroutine

!   associate(x=>merge(b1,b2,.false.))
   subroutine associate34_45(x)
      type(b(*)), intent(in) :: x

      if(x%l2 /= 3)                                       error stop 34_4
      if(any(x%cb /= "xlc"))                              error stop 35_4
      if(x%cb%len /= len(x%cb) .or. x%cb%len /= 3)        error stop 36_4
      if(size(x%cb) /= 3)                                 error stop 37_4
      if(x%type1%l1 /= 4)                                 error stop 38_4
      if(any(x%type1%ca /= "000"))                        error stop 39_4
      if(len(x%type1%ca) /= len(x%type1%ca) .or. &
           len(x%type1%ca) /= 4)                          error stop 40_4
      if(size(x%type1%ca,1) /= 4)                         error stop 41_4

      if(x%type2%l1 /= 4)                                 error stop 42_4
      if(any(x%type2%ca /= "111"))                        error stop 43_4
      if(len(x%type1%ca) /= len(x%type1%ca) .or. &
           len(x%type1%ca) /= 4)                          error stop 44_4
      if(size(x%type1%ca,1) /= 4)                         error stop 45_4
   end subroutine

end program

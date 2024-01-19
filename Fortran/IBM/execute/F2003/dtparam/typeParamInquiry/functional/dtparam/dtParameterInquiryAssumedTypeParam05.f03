!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 15 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3
!* 2. TYPE PARAMETER INQUIRY
!* 3. USE POLYMORPHIC,INHERITANCE,SELECT TYPE
!* 4. DEFECT 355097,355108,354013
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k,l)
      integer, kind  :: k
      integer(8),len :: l

      character(l)  :: c1(k:l)
   end type
   type,extends(base) :: child(m)
      integer(2),len     :: m
      character(m)    :: c2(k:m)
   end type

   contains

   function checkresult(b)
     class(base(2,*)),intent(in) :: b
     class(base(2,:)),allocatable :: checkresult
     integer :: i=0
     select type(b)
       type is(base(2,*))
         if(b%k /= 2)                                        error stop 10_4
         if(b%k%kind /= kind(b%k) .or. b%k%kind /= 4)        error stop 11_4
         if(b%l /= 6)                                        error stop 12_4
         if(b%l%kind /= kind(b%l) .or. b%l%kind /= 8)        error stop 13_4
         !--- defect 354013---!
         if(b%c1%len /= len(b%c1) .or. b%c1%len /= 6)        error stop 14_4
         if(lbound(b%c1,1) /= 2)                             error stop 15_4
         if(ubound(b%c1,1) /= 6)                             error stop 16_4

         if(any(b%c1  /='xlftes'))                           error stop 17_4

         allocate(checkresult,source=b)

       type is(child(2,*,*))
         if(b%k /= 2)                                        error stop 18_4
         if(b%k%kind /= kind(b%k) .or. b%k%kind /= 4)        error stop 19_4
         if(b%l /= 8)                                        error stop 20_4
         if(b%l%kind /= kind(b%l) .or. b%l%kind /= 8)        error stop 21_4
         if(b%m /= 12)                                       error stop 22_4
         if(b%m%kind /= kind(b%m) .or. b%m%kind /= 2)        error stop 23_4
         !--- defect 354013---!
         if(b%c1%len /= len(b%c1) .or. b%c1%len /= 8)        error stop 24_4
         if(b%c2%len /= len(b%c2) .or. b%c2%len /= 12)       error stop 25_4

         if(lbound(b%c1,1) /= 2)                             error stop 26_4
         if(ubound(b%c1,1) /= 8)                             error stop 27_4
         if(lbound(b%c2,1) /= 2)                             error stop 28_4
         if(ubound(b%c2,1) /= 12)                            error stop 29_4

         if(any(b%c1  /="fortran"))                          error stop 30_4
         if(any(b%c2  /="xlftest"))                          error stop 31_4

         allocate(checkresult,source=b)

       class is(base(2,*))
            error stop 32_4

       class default
            error stop 33_4
     end select
   end function
end module

  program dtParameterInquiryAssumedTypeParam05
  use m
  implicit none

  integer :: i=0
  class(base(k=2,l=:)),allocatable :: b1
  class(base(k=2,l=:)),allocatable     :: b2

  allocate(b1,source=base(k=2,l=6)(c1="xlftest"))
  allocate(b2,source=checkresult(b1))
select type(b2)

  type is(base(2,*))

  if(b2%k /= 2)                                            error stop 40_4
  if(b2%k%kind /= kind(b2%k) .or. b2%k%kind /= 4)          error stop 41_4
  if(b2%l /= 6)                                            error stop 42_4
  if(b2%l%kind /= kind(b2%l) .or. b2%l%kind /= 8)          error stop 43_4
  !--- defect 354013---!
  if(b2%c1%len /= len(b2%c1) .or. b2%c1%len /= 6)          error stop 44_4
  if(lbound(b2%c1,1) /= 2)                                 error stop 45_4
  if(ubound(b2%c1,1) /= 6)                                 error stop 46_4
  if(any(b2%c1  /='xlftes'))                               error stop 47_4
  class default
     error stop 100_4
end select

  deallocate(b1)
  deallocate(b2)

  allocate(b1,source=child(k=2,l=8,m=12)(c1="fortran",c2="xlftest"))
  allocate(b2,source=checkresult(b1))

select type(b2)
  type is(child(2,*,*))

  if(b2%k /= 2)                                            error stop 48_4
  if(b2%k%kind /= kind(b2%k) .or. b2%k%kind /= 4)          error stop 49_4
  if(b2%l /= 8)                                            error stop 50_4
  if(b2%l%kind /= kind(b2%l) .or. b2%l%kind /= 8)          error stop 51_4
  !--- defect 354013---!
  if(b2%c1%len /= len(b2%c1) .or. b2%c1%len /= 8)           error stop 52_4
  if(lbound(b2%c1,1) /= 2)                                  error stop 53_4
  if(ubound(b2%c1,1) /= 8)                                  error stop 54_4
  if(any(b2%c1  /='fortran'))                               error stop 55_4
  if(b2%m /= 12)                                            error stop 56_4
  if(b2%m%kind /= kind(b2%m) .or. b2%m%kind /= 2)           error stop 57_4
  if(any(b2%c2 /= "xlftest"))                               error stop 58_4
  if(lbound(b2%c2,1) /= 2)                                  error stop 59_4
  if(ubound(b2%c2,1) /= 12)                                 error stop 60_4

  class default
     error stop 101_4
end select

  deallocate(b1)
  deallocate(b2)

end

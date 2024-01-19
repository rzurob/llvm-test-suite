!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 13 2008
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
!* 2. TYPE PARAMETER INQUIRY FOR INTRINSIC TYPE
!* 3. DUMMY ARGUMENT HAS ASSUMED LENGTH AND HAS INTENT(INOUT) ATTRIBUTE
!* 4. DEFECT 352994,354606
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   character(:),pointer :: a
   character(:),allocatable :: b
   character(:),pointer :: c(:)
   character(:),allocatable :: d(:)
   character(len=7) :: e="xlftest"
   character(len=*),parameter :: f(3)=['abcd','efgh','ijkl']
end module
program typeParamInquiryIntrinsicAssum02
    use m
    implicit none

    allocate(a,source="xlf"//"test")
    b="xlf"//"test"

    call test1(a)

    if(a /= "Hello")                                        error stop 30_4
    if(a%len /= len(a) .or. a%len /= 7)                     error stop 31_4
    if(a%kind /= kind(a) .or. a%kind /=1)                   error stop 32_4

    call test1(b)

    if(b /= "Hello")                                        error stop 33_4
    if(b%len /= len(b) .or. b%len /= 7)                     error stop 34_4
    if(b%kind /= kind(b) .or. b%kind /=1)                   error stop 35_4

    call test1(e)

    if(e /= "Hello")                                        error stop 36_4
    if(e%len /= len(b) .or. e%len /= 7)                     error stop 37_4
    if(e%kind /= kind(e) .or. e%kind /=1)                   error stop 38_4


    deallocate(a)
    allocate(a,source="xlf"//"test")
    b="xlf"//"test"

    call test2(a(1:3))

    if(a /= "Heltest")                                      error stop 39_4
    if(a%len /= len(a) .or. a%len /= 7)                     error stop 40_4
    if(a%kind /= kind(a) .or. a%kind /=1)                   error stop 41_4

    call test2(b(1:3))
    if(b /= "Heltest")                                      error stop 42_4
    if(b%len /= len(b) .or. b%len /= 7)                     error stop 43_4
    if(b%kind /= kind(b) .or. b%kind /=1)                   error stop 44_4

    e="xlftest"
    call test2(e(1:3))
    if(e /= "Heltest")                                      error stop 45_4
    if(e%len /= len(b) .or. e%len /= 7)                     error stop 46_4
    if(e%kind /= kind(e) .or. e%kind /=1)                   error stop 47_4


    allocate(c(size(f)),source=f)
    d=c

    call test3(c)
    if(any(c /= ['seed','weed','wate']))                    error stop 48_4
    if(c%len /= len(c) .or. c%len /= 4)                     error stop 49_4
    if(c%kind /= kind(c) .or. c%kind /= 1)                  error stop 50_4

    call test3(d)
    if(any(d /= ['seed','weed','wate']))                    error stop 51_4
    if(d%len /= len(d) .or. d%len /= 4)                     error stop 52_4
    if(d%kind /= kind(d) .or. d%kind /= 1)                  error stop 53_4

    deallocate(c)
    allocate(c(size(f)),source=f)
    d=c

    call test4(c(1:2))
    if(any(c /= ['seed','weed','ijkl']))                    error stop 54_4
    if(c%len /= len(c) .or. c%len /= 4)                     error stop 55_4
    if(c%kind /= kind(c) .or. c%kind /= 1)                  error stop 56_4

     call test4(d(1:2))

    if(any(c /= ['seed','weed','ijkl']))                    error stop 57_4
    if(c%len /= len(c) .or. c%len /= 4)                     error stop 58_4
    if(d%kind /= kind(d) .or. d%kind /= 1)                  error stop 59_4


    contains

      subroutine test1(arg)
         character(*),intent(inout) :: arg

           if(arg%len /= len(arg) .or. arg%len /=7)          error stop 10_4
           if(arg%kind /= kind(arg) .or. arg%kind /=1)       error stop 11_4
           if(len(arg(1:3))    /=3)                          error stop 12_4
           if(arg /= "xlftest")                              error stop 13_4
         arg="Hello"
      end subroutine

      subroutine test2(arg)
         character(*),intent(inout) :: arg

           if(arg%len /= len(arg) .or. arg%len /=3)          error stop 14_4
           if(arg%kind /= kind(arg) .or. arg%kind /=1)       error stop 15_4
           if(len(arg(1:0))    /=0)                          error stop 16_4
           if(arg /= "xlf")                                  error stop 17_4

           arg="Hello World!"
      end subroutine

     subroutine test3(arg)
        character(*),intent(inout) :: arg(2:)

          if(arg%len /= len(arg) .or. arg%len /=4)           error stop 18_4
          if(arg%kind /= kind(arg) .or. arg%kind /=1)        error stop 19_4
          if(ubound(arg,1) /= 4)                             error stop 20_4
          if(any(arg /= ['abcd','efgh','ijkl']))             error stop 21_4
          arg=['seeds','weeds','water']
     end subroutine

     subroutine test4(arg)
        character(*),intent(inout) :: arg(:)

          if(arg%len /= len(arg) .or. arg%len /=4)           error stop 22_4
          if(arg%kind /= kind(arg) .or. arg%kind /=1)        error stop 23_4
          if(ubound(arg,1) /= 2)                             error stop 24_4
          if(any(arg /= ['abcd','efgh']))                    error stop 25_4
          arg=['seeds','weeds']

     end subroutine

end


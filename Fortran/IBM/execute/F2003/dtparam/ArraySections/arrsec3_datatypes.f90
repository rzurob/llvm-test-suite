!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : arrsec3_datatypes.f
!*                               arrsec1_datatype00-06.f)
!*  DATE                       : Dec 31, 2008
!*  ORIGIN                     : XLF Development
!*  PRIMARY FUNCTIONS TESTED   : Array sections
!*  DESCRIPTION
!*
!*  Array components of byte, character, complex, real, logical, integer types
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

    module m
      type ARRSC(n)
        integer, len :: n
        byte b
        character c
        complex x
        real r
        logical l
        integer i
      end type

      type A(n)
        integer, len :: n
        integer h(n)
        type(ARRSC(n)) :: i(n)
        integer j(n)
      end type
    end module

    use m
    integer, parameter :: NA=4
    real r
    type(A(:)), allocatable :: a1(:)
    allocate (A(4) :: a1(NA))

    a1%h(4) = [(i, i=1,NA)]
    a1%i(2)%b = [(i, i=1,NA)]
    a1%i(2)%c = 'o'
    a1%i(2)%r = [(r, r=1,NA)]
    a1%i(2)%l = [(.true., i=1,NA)]
    a1%i(2)%i = [(i, i=1,NA)]
    a1%i(2)%x = [((r,r*2), r=1,NA)]
    a1%j(1) = [(i, i=1,NA)]

    print *, a1(:)%h(4)
    print *, a1(:)%i(2)%b
    print *, a1(:)%i(2)%c
    print *, a1(:)%i(2)%r
    print *, a1(:)%i(2)%l
    print *, a1(:)%i(2)%i
    print *, a1(:)%i(2)%x
    print *, a1(:)%j(1)
    end
